use std::collections::HashMap;

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree as Tt};

use crate::{error::error, expand::metaval::ConcatenateError, literal::{LitKind, parse_int, parse_string}};

use metaval::Metaval;

mod metaval;

pub struct ExpandContext<'a> {
    parent: Option<&'a Self>,
    sigil: char,
    metavars: HashMap<String, Metaval>,
    metalists: HashMap<String, Vec<Metaval>>,
}

impl<'a> Default for ExpandContext<'a> {
    fn default() -> Self {
        Self {
            parent: Default::default(),
            sigil: '$',
            metavars: Default::default(),
            metalists: Default::default(),
        }
    }
}


impl ExpandContext<'static> {
    pub fn new() -> Self {
        let mut cx = Self::default();
        cx.let_var("dollar_sign".to_owned(), Metaval::Single(Tt::Punct(Punct::new('$', Spacing::Alone))));
        cx
    }
}

impl<'a> ExpandContext<'a> {
    pub fn child(&self) -> ExpandContext<'_> {
        ExpandContext {
            parent: Some(self),
            sigil: self.sigil,
            ..Default::default()
        }
    }

    pub fn let_var(&mut self, name: String, value: impl Into<Metaval>) {
        self.metavars.insert(name, value.into());
    }

    pub fn value(&self, name: &Ident) -> Result<&Metaval, TokenStream> {
        if let Some(val) = self.metavars.get(&name.to_string()) {
            Ok(val)
        } else if let Some(parent) = self.parent {
            parent.value(name)
        } else {
            Err(error(name, "unknown metavariable"))
        }
    }

    pub fn value_spanned(&self, name: &str, span: Span) -> Result<&Metaval, TokenStream> {
        if let Some(val) = self.metavars.get(name) {
            Ok(val)
        } else if let Some(parent) = self.parent {
            parent.value_spanned(name, span)
        } else {
            Err(error(&span, "unknown metavariable"))
        }
    }

    pub fn let_list(&mut self, name: String, value: Vec<Metaval>) {
        self.metalists.insert(name, value);
    }

    pub fn list(&self, name: &Ident) -> Result<&Vec<Metaval>, TokenStream> {
        if let Some(val) = self.metalists.get(&name.to_string()) {
            Ok(val)
        } else if let Some(parent) = self.parent {
            parent.list(name)
        } else {
            Err(error(name, "unknown metalist"))
        }
    }
}


pub fn expand(mut cx: ExpandContext<'_>, stream: TokenStream) -> Result<TokenStream, TokenStream> {
    pub enum State {
        Code,
        Arrowhead,
        Arrow,
        StartDirective,
        UseChar,
        LetDollar,
        LetName { is_list: bool },
        LetEqual { is_list: bool, name: String },
        LetValue { is_list: bool, name: String },
        LetInterpolation { is_list: bool, name: String },
        ForDollar,
        ForName,
        ForIn { name: String },
        ForList { var: String, list: Vec<Metaval> },
        ForInterpolation { var: String, list: Vec<Metaval> },
        ForListInterpolation { var: String, list: Vec<Metaval> },
        Repeat,
        RepeatInterpolation,
        RepeatBody { count: u64 },
        Interpolation,
    }
    let mut state = State::Code;

    let mut code = Vec::new();
    let mut tentative = Vec::new();

    for tt in stream {
        match state {
            State::Code => {
                match tt {
                    Tt::Punct(ref punct) => {
                        match punct.as_char() {
                            '<' => {
                                state = State::Arrowhead;
                                tentative.push(tt);
                            }
                            c if c == cx.sigil => {
                                state = State::Interpolation;
                            }
                            _ => code.push(expand_if_group(&cx, tt)?),
                        }
                    },
                    _ => code.push(expand_if_group(&cx, tt)?),
                }
            },
            // <
            // <-
            State::Arrowhead | State::Arrow => {
                match tt {
                    Tt::Punct(ref punct) => {
                        if punct.as_char() == '-' {
                            state = match state {
                                State::Arrowhead => State::Arrow,
                                State::Arrow => State::StartDirective,
                                _ => unreachable!()
                            };
                            tentative.push(tt);
                        }
                    },
                    _ => {
                        code.extend(tentative.drain(..).rev());
                        code.push(expand_if_group(&cx, tt)?);
                    },
                }
            },
            // <--
            State::StartDirective => {
                match tt {
                    Tt::Ident(ident) => {
                        state = match &*ident.to_string() {
                            "use" => State::UseChar,
                            "let" => State::LetDollar,
                            "for" => State::ForDollar,
                            "repeat" => State::Repeat,
                            _ => return Err(error(&ident, "invalid directive")),
                        };
                    },
                    _ => return Err(error(&tt, "directive begins with a non-identifier")),
                }
            },
            // <--use
            State::UseChar => {
                match tt {
                    Tt::Punct(p) => {
                        cx.sigil = p.as_char();
                    }
                    _ => return Err(error(&tt, "only punctuation can be used as interpolation sigil")),
                }
                state = State::Code;
            },
            // <--let
            State::LetDollar => {
                check_punct(tt, cx.sigil, &format!("let directive name requires `{}`", cx.sigil))?;
                state = State::LetName { is_list: false };
            },
            // <--let $
            State::LetName { is_list } => {
                if !is_list && let Tt::Punct(ref p) = tt && p.as_char() == '*' {
                    state = State::LetName { is_list: true }
                } else {
                    let Tt::Ident(ref name) = tt else {return Err(error(&tt, "let directive name must be an identifier"))};
                    state = State::LetEqual { is_list, name: name.to_string() };
                }
            }
            // <--let $foo
            State::LetEqual { is_list, name } => {
                check_punct(tt, '=', "let directive requires `=` here")?;
                state = State::LetValue { is_list, name };
            }
            // <--let $foo =
            State::LetValue { is_list, name } => {
                if matches!(tt, Tt::Punct(ref punct) if punct.as_char() == cx.sigil) {
                    state = State::LetInterpolation { is_list, name };
                } else {
                    if !is_list {
                        let val = Metaval::parse(tt)?;
                        cx.let_var(name, val);
                        state = State::Code;
                    } else {
                        let list = Metaval::parse_list(tt)?;
                        cx.let_list(name, list);
                        state = State::Code;
                    }
                }
            }
            // <--let $foo = $
            State::LetInterpolation { is_list, name } => {
                if is_list && let Tt::Punct(ref p) = tt && p.as_char() == '*' {
                    let list = eval_list_interpolation(&cx, &tt)?;
                    cx.let_list(name, list);
                    state = State::Code;
                } else {
                    let val = eval_interpolation(&cx, &tt)?;
                    if !is_list {
                        cx.let_var(name, val);
                    } else {
                        cx.let_list(name, vec![val]);
                    }
                    state = State::Code;
                }
            }
            // <--for
            State::ForDollar => {
                check_punct(tt, cx.sigil, &format!("for directive name requires `{}`", cx.sigil))?;
                state = State::ForName;
            }
            // <--for $
            State::ForName => {
                let Tt::Ident(ref name) = tt else {return Err(error(&tt, "for directive name must be an identifier"))};
                state = State::ForIn { name: name.to_string() };
            }
            // <--for $foo
            State::ForIn { name } => {
                if !matches!(tt, Tt::Ident(ref ident) if ident.to_string() == "in") {
                    return Err(error(&tt, "for directive requires `in` here"));
                }
                state = State::ForList { var: name, list: Vec::new() };
            }
            // <--for $foo in
            State::ForList { var, mut list } => {
                match tt {
                    Tt::Group(ref g) if g.delimiter() == Delimiter::Brace => {
                        for value in list {
                            let mut cx = cx.child();
                            cx.let_var(var.clone(), value);
                            code.extend(expand(cx, g.stream())?);
                        }
                        state = State::Code;
                    }
                    Tt::Punct(ref p) if p.as_char() == cx.sigil => {
                        state = State::ForInterpolation { var, list };
                    }
                    _ => {
                        list.push(Metaval::parse(tt)?);
                        state = State::ForList { var, list };
                    }
                }
            }
            // <--for $foo in $
            State::ForInterpolation { var, mut list } => {
                if let Tt::Punct(ref p) = tt && p.as_char() == '*' {
                    state = State::ForListInterpolation { var, list };
                } else {
                    list.push(eval_interpolation(&cx, &tt)?);
                    state = State::ForList { var, list };
                }
            }
            // <--for $foo in $*
            State::ForListInterpolation { var, mut list } => {
                list.extend(eval_list_interpolation(&cx, &tt)?);
                state = State::ForList { var, list };
            }
            // <--repeat
            State::Repeat => {
                if let Tt::Punct(ref p) = tt && p.as_char() == cx.sigil {
                    state = State::RepeatInterpolation;
                } else {
                    let Tt::Literal(ref lit) = tt else {return Err(error(&tt, "repeat directive requires an integer literal count"))};
                    let Some(count) = parse_int(&lit.to_string()).ok() else {return Err(error(&tt, "repeat directive requires an integer count"))};
                    state = State::RepeatBody { count };
                }
            }
            // <--repeat $
            State::RepeatInterpolation => {
                let val = eval_interpolation(&cx, &tt)?;
                let Metaval::Single(Tt::Literal(ref lit)) = val else {return Err(error(&tt, "repeat directive count must evaluate to a single integer literal"))};
                let Some(count) = parse_int(&lit.to_string()).ok() else {return Err(error(&tt, "repeat directive count must evaluate to a single integer literal"))};
                state = State::RepeatBody { count };
            }
            // <--repeat N
            State::RepeatBody { count } => {
                if let Tt::Group(ref g) = tt && g.delimiter() == Delimiter::Brace {
                    for _ in 0..count {
                        code.extend(expand(cx.child(), g.stream())?);
                    }
                    state = State::Code;
                } else {
                    return Err(error(&tt, "repeat directive requires `{ }`"));
                }
            }
            // $
            State::Interpolation => {
                let val = eval_interpolation(&cx, &tt)?;
                match val.clone() {
                    Metaval::Single(token_tree) => code.push(token_tree),
                    Metaval::Multi(token_stream) => code.extend(token_stream),
                }
                state = State::Code;
            }
        }
    }

    Ok(code.into_iter().collect())
}

fn eval_interpolation(cx: &ExpandContext<'_>, tt: &Tt) -> Result<Metaval, TokenStream> {    
    match tt {
        Tt::Ident(ident) => {
            let val = cx.value(ident)?;
            Ok(val.clone())
        },
        Tt::Group(group) if group.delimiter() == Delimiter::Brace => {
            eval_expression(cx, group)
        },
        Tt::Punct(p) if p.as_char() == cx.sigil => {
            Ok(Metaval::Single(tt.clone()))
        },
        _ => Err(error(tt, &format!("`{}` requires an identifier or `{{ }}`", cx.sigil)))
    }
}

fn eval_list_interpolation(cx: &ExpandContext<'_>, tt: &Tt) -> Result<Vec<Metaval>, TokenStream> {
    match tt {
        Tt::Ident(ident) => {
            let val = cx.list(ident)?;
            Ok(val.clone())
        },
        _ => Err(error(tt, "list interpolation requires an identifier"))
    }
}

fn eval_expression(xcx: &ExpandContext<'_>, group: &Group) -> Result<Metaval, TokenStream> {
    let mut result: Option<(Tt, Metaval)> = None;

    for tt in group.stream() {
        match tt {
            Tt::Ident(ref ident) => {
                let val = xcx.value(ident)?;
                if let Some((ref prev_tt, ref mut result)) = result {
                    *result = result.clone().concatenate(val).map_err(|e| map_cat_err(e, tt, prev_tt))?;
                } else {
                    result = Some((tt, val.clone()));
                }
            }
            Tt::Group(ref g) if g.delimiter() == Delimiter::Bracket => {
                let val = Metaval::parse(tt.clone())?;
                if let Some((ref prev_tt, ref mut result)) = result {
                    *result = result.clone().concatenate(&val).map_err(|e| map_cat_err(e, tt, prev_tt))?;
                } else {
                    result = Some((tt, val));
                }
            }
            Tt::Literal(ref l) => {
                let kind = LitKind::of(l);
                if let Some(string_type) = kind.string_type() {
                    let s = l.to_string();
                    let (content, "") = parse_string(&s) else { return Err(error(&tt, "literal suffix is not allowed here")) };
                    let val = xcx.value_spanned(content, tt.span())?;
                    let val = val.stringify(string_type);

                    if let Some((ref prev_tt, ref mut result)) = result {
                        *result = result.clone().concatenate(&val).map_err(|e| map_cat_err(e, tt, prev_tt))?;
                    } else {
                        result = Some((tt, val.clone()));
                    }
                } else {
                    return Err(error(&tt, "unsupported expression literal token"));
                }
            }
            _ => return Err(error(&tt, "unsupported expression token")),
        }
    }

    let Some((_, result)) = result else {return Err(error(group, "empty interpolation expressions are not allowed"))};

    Ok(result)
}

fn map_cat_err(e: ConcatenateError, next_tt: Tt, prev_tt: &Tt) -> TokenStream {
    match e {
        ConcatenateError::RhsMulti => error(&next_tt, "only individual tokens can be concatenated"),
        ConcatenateError::LhsMulti => error(prev_tt, "only individual tokens can be concatenated"),
        ConcatenateError::RhsBadLiteralForIdent => error(&next_tt, "invalid literal to concatenate with identifier"),
        ConcatenateError::RhsBadNumberForIdent => error(&next_tt, "cannot concatenate non-identifier characters with identifier"),
        ConcatenateError::LhsSuffix => error(prev_tt, "this literal's suffix prevents this concatenation"),
        ConcatenateError::BadUnknown => error(&next_tt, "invalid concatenation"),
    }
}


pub fn check_punct(token: Tt, punct: char, msg: &str) -> Result<Tt, TokenStream> {
    if !matches!(token, Tt::Punct(ref p) if p.as_char() == punct) {
        return Err(error(&token, msg));
    }
    Ok(token)
}

fn expand_if_group(cx: &ExpandContext<'_>, tt: Tt) -> Result<Tt, TokenStream> {
    let Tt::Group(group) = tt else {return Ok(tt)};

    Ok(Tt::Group(Group::new(
        group.delimiter(),
        expand(cx.child(), group.stream())?,
    )))
}
