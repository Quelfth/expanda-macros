use std::{collections::HashMap, env, mem};

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree as Tt
};

use crate::{
    error::{ErrorSpan, error},
    expand::{metaval::{Case, ConcatenateError}, pattern::{MatchContext, PatternIter}},
    literal::{LitKind, StringType, parse_int, parse_string},
};

use metaval::Metaval;

mod metaval;
mod pattern;

pub struct ExpandContext<'a> {
    parent: Option<&'a Self>,
    sigil: char,
    metavars: HashMap<String, Metaval>,
}

impl<'a> Default for ExpandContext<'a> {
    fn default() -> Self {
        Self {
            parent: Default::default(),
            sigil: '$',
            metavars: Default::default(),
        }
    }
}


impl ExpandContext<'static> {
    pub fn new() -> Self {
        let mut cx = Self::default();
        cx.let_var("dollar_sign".to_owned(), Metaval::single(Tt::Punct(Punct::new('$', Spacing::Alone))));
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
}


pub fn expand(mut cx: ExpandContext<'_>, stream: TokenStream) -> Result<TokenStream, TokenStream> {
    enum State {
        Code,
        Arrowhead,
        Arrow,
        StartDirective,
        UseChar,
        Let,
        LetName,
        LetEqual { name: NameOrPattern },
        LetValue { name: NameOrPattern, interpolation: bool },
        LetEnv,
        ForDollar,
        ForName,
        ForIn { name: String },
        ForList { var: String, list: Vec<Metaval> },
        ForInterpolation { var: String, list: Vec<Metaval> },
        ForListInterpolation { var: String, list: Vec<Metaval> },
        Repeat,
        RepeatInterpolation,
        RepeatBody { count: u64 },
        MatchScrutinee,
        MatchInterpolation,
        MatchBody { scrutinee: Metaval },
        DebugDollar,
        DebugName,
        Interpolation,
    }

    enum NameOrPattern {
        Name(String),
        Pattern {
            pattern: TokenStream,
            span: Span,
        },
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
                            '<' if punct.spacing() == Spacing::Joint => {
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
                    Tt::Punct(ref punct) if punct.as_char() == '-' => {
                        state = match state {
                            State::Arrowhead => {
                                tentative.push(tt);
                                State::Arrow
                            },
                            State::Arrow => {
                                tentative.clear();
                                State::StartDirective
                            },
                            _ => unreachable!()
                        };
                    },
                    Tt::Punct(ref punct) if punct.as_char() == '<' && punct.spacing() == Spacing::Joint => {
                        code.extend(mem::take(&mut tentative));
                        state = State::Arrowhead
                    }
                    Tt::Punct(ref punct) if punct.as_char() == cx.sigil => {
                        code.extend(mem::take(&mut tentative));
                        state = State::Interpolation
                    }
                    _ => {
                        code.extend(mem::take(&mut tentative));
                        code.push(expand_if_group(&cx, tt)?);
                        state = State::Code
                    },
                }
            },
            // <--
            State::StartDirective => {
                match tt {
                    Tt::Ident(ident) => {
                        state = match &*ident.to_string() {
                            "use" => State::UseChar,
                            "let" => State::Let,
                            "for" => State::ForDollar,
                            "repeat" => State::Repeat,
                            "match" => State::MatchScrutinee,
                            "debug" => State::DebugDollar,
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
            State::Let => {
                match tt {
                    Tt::Punct(ref punct) if punct.as_char() == cx.sigil => {
                        state = State::LetName;
                    }
                    Tt::Group(ref g) => {
                        state = State::LetEqual { name: NameOrPattern::Pattern {
                            pattern: g.stream(),
                            span: g.span(),
                        }};
                    }
                    Tt::Ident(ref i) if i.to_string() == "env" => {
                        state = State::LetEnv
                    }
                    _ => return Err(error(&tt, &format!("let directive requires `{}name` or `( )`, `[ ]` or `{{  }}` here", cx.sigil)))
                }
            },
            // <--let $
            State::LetName => {
                let Tt::Ident(ref name) = tt else {return Err(error(&tt, "let directive name must be an identifier"))};
                state = State::LetEqual { name: NameOrPattern::Name(name.to_string()) };
            }
            // <--let $name
            State::LetEqual { name } => {
                check_punct(tt, '=', "let directive requires `=` here")?;
                state = State::LetValue { name, interpolation: false };
            }
            // <--let $name =
            State::LetValue { name, interpolation } => {
                if !interpolation && matches!(tt, Tt::Punct(ref punct) if punct.as_char() == cx.sigil) {
                    state = State::LetValue { name, interpolation: true };
                } else {
                    let value = if interpolation {eval_interpolation(&cx, &tt)?} else {expand_metaval(&cx, tt)?};
                    match name {
                        NameOrPattern::Name(name) => cx.let_var(name, value),
                        NameOrPattern::Pattern { pattern, span } => if !match_pattern(&mut cx, &value, pattern)? {
                            return Err(error(&span, "let pattern failed to match"));
                        },
                    }
                    state = State::Code;
                }
            }
            State::LetEnv => {
                let Tt::Ident(ref name) = tt else {return Err(error(&tt, "let env directive requires an identifier here"))};
                let Ok(var) = env::var(name.to_string()) else {return Err(error(&tt, &format!("unable to read env var {name}")))};
                cx.let_var(name.to_string(), Tt::Literal(Literal::string(&var)));
                state = State::Code;
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
            // <--for $name
            State::ForIn { name } => {
                if !matches!(tt, Tt::Ident(ref ident) if ident.to_string() == "in") {
                    return Err(error(&tt, "for directive requires `in` here"));
                }
                state = State::ForList { var: name, list: Vec::new() };
            }
            // <--for $var in
            State::ForList { var, mut list } => {
                match tt {
                    Tt::Group(ref g) if g.delimiter() == Delimiter::Brace => {
                        if list.len() == 1 {
                            list = list[0].split().collect();
                        }
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
                        list.push(expand_metaval(&cx, tt)?);
                        state = State::ForList { var, list };
                    }
                }
            }
            // <--for $var in $
            State::ForInterpolation { var, mut list } => {
                if let Tt::Punct(ref p) = tt && p.as_char() == '*' {
                    state = State::ForListInterpolation { var, list };
                } else {
                    list.push(eval_interpolation(&cx, &tt)?);
                    state = State::ForList { var, list };
                }
            }
            // <--for $var in $*
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
                let Some(Tt::Literal(ref lit)) = val.into_single() else {return Err(error(&tt, "repeat directive count must evaluate to a single integer literal"))};
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
            // <--match
            State::MatchScrutinee => {
                if matches!(tt, Tt::Punct(ref punct) if punct.as_char() == cx.sigil) {
                    state = State::MatchInterpolation;
                } else {
                    state = State::MatchBody { scrutinee: expand_metaval(&cx, tt)? };
                }
            }
            // <--match $
            State::MatchInterpolation => {
                let scrutinee = eval_interpolation(&cx, &tt)?;
                state = State::MatchBody { scrutinee };
            }
            // <--match $name
            State::MatchBody { scrutinee } => {
                let Tt::Group(group) = tt else { return Err(error(&tt, "match directive requires `( )`, `[ ]`, or `{ }` here")) };
                code.extend(expand_match(&cx, scrutinee, group)?);
                state = State::Code;
            }
            // <--debug
            State::DebugDollar => {
                check_punct(tt, cx.sigil, &format!("debug requires `{}`", cx.sigil))?;
                state = State::DebugName;
            }
            // <--debug $
            State::DebugName => {
                let val = eval_interpolation(&cx, &tt)?;
                return Err(error(&tt, &format!("evaluates to: {val}")))
            }
            // $
            State::Interpolation => {
                let val = eval_interpolation(&cx, &tt)?;

                code.extend(val.to_stream());

                state = State::Code;
            }
        }
    }

    match state {
        State::Code | State::Arrowhead | State::Arrow => Ok(code.into_iter().chain(tentative).collect()),
        State::StartDirective => Err(error(&Span::call_site(), "directive arrow is missing directive")),
        State::Interpolation => Err(error(&Span::call_site(), "interpolation sigil missing interpolation")),
        _ => Err(error(&Span::call_site(), "incomplete directive")),
    }
}

fn expand_metaval(cx: &ExpandContext<'_>, token: Tt) -> Result<Metaval, TokenStream> {
    match token {
        Tt::Group(group) => Ok(expand(cx.child(), group.stream())?.into()),
        tt => Ok(tt.into()),
    }
}

fn expand_match(cx: &ExpandContext<'_>, scrutinee: Metaval, body: Group) -> Result<TokenStream, TokenStream> {
    enum State {
        Pattern,
        ArrowStem { pattern: TokenStream },
        ArrowHead { pattern: TokenStream },
        Body { pattern: TokenStream },
    }
    let mut state = State::Pattern;

    for tt in body.stream() {
        state = match state {
            State::Pattern => {
                let Tt::Group(ref g) = tt else {return Err(error(&tt, "match arm pattern requires `( )`, `[ ]`, or `{ }`"))};
                State::ArrowStem { pattern: g.stream() }
            },
            State::ArrowStem { pattern } => {
                check_punct(tt, '=', "match arm requires `=>`")?;
                State::ArrowHead { pattern }
            },
            State::ArrowHead { pattern } => {
                check_punct(tt, '>', "match arm requires `=>`")?;
                State::Body { pattern }
            },
            State::Body { pattern } => {
                let Tt::Group(ref g) = tt else {return Err(error(&tt, "match arm body requires `( )`, `[ ]`, or `{ }`"))};

                let mut cx = cx.child();
                if match_pattern(&mut cx, &scrutinee, pattern)? {
                    return expand(cx, g.stream());
                } else {
                    state = State::Pattern;
                    continue;
                }
            },
        };
    }

    Err(error(&body, "match directive failed to match any branches"))
}

fn match_pattern(cx: &mut ExpandContext<'_>, scrutinee: &Metaval, pattern: TokenStream) -> Result<bool, TokenStream> {
    let mut mcx = MatchContext::new(0);
    let mut stream = &*scrutinee.to_stream().into_iter().collect::<Vec<_>>();

    for pattern in PatternIter::new(cx, pattern) {
        match pattern?.matches(cx, Some(&mut mcx), stream)? {
            Some(count) => stream = &stream[count..],
            None => {
                return Ok(false)
            },
        }
    }

    if !stream.is_empty() {
        return Ok(false);
    }

    for (name, value) in mcx.into_vars() {
        cx.let_var(name, value);
    }

    Ok(true)
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
            Ok(Metaval::single(tt.clone()))
        },
        _ => Err(error(tt, &format!("`{}` requires an identifier or `{{ }}`", cx.sigil)))
    }
}

fn eval_list_interpolation(cx: &ExpandContext<'_>, tt: &Tt) -> Result<Vec<Metaval>, TokenStream> {
    match tt {
        Tt::Ident(ident) => {
            Ok(cx.value(ident)?.split().collect())
        },
        _ => Err(error(tt, "list interpolation requires an identifier"))
    }
}


fn eval_expression(cx: &ExpandContext<'_>, group: &Group) -> Result<Metaval, TokenStream> {
    let mut tokens = Vec::new();

    enum State {
        Value,
        Operator(Span),
    }

    let mut state = State::Value;

    for tt in group.stream() {
        match state {
            State::Value => match tt {
                Tt::Ident(ref ident) => {
                    tokens.push(cx.value(ident)?.clone());
                }
                Tt::Group(ref g) if g.delimiter() == Delimiter::Parenthesis => {
                    tokens.push(expand_metaval(cx, tt.clone())?);
                }
                Tt::Group(ref g) if g.delimiter() == Delimiter::Brace => {
                    tokens.push(eval_expression(cx, g)?);
                }
                Tt::Literal(ref l) => {
                    let kind = LitKind::of(l);
                    if let Some(string_type) = kind.string_type() {
                        let s = l.to_string();
                        let (content, "") = parse_string(&s) else { return Err(error(&tt, "literal suffix is not allowed here")) };
                        let val = cx.value_spanned(content, tt.span())?;
                        let val = val.stringify(string_type);

                        tokens.push(val);
                    } else {
                        return Err(error(&tt, "unsupported expression literal token"));
                    }
                }
                Tt::Punct(ref p) if p.as_char() == '.' => {
                    state = State::Operator(p.span());
                }
                _ => return Err(error(&tt, "unsupported expression token")),
            }
            State::Operator(span) => {
                let Tt::Ident(ref i) = tt else {
                    return Err(error(&tt, "an identifier is required after `.`"));
                };

                let Some(value) = tokens.last_mut() else {
                    return Err(error(&span, "`.` must come after a value to operate on"))
                };

                match &*i.to_string() {
                    "stringify" => *value = value.stringify(StringType::String),
                    "snake_case" => *value = value.clone().recase(Case::Snake)?,
                    "upper_camel_case" => *value = value.clone().recase(Case::UpperCamel)?,
                    "screaming_snake_case" => *value = value.clone().recase(Case::ScreamingSnake)?,
                    "camel_case" => *value = value.clone().recase(Case::Camel)?,
                    "upper_snake_case" => *value = value.clone().recase(Case::UpperSnake)?,
                    "to_dashes" => *value = value.clone().to_dashes()?,
                    _ => return Err(error(&tt, "unknown interpolation operator"))
                }
                state = State::Value;
            }
        }
    }

    let mut iter = tokens.into_iter();
    let Some(mut prev) = iter.next() else {return Ok(Metaval::empty())};
    let mut token = prev.clone();
    for tt in iter {
        token = token.concatenate(&tt).map_err(|e| map_cat_err(
            e,
            &tt.first_span().unwrap_or_else(Span::call_site),
            &prev.last_span().unwrap_or_else(Span::call_site),
        ))?;
        prev = tt;
    }

    Ok(token)
}

fn map_cat_err(e: ConcatenateError, next_tt: &impl ErrorSpan, prev_tt: &impl ErrorSpan) -> TokenStream {
    match e {
        ConcatenateError::RhsMulti => error(next_tt, "only individual tokens can be concatenated"),
        ConcatenateError::LhsMulti => error(prev_tt, "only individual tokens can be concatenated"),
        ConcatenateError::RhsBadLiteralForIdent => error(next_tt, "invalid literal to concatenate with identifier"),
        ConcatenateError::RhsBadNumberForIdent => error(next_tt, "cannot concatenate non-identifier characters with identifier"),
        ConcatenateError::LhsSuffix => error(prev_tt, "this literal's suffix prevents this concatenation"),
        ConcatenateError::BadUnknown => error(next_tt, "invalid concatenation"),
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
