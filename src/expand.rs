use std::{collections::HashMap, ffi::CString, num::ParseIntError};

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree as Tt};
use quote::{quote_spanned};

use crate::error::{error, err};

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

#[derive(Clone)]
pub enum Metaval {
    Single(Tt),
    Multi(TokenStream),
}

impl Metaval {
    pub fn parse(tt: Tt) -> Result<Self, TokenStream> {
        match tt {
            Tt::Group(group) => {
                match group.delimiter() {
                    Delimiter::Bracket | Delimiter::None => {
                        let stream = group.stream();
                        let mut clone = stream.clone().into_iter();
                        if let Some(token) = clone.next() && clone.next().is_none() {
                            return Ok(token.into());
                        }
                        Ok(stream.into())
                    }
                    _ => err!(group, "invalid metavalue delimiter"),
                }
            },
            _ => Ok(tt.into())
        }
    }

    pub fn parse_list(tt: Tt) -> Result<Vec<Self>, TokenStream> {
        match tt {
            Tt::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                let stream = group.stream();
                let mut list = Vec::new();

                for token in stream {
                    let val = Metaval::parse(token)?;
                    list.push(val);
                }
                Ok(list)
            },
            _ => err!(tt, "metalist requires `( )`"),
        }
    }

    pub fn concatenate(self, rhs: &Self) -> Result<Self, ConcatenateError> {
        fn unraw(maybe_raw: &str) -> &str {
            maybe_raw.strip_prefix("r#").unwrap_or(maybe_raw)
        }
        match (self, rhs) {
            (Metaval::Single(lhs), Metaval::Single(rhs)) => {
                match (lhs, rhs) {
                    (Tt::Ident(lhs), Tt::Ident(rhs)) => {
                        let cat = &(unraw(&lhs.to_string()).to_owned() + unraw(&rhs.to_string()));

                        Ok(Self::Single(Tt::Ident(make_ident(cat, lhs.span()))))
                    }
                    (Tt::Ident(lhs), Tt::Literal(rhs)) => {
                        match LitKind::of(rhs) {
                            LitKind::Number => {
                                let r = rhs.to_string();
                                if r.chars().any(|c| !(c.is_alphanumeric() || c == '_')) {
                                    return Err(ConcatenateError::RhsBadNumberForIdent);
                                }
                                let cat = &(unraw(&lhs.to_string()).to_owned() + &r);

                                Ok(Self::Single(Tt::Ident(make_ident(cat, lhs.span()))))
                            },
                            _ => Err(ConcatenateError::RhsBadLiteralForIdent),
                        }
                    }
                    (Tt::Literal(lhs), Tt::Literal(rhs)) => {
                        let lkind = LitKind::of(&lhs);
                        let rkind = LitKind::of(rhs);
                        if let Some(ltype) = lkind.string_type() && let Some(rtype) = rkind.string_type() && ltype == rtype {
                            let l = lhs.to_string();
                            let r = rhs.to_string();
                            let (l, l_suffix) = parse_strlit(&l);
                            let (r, r_suffix) = parse_strlit(&r);
                            let l = if lkind.is_raw() {l.to_owned()} else {unescape(l)};
                            let r = if rkind.is_raw() {r.to_owned()} else {unescape(r)};
                            if !l_suffix.is_empty() {return Err(ConcatenateError::LhsSuffix)}
                            let mut result = ltype.make_literal(&(l.to_owned() + &r));
                            result.set_span(lhs.span());
                            if !r_suffix.is_empty() {
                                result = (result.to_string() + r_suffix).parse().unwrap();
                            }
                            Ok(Self::Single(Tt::Literal(result)))
                        } else {
                            Err(ConcatenateError::BadUnknown)
                        }
                    }
                    _ => Err(ConcatenateError::BadUnknown)
                }
            },
            (Metaval::Single(_), Metaval::Multi(_)) => Err(ConcatenateError::RhsMulti),
            _ => Err(ConcatenateError::LhsMulti),
        }
    }

    pub fn stringify(&self, r#type: StringType) -> Self {
        let string = match self {
            Metaval::Single(token_tree) => token_tree.to_string(),
            Metaval::Multi(token_stream) => token_stream.to_string(),
        };
        let lit = r#type.make_literal(&string);
        Self::Single(Tt::Literal(lit))
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum StringType {
    String,
    ByteString,
    CString,
}

impl StringType {
    pub fn make_literal(self, string: &str) -> Literal {
        match self {
            StringType::String => Literal::string(string),
            StringType::ByteString => Literal::byte_string(string.as_bytes()),
            StringType::CString => Literal::c_string(&CString::new(string).unwrap()),
        }
    }
}

pub enum ConcatenateError {
    LhsMulti,
    RhsMulti,
    RhsBadLiteralForIdent,
    RhsBadNumberForIdent,
    LhsSuffix,
    BadUnknown,
}

impl From<Tt> for Metaval {
    fn from(value: Tt) -> Self {
        Self::Single(value)
    }
}

impl From<TokenStream> for Metaval {
    fn from(value: TokenStream) -> Self {
        Self::Multi(value)
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
            err!(name, "unknown metavariable")
        }
    }

    pub fn value_spanned(&self, name: &str, span: Span) -> Result<&Metaval, TokenStream> {
        if let Some(val) = self.metavars.get(name) {
            Ok(val)
        } else if let Some(parent) = self.parent {
            parent.value_spanned(name, span)
        } else {
            Err(quote_spanned!{
                span.into() => compile_error!("unknown metavariable")
            }.into())
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
            err!(name, "unknown metalist")
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
                            _ => return err!(ident, "invalid directive"),
                        };
                    },
                    _ => return err!(tt, "directive begins with a non-identifier"),
                }
            },
            // <--use
            State::UseChar => {
                match tt {
                    Tt::Punct(p) => {
                        cx.sigil = p.as_char();
                    }
                    _ => return err!(tt, "only punctuation can be used as interpolation sigil"),
                }
                state = State::Code;
            },
            // <--let
            State::LetDollar => {
                assert_punct!(tt, cx.sigil, "let directive name requires `$`");
                state = State::LetName { is_list: false };
            },
            // <--let $
            State::LetName { is_list } => {
                if !is_list && let Tt::Punct(ref p) = tt && p.as_char() == '*' {
                    state = State::LetName { is_list: true }
                } else {
                    let Tt::Ident(ref name) = tt else {return err!(tt, "let directive name must be an identifier")};
                    state = State::LetEqual { is_list, name: name.to_string() };
                }
            }
            // <--let $foo
            State::LetEqual { is_list, name } => {
                assert_punct!(tt, '=', "let directive requires `=` here");
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
                assert_punct!(tt, cx.sigil, "for directive name requires `$`");
                state = State::ForName;
            }
            // <--for $
            State::ForName => {
                let Tt::Ident(ref name) = tt else {return err!(tt, "for directive name must be an identifier")};
                state = State::ForIn { name: name.to_string() };
            }
            // <--for $foo
            State::ForIn { name } => {
                if !matches!(tt, Tt::Ident(ref ident) if ident.to_string() == "in") {
                    return err!(tt, "for directive requires `in` here");
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
                    let Tt::Literal(ref lit) = tt else {return err!(tt, "repeat directive requires an integer literal count")};
                    let Some(count) = parse_intlit(&lit.to_string()).ok() else {return err!(tt, "repeat directive requires an integer count")};
                    state = State::RepeatBody { count };
                }
            }
            // <--repeat $
            State::RepeatInterpolation => {
                let val = eval_interpolation(&cx, &tt)?;
                let Metaval::Single(Tt::Literal(ref lit)) = val else {return err!(tt, "repeat directive count must evaluate to a single integer literal")};
                let Some(count) = parse_intlit(&lit.to_string()).ok() else {return err!(tt, "repeat directive count must evaluate to a single integer literal")};
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
                    return err!(tt, "repeat directive requires `{ }`");
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
        _ => err!(tt, "`$` requires an identifier or `{ }`")
    }
}

fn eval_list_interpolation(cx: &ExpandContext<'_>, tt: &Tt) -> Result<Vec<Metaval>, TokenStream> {
    match tt {
        Tt::Ident(ident) => {
            let val = cx.list(ident)?;
            Ok(val.clone())
        },
        _ => err!(tt, "list interpolation requires an identifier")
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
                    let (content, "") = parse_strlit(&s) else { return err!(tt, "literal suffix is not allowed here") };
                    let val = xcx.value_spanned(content, tt.span())?;
                    let val = val.stringify(string_type);

                    if let Some((ref prev_tt, ref mut result)) = result {
                        *result = result.clone().concatenate(&val).map_err(|e| map_cat_err(e, tt, prev_tt))?;
                    } else {
                        result = Some((tt, val.clone()));
                    }
                } else {
                    return err!(tt, "unsupported expression literal token");
                }
            }
            _ => return err!(tt, "unsupported expression token"),
        }
    }

    let Some((_, result)) = result else {return err!(group, "empty interpolation expressions are not allowed")};

    Ok(result)
}

fn map_cat_err(e: ConcatenateError, next_tt: Tt, prev_tt: &Tt) -> TokenStream {
    match e {
        ConcatenateError::RhsMulti => error!(next_tt, "only individual tokens can be concatenated"),
        ConcatenateError::LhsMulti => error!(prev_tt, "only individual tokens can be concatenated"),
        ConcatenateError::RhsBadLiteralForIdent => error!(next_tt, "invalid literal to concatenate with identifier"),
        ConcatenateError::RhsBadNumberForIdent => error!(next_tt, "cannot concatenate non-identifier characters with identifier"),
        ConcatenateError::LhsSuffix => error!(prev_tt, "this literal's suffix prevents this concatenation"),
        ConcatenateError::BadUnknown => error!(next_tt, "invalid concatenation"),
    }
}

macro_rules! assert_punct {
    ($token:ident, $punct:expr, $error:literal) => {
        if !matches!($token, Tt::Punct(ref punct) if punct.as_char() == $punct) {return err!($token, $error)}
    }
}
use assert_punct;

fn expand_if_group(cx: &ExpandContext<'_>, tt: Tt) -> Result<Tt, TokenStream> {
    let Tt::Group(group) = tt else {return Ok(tt)};

    Ok(Tt::Group(Group::new(
        group.delimiter(),
        expand(cx.child(), group.stream())?,
    )))
}

fn make_ident(string: &str, span: Span) -> Ident {
    match string {
        "as" | "async" | "await" | "break" | "const" | "continue" |
        "dyn" | "else" | "enum" | "extern" | "false" | "fn" | "for" |
        "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move" |
        "mut" | "pub" | "ref" | "return" | "static" | "struct" | "trait" |
        "true" | "type" | "unsafe" | "use" | "where" | "while" => Ident::new_raw(string, span),
        _ => Ident::new(string, span),
    }
}


#[allow(unused)]
#[derive(Copy, Clone)]
enum LitKind {
    Char,
    String,
    RawString(u8),
    ByteChar,
    ByteString,
    RawByteString(u8),
    CString,
    RawCString(u8),
    Number,
    Unknown,
}

impl LitKind {
    pub fn of(lit: &Literal) -> Self {
        let s = lit.to_string();
        let mut s = s.chars();
        let Some(c) = s.next() else { return Self::Unknown };
        match c {
            '\'' => Self::Char,
            '"' => Self::String,
            'r' => Self::RawString(s.take_while(|&c| c == '#').count() as _),
            'b' => {
                let Some(c) = s.next() else { return Self::Unknown };
                match c {
                    '\'' => Self::ByteChar,
                    '"' => Self::ByteString,
                    'r' => Self::RawByteString(s.take_while(|&c| c == '#').count() as _),
                    _ => Self::Unknown,
                }
            },
            'c' => {
                let Some(c) = s.next() else { return Self::Unknown };
                match c {
                    '"' => Self::CString,
                    'r' => Self::RawCString(s.take_while(|&c| c == '#').count() as _),
                    _ => Self::Unknown,
                }
            }
            '0'..='9' => Self::Number,
            _ => Self::Unknown,
        }
    }

    pub fn string_type(self) -> Option<StringType> {
        Some(match self {
            LitKind::String | LitKind::RawString(_) => StringType::String,
            LitKind::ByteString | LitKind::RawByteString(_) => StringType::ByteString,
            LitKind::CString | LitKind::RawCString(_) => StringType::CString,
            _ => return None
        })
    }

    pub fn is_raw(self) -> bool {
        matches!(self, LitKind::RawString(_) | LitKind::RawCString(_) | LitKind::RawByteString(_))
    }
}

fn parse_strlit(lit: &str) -> (&str, &str) {
    let i = lit.bytes().enumerate().find(|&(_, c)| c == b'"').unwrap().0;
    let j = lit.bytes().enumerate().rev().find(|&(_, c)| c == b'"').unwrap().0;
    let k = lit.bytes().enumerate().rev().find(|&(_, c)| matches!(c, b'"' | b'#')).unwrap().0;
    (&lit[i + 1..j], &lit[k + 1..])
}

fn unescape(str: &str) -> String {
    let mut chars = str.chars();
    let mut result = String::new();
    while let Some(char) = chars.next() {
        if char != '\\' {
            result.push(char);
            continue;
        }
        result.push(match chars.next().unwrap() {
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            '0' => '\0',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'x' => {
                u8::from_str_radix(&chars.by_ref().take(2).collect::<String>(), 0x10).unwrap().into()
            }
            'u' => {
                chars.next();
                u32::from_str_radix(&chars.by_ref().take_while(|&c| c != '}').collect::<String>(), 0x10).unwrap().try_into().unwrap()
            }
            _ => panic!(),
        })
    }
    result
}

fn parse_intlit(lit: &str) -> Result<u64, ParseIntError> {
    u64::from_str_radix(
        lit, 
        match lit.get(0..2) {
            Some("0x") => 16,
            Some("0b") => 2,
            Some("0o") => 8,
            _ => 10,
        },
    )
}
