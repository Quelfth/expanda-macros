use std::collections::HashMap;

use proc_macro::{Delimiter, Punct, Spacing, TokenStream, TokenTree as Tt};

use crate::{error::error, expand::metaval::{Metaval, MetavalToken}, literal::{self, LitKind}};

use super::ExpandContext;

pub struct PatternIter<'a, 'cx> {
    cx: &'a ExpandContext<'cx>,
    stream: <TokenStream as IntoIterator>::IntoIter,
}

impl<'a, 'cx> PatternIter<'a, 'cx> {
    pub fn new(cx: &'a ExpandContext<'cx>, stream: TokenStream) -> Self {
        Self {
            cx,
            stream: stream.into_iter(),
        }
    }
}

pub enum Pattern {
    Token(Tt),
    Capture {
        name: Option<String>,
        repeat: (u64, Option<u64>),
        negative: Option<PatternBody>,
        body: Option<PatternBody>,
    },
}

pub enum PatternBody {
    Sequence(TokenStream),
    Alternatives(TokenStream),
}

impl Iterator for PatternIter<'_, '_> {
    type Item = Result<Pattern, TokenStream>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token = self.stream.next()?;
        if let Tt::Punct(ref p) = token && p.as_char() == self.cx.sigil {
            token = self.stream.next()?;
            let mut name = None;

            if let Tt::Ident(ref i) = token && i.to_string() != "_" {
                name = Some(i.to_string());
                token = self.stream.next()?;
            }

            let mut repeat = (1, Some(1));
            match token {
                Tt::Punct(ref p) if p.as_char() == '*' => {
                    repeat = (0, None);
                    token = self.stream.next()?;
                }
                Tt::Punct(ref p) if p.as_char() == '+' => {
                    repeat = (1, None);
                    token = self.stream.next()?;
                }
                Tt::Punct(ref p) if p.as_char() == '?' => {
                    repeat = (0, Some(1));
                    token = self.stream.next()?;
                }
                Tt::Group(ref g) if g.delimiter() == Delimiter::Brace => {
                    let tokens: Vec<Tt> = g.stream().into_iter().collect();
                    match &*tokens {
                        [Tt::Literal(num)] if LitKind::of(num) == LitKind::Number => {
                            let Ok(value) = literal::parse_int(&num.to_string()) else { return Some(Err(error(num, &format!("{num} is not a valid integer"))))};
                            repeat = (value, Some(value));
                        }
                        [Tt::Punct(a), Tt::Punct(b)] if is_dot_dot(a, b) => {
                            repeat = (0, None);
                        }
                        [Tt::Literal(num), Tt::Punct(a), Tt::Punct(b)]
                            if LitKind::of(num) == LitKind::Number && is_dot_dot(a, b)
                        => {
                            let Ok(value) = literal::parse_int(&num.to_string()) else { return Some(Err(error(num, &format!("{num} is not a valid integer"))))};
                            repeat = (value, None);
                        }
                        [Tt::Punct(a), Tt::Punct(b), Tt::Literal(num)]
                            if LitKind::of(num) == LitKind::Number && is_dot_dot(a, b)
                        => {
                            let Ok(value) = literal::parse_int(&num.to_string()) else { return Some(Err(error(num, &format!("{num} is not a valid integer"))))};
                            repeat = (0, Some(value));
                        }
                        [Tt::Literal(start), Tt::Punct(a), Tt::Punct(b), Tt::Literal(end)]
                            if LitKind::of(start) == LitKind::Number && is_dot_dot(a, b) && LitKind::of(end) == LitKind::Number
                        => {
                            let Ok(start) = literal::parse_int(&start.to_string()) else { return Some(Err(error(start, &format!("{start} is not a valid integer"))))};
                            let Ok(end) = literal::parse_int(&end.to_string()) else { return Some(Err(error(end, &format!("{end} is not a valid integer"))))};
                            repeat = (start, Some(end));
                        }
                        _ => return Some(Err(error(g, "capture repetition `{ }` must contain either an integer, or a range like `..` with optional integers on either side"))),
                    }
                    token = self.stream.next()?;
                }
                _ => ()
            }

            let mut negative = None;

            if let Tt::Punct(ref p) = token && p.as_char() == '^' {
                token = self.stream.next()?;
                let Tt::Group(ref g) = token else {return Some(Err(error(&token, "capture requires `( )` or `[ ]` here")))};
                match g.delimiter() {
                    Delimiter::Parenthesis => negative = Some(PatternBody::Sequence(g.stream())),
                    Delimiter::Bracket => negative = Some(PatternBody::Alternatives(g.stream())),
                    _ => return Some(Err(error(g, "capture requires `( )` or `[ ]` here"))),
                }
                token = self.stream.next()?;
            }

            let mut body = None;

            if !matches!(&token, Tt::Punct(p) if p.as_char() == '.') {
                let Tt::Group(ref g) = token else {return Some(Err(error(&token, "capture requires `( )` or `[ ]` here")))};
                match g.delimiter() {
                    Delimiter::Parenthesis => body = Some(PatternBody::Sequence(g.stream())),
                    Delimiter::Bracket => body = Some(PatternBody::Alternatives(g.stream())),
                    _ => return Some(Err(error(g, "capture requires `.`, `( )`, or `[ ]` here"))),
                }
            }

            Some(Ok(Pattern::Capture {
                name,
                repeat,
                negative,
                body,
            }))
        } else {
            Some(Ok(Pattern::Token(token)))
        }
    }
}

fn is_dot_dot(l: &Punct, r: &Punct) -> bool {
    l.as_char() == '.' && r.as_char() == '.' && l.spacing() == Spacing::Joint
}

pub struct MatchContext {
    depth: u32,
    vars: HashMap<String, Metaval>,
}

impl MatchContext {
    pub fn new(depth: u32) -> Self {
        Self {
            depth,
            vars: HashMap::new(),
        }
    }

    pub fn push_val(&mut self, name: String, val: &[Tt]) {
        self
            .vars
            .entry(name)
            .or_default()
            .extend(
                val
                    .iter()
                    .cloned()
                    .map(
                        MetavalToken::order_fn(self.depth)
                    )
            )
    }

    pub fn merge(&mut self, other: Self) {
        for (key, value) in other.vars {
            let entry = self.vars.entry(key).or_default();
            entry.extend(value);
            entry.raise_final();
        }
    }

    pub fn into_vars(self) -> impl Iterator<Item = (String, Metaval)> {
        self.vars.into_iter()
    }
}



impl Pattern {
    pub fn matches(&self, cx: &ExpandContext<'_>, mut mcx: Option<&mut MatchContext>, stream: &[Tt]) -> Result<Option<usize>, TokenStream> {
        match self {
            Pattern::Token(tt) => {
                if stream.is_empty() {
                    return Ok(None);
                }
                match &stream[0] {
                    Tt::Group(g) => {
                        let Tt::Group(h) = tt else {return Ok(None)};
                        if g.delimiter() != h.delimiter() { return Ok(None) }

                        let mut stream = &*g.stream().into_iter().collect::<Vec<_>>();

                        for pattern in PatternIter::new(cx, h.stream()) {
                            match pattern?.matches(cx, mcx.as_deref_mut(), stream)? {
                                Some(count) => stream = &stream[count..],
                                None => return Ok(None),
                            }
                        }

                        if !stream.is_empty() {
                            return Ok(None);
                        }

                        Ok(Some(1))
                    }
                    t => Ok((t.to_string() == tt.to_string()).then_some(1)),
                }
            },
            Pattern::Capture { name, repeat, negative, body } => {
                let mut count = 0;
                let mut len = 0;

                let mut new_mcx = mcx.as_ref().map(|mcx| MatchContext::new(mcx.depth + 1));

                loop {
                    if len == stream.len() {
                        break
                    }
                    if let Some(max) = repeat.1
                        && count >= max {
                            break
                        }

                    if let Some(negative) = negative
                        && negative.matches(cx, None, &stream[len..])?.is_some() {
                            break
                        }
                    if let Some(body) = body {
                        if let Some(l) = body.matches(cx, new_mcx.as_mut(), &stream[len..])? {
                            len += l;
                            if l == 0 {
                                if count < repeat.0 {
                                    count = repeat.0;
                                }
                                break
                            }
                        } else {
                            break
                        }
                    } else {
                        if stream.len() > len {
                            len += 1;
                        } else {
                            break
                        }
                    }
                    count += 1;
                }

                if count < repeat.0 {
                    return Ok(None);
                }

                if let Some(mcx) = mcx {
                    if let Some(name) = name {
                        mcx.push_val(name.clone(), &stream[..len]);
                    }
                    if let Some(new_mcx) = new_mcx {
                        mcx.merge(new_mcx);
                    }
                }

                Ok(Some(len))
            },
        }
    }
}

impl PatternBody {
    pub fn matches(&self, cx: &ExpandContext<'_>, mut mcx: Option<&mut MatchContext>, stream: &[Tt]) -> Result<Option<usize>, TokenStream> {
        match self {
            PatternBody::Sequence(tokens) => {
                let mut len = 0;

                for pattern in PatternIter::new(cx, tokens.clone()) {
                    match pattern?.matches(cx, mcx.as_deref_mut(), &stream[len..])? {
                        Some(count) => len += count,
                        None => return Ok(None),
                    }
                }

                Ok(Some(len))
            },
            PatternBody::Alternatives(alternatives) => {
                for pattern in PatternIter::new(cx, alternatives.clone()) {
                    if let Some(len) = pattern?.matches(cx, mcx.as_deref_mut(), stream)? {
                        return Ok(Some(len));
                    }
                }

                Ok(None)
            },
        }
    }
}
