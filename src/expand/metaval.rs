use proc_macro::{Delimiter, TokenStream, TokenTree as Tt};

use crate::{
    error::error,
    literal::{LitKind, StringType, make_ident, parse_string, unescape},
};

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
                    _ => Err(error(&group, "invalid metavalue delimiter")),
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
            _ => Err(error(&tt, "metalist requires `( )`")),
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
                            let (l, l_suffix) = parse_string(&l);
                            let (r, r_suffix) = parse_string(&r);
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