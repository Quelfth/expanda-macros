use std::{collections::HashSet, slice};

use proc_macro::{Ident, Literal, Span, TokenStream, TokenTree as Tt};

use crate::{error::error, literal::{LitKind, StringType, make_ident, parse_string, unescape}};

mod display;

#[derive(Clone, Default, Debug)]
pub struct Metaval(Vec<MetavalToken>);

#[derive(Clone, Debug)]
pub struct MetavalToken {
    order: u32,
    token: Tt,
}

impl MetavalToken {
    pub fn order_fn(order: u32) -> impl Fn(Tt) -> Self {
        move |token| Self { order, token }
    }

    pub fn new(order: u32, token: Tt) -> Self {
        Self { order, token }
    }

    pub fn zero(token: Tt) -> Self {
        Self {
            order: 0,
            token,
        }
    }

    pub fn raise(&self) -> Self {
        Self {
            order: self.order.saturating_sub(1),
            token: self.token.clone()
        }
    }
}

impl Metaval {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn single(token: Tt) -> Self {
        Self(vec![MetavalToken::zero(token)])
    }

    pub fn flat(tokens: impl IntoIterator<Item = Tt>) -> Self {
        Self(tokens.into_iter().map(MetavalToken::zero).collect())
    }

    pub fn len_flat(&self) -> usize {
        self.0.len()
    }

    pub fn count(&self) -> usize {
        if self.0.is_empty() { 0 } else {
            self.0[0..self.0.len() - 1].iter().filter(|x| x.order == 0).count() + 1
        }
    }

    pub fn is_single(&self) -> bool {
        self.len_flat() == 1
    }

    pub fn as_single(&self) -> Option<&Tt> {
        if !self.is_single() { return None }
        Some(&self.0[0].token)
    }

    pub fn into_single(self) -> Option<Tt> {
        let [token] = self.0.try_into().ok()?;
        Some(token.token)
    }

    pub fn to_stream(&self) -> TokenStream {
        self
            .0
            .iter()
            .map(
                |MetavalToken { token, .. }| token.clone()
            ).collect()
    }

    pub fn simplify(mut self) -> Self {
        if self.0.is_empty() {
            return self;
        }
        let mut max_order = 0;
        let mut orders = HashSet::new();
        for token in &self.0[..self.0.len() - 1] {
            if token.order > max_order {
                max_order = token.order;
            }
            orders.insert(token.order);
        }

        let mut missing_orders = HashSet::new();
        for o in 0..=max_order {
            if !orders.contains(&o) {
                missing_orders.insert(o);
            }
        }

        for token in &mut self.0 {
            let order = token.order;
            for &m in &missing_orders {
                if m < order {
                    token.order = token.order.saturating_sub(1);
                }
            }
        }

        self
    }
    
    pub fn first_span(&self) -> Option<Span> {
        Some(self.0.first()?.token.span())
    }

    pub fn last_span(&self) -> Option<Span> {
        Some(self.0.last()?.token.span())
    }

    pub fn raise_final(&mut self) {
        if let Some(token) = self.0.last_mut() {
            token.order = token.order.saturating_sub(1);
        }
    }

    pub fn split(&self) -> impl Iterator<Item = Self> {
        Split { iter: self.0.iter() }
    }

    pub fn concatenate(self, rhs: &Self) -> Result<Self, ConcatenateError> {
        fn unraw(maybe_raw: &str) -> &str {
            maybe_raw.strip_prefix("r#").unwrap_or(maybe_raw)
        }
        match (self.into_single(), rhs.as_single()) {
            (Some(lhs), Some(rhs)) => {
                match (lhs, rhs) {
                    (Tt::Ident(lhs), Tt::Ident(rhs)) => {
                        let cat = &(unraw(&lhs.to_string()).to_owned() + unraw(&rhs.to_string()));

                        Ok(Self::single(Tt::Ident(make_ident(cat, lhs.span()))))
                    }
                    (Tt::Ident(lhs), Tt::Literal(rhs)) => {
                        match LitKind::of(rhs) {
                            LitKind::Number => {
                                let r = rhs.to_string();
                                if r.chars().any(|c| !(c.is_alphanumeric() || c == '_')) {
                                    return Err(ConcatenateError::RhsBadNumberForIdent);
                                }
                                let cat = &(unraw(&lhs.to_string()).to_owned() + &r);

                                Ok(Self::single(Tt::Ident(make_ident(cat, lhs.span()))))
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
                            Ok(Self::single(Tt::Literal(result)))
                        } else {
                            Err(ConcatenateError::BadUnknown)
                        }
                    }
                    _ => Err(ConcatenateError::BadUnknown)
                }
            },
            (Some(_), None) => Err(ConcatenateError::RhsMulti),
            _ => Err(ConcatenateError::LhsMulti),
        }
    }

    pub fn stringify(&self, r#type: StringType) -> Self {
        let string = self.to_stream().to_string();
        let lit = r#type.make_literal(&string);
        Self::single(Tt::Literal(lit))
    }

    pub fn recase(mut self, case: Case) -> Result<Self, TokenStream> {
        for token in &mut self.0 {
            let tt = &mut token.token;
            let Tt::Ident(ident) = tt else {
                return Err(error(tt, &format!("`{}` can only be applied to identifiers", case.operator_name())))
            };

            let mut words = Vec::new();
            let string = ident.to_string();


            for w in string.split("_") {
                let mut i = 0;
                let mut j = w.ceil_char_boundary(1);

                while let Some(slice) = w.get(j..) && let Some(char) = slice.chars().next() {
                    if char.is_uppercase() && w.get(w.ceil_char_boundary(j + 1)..).is_some_and(|slice| slice.chars().next().is_some_and(|c| !c.is_uppercase())) {
                        words.push(&w[i..j]);
                        i = j;
                    }
                    j += char.len_utf8();
                }

                words.push(&w[i..j]);
            }

            let mut text = String::new();

            let mut words = words.into_iter();

            if let Some(word) = words.next() {
                match case {
                    Case::Snake | Case::Camel => text += &word.to_lowercase(),
                    Case::UpperCamel | Case::UpperSnake => {
                        let i = word.ceil_char_boundary(1);
                        text += &format!("{}{}", word[..i].to_uppercase(), word[i..].to_lowercase())
                    }
                    Case::ScreamingSnake => text += &word.to_uppercase(),
                }
            }

            for word in words {
                if matches!(case, Case::Snake | Case::ScreamingSnake | Case::UpperSnake) {
                    text += "_";
                }
                match case {
                    Case::Snake => text += &word.to_lowercase(),
                    Case::UpperCamel | Case::Camel | Case::UpperSnake => {
                        let i = word.ceil_char_boundary(1);
                        text += &format!("{}{}", word[..i].to_uppercase(), word[i..].to_lowercase())
                    },
                    Case::ScreamingSnake => text += &word.to_uppercase(),
                }
            }

            if text.is_empty() || text.chars().any(|c| !(c.is_alphanumeric() || c == '_')) {
                return Err(error(&ident.span(), &format!("`{text}` is not a valid identifier")));
            }

            *ident = Ident::new(&text, ident.span());
        }

        Ok(self)
    }

    pub fn to_dashes(mut self) -> Result<Self, TokenStream> {
        for token in &mut self.0 {
            match &mut token.token {
                Tt::Literal(literal) => {
                    if LitKind::of(literal).string_type().is_some() {
                        let string = literal.to_string();
                        let (text, _) = parse_string(&string);
                        let text = text.replace("_", "-");
                        *literal = Literal::string(&text);
                    }
                },
                _ => continue,
            }
        }

        Ok(self)
    }

    pub fn len_token(&self) -> Self {
        Self::single(Tt::Literal(Literal::usize_unsuffixed(self.count())))
    }
}

#[derive(Copy, Clone)]
pub enum Case {
    Snake,
    UpperCamel,
    ScreamingSnake,
    Camel,
    UpperSnake,
}

impl Case {
    pub fn operator_name(self) -> &'static str {
        match self {
            Case::Snake => "snake_case",
            Case::UpperCamel => "upper_camel_case",
            Case::ScreamingSnake => "screaming_snake_case",
            Case::Camel => "camel_case",
            Case::UpperSnake => "upper_snake_case",
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
        Self::single(value)
    }
}

impl From<TokenStream> for Metaval {
    fn from(value: TokenStream) -> Self {
        Self::flat(value)
    }
}

impl IntoIterator for Metaval {
    type Item = MetavalToken;

    type IntoIter = <Vec<MetavalToken> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Extend<MetavalToken> for Metaval {
    fn extend<T: IntoIterator<Item = MetavalToken>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}

pub struct Split<'a> {
    iter: slice::Iter<'a, MetavalToken>,
}

impl<'a> Iterator for Split<'a> {
    type Item = Metaval;

    fn next(&mut self) -> Option<Self::Item> {
        let mut vals = Vec::new();
        for val in self.iter.by_ref() {
            vals.push(val.raise());
            if val.order == 0 {
                return Some(Metaval(vals))
            }
        }
        None
    }
}
