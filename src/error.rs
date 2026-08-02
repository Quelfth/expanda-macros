
use std::iter;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

pub trait ErrorSpan {
    fn span(&self) -> Span;
}

impl ErrorSpan for Span {
    fn span(&self) -> Span { *self }
}

impl ErrorSpan for TokenTree {
    fn span(&self) -> Span { self.span() }
}

impl ErrorSpan for Group {
    fn span(&self) -> Span { self.span() }
}

impl ErrorSpan for Ident {
    fn span(&self) -> Span { self.span() }
}

pub fn error(span: &impl ErrorSpan, msg: &str) -> TokenStream {
    let span = span.span();
    
    TokenStream::from_iter([
        TokenTree::Ident(Ident::new("compile_error", span.span())),
        TokenTree::Punct({let mut token = Punct::new('!', Spacing::Alone); token.set_span(span); token}),
        TokenTree::Group({let mut token = Group::new(Delimiter::Parenthesis, TokenStream::from_iter(iter::once(
            TokenTree::Literal({let mut token = Literal::string(msg); token.set_span(span); token}),
        ))); token.set_span(span); token}),
    ])
}
