use proc_macro::{
    Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree
};

pub fn punct(punct: char) -> TokenTree {
    TokenTree::Punct(Punct::new(punct, Spacing::Alone))
}

pub fn punct_joint(punct: char) -> TokenTree {
    TokenTree::Punct(Punct::new(punct, Spacing::Joint))
}

pub fn group(delimiter: Delimiter, stream: TokenStream) -> TokenTree {
    TokenTree::Group(Group::new(delimiter, stream))
}

pub fn ident(text: &str) -> TokenTree {
    TokenTree::Ident(Ident::new(text, Span::call_site()))
}
