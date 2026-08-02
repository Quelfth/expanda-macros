use std::convert::identity;

use proc_macro::TokenStream;

use crate::expand::ExpandContext;

mod expand;
mod literal;
mod import;
mod error;

#[proc_macro]
pub fn expand(input: TokenStream) -> TokenStream {
    expand::expand(ExpandContext::new(), input).unwrap_or_else(identity)
}

#[proc_macro_attribute]
pub fn using(args: TokenStream, item: TokenStream) -> TokenStream {
    import::using(args.into(), item.into()).unwrap_or_else(identity).into()
}

#[proc_macro]
pub fn using_fn(input: TokenStream) -> TokenStream {
    let mut i = input.into_iter();
    let proc_macro::TokenTree::Group(g) = i.next().unwrap() else {panic!()};
    let args = g.stream();
    let proc_macro::TokenTree::Group(g) = i.next().unwrap() else {panic!()};
    let item = g.stream();
    import::using(args.into(), item.into()).unwrap_or_else(identity).into()
}
