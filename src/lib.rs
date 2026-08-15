use std::convert::identity;

use proc_macro::TokenStream;

use crate::expand::ExpandContext;

mod expand;
mod literal;
mod import;
mod declare;
mod error;
mod util;

#[proc_macro]
pub fn expand(input: TokenStream) -> TokenStream {
    expand::expand(ExpandContext::new(), input).unwrap_or_else(identity)
}

#[proc_macro_attribute]
pub fn expand_attr(input: TokenStream, item: TokenStream) -> TokenStream {
    match expand::expand(ExpandContext::new(), input) {
        Ok(attrs) => TokenStream::from_iter([attrs, item]),
        Err(error) => error,
    }
}

#[proc_macro_attribute]
pub fn using(args: TokenStream, item: TokenStream) -> TokenStream {
    import::using(args, item).unwrap_or_else(identity)
}

#[proc_macro_attribute]
pub fn declare_item(args: TokenStream, item: TokenStream) -> TokenStream {
    declare::declare_item(args, item).unwrap_or_else(identity)
}
