use proc_macro::{
    Delimiter,
    Span,
    TokenStream,
    TokenTree as Tt,
};

use crate::{
    error::error,
    util::{group, ident, punct, punct_joint},
};


pub fn declare_item(args: TokenStream, item: TokenStream) -> Result<TokenStream, TokenStream> {
    let Ok([Tt::Ident(name)]) = <[Tt; 1]>::try_from(args.into_iter().collect::<Vec<Tt>>()) else {
        return Err(error(&Span::call_site(), "declare item requires a single identifier as argument"));
    };

    Ok(TokenStream::from_iter([
        punct_joint(':'), punct(':'),
        ident("expanda"),
        punct_joint(':'), punct(':'),
        ident("declare"),
        punct('!'),
        group(Delimiter::Brace, TokenStream::from_iter([
            Tt::Ident(name),
            punct('='),
            group(Delimiter::Parenthesis, item.clone()),
        ])),
    ].into_iter().chain(item)))
}
