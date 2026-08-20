use proc_macro::{
    Delimiter,
    TokenStream,
};

use crate::{
    util::{group, ident, punct, punct_joint},
};


pub fn declare_item(args: TokenStream, item: TokenStream) -> Result<TokenStream, TokenStream> {
    Ok(TokenStream::from_iter([
        punct_joint(':'), punct(':'),
        ident("expanda"),
        punct_joint(':'), punct(':'),
        ident("declare"),
        punct('!'),
        group(Delimiter::Brace, {
            let mut stream = TokenStream::new();
            stream.extend(args);
            stream.extend([
                punct('='),
                group(Delimiter::Parenthesis, item.clone()),
            ]);
            stream
        }),
    ].into_iter().chain(item)))
}
