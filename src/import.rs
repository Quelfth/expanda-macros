use std::collections::VecDeque;

use proc_macro::Span;

use proc_macro::{
    Delimiter, Group, Ident, Punct, Spacing, TokenStream, TokenTree
};

use crate::error::error;

pub fn using(args: TokenStream, item: TokenStream) -> Result<TokenStream, TokenStream> {
    let mut item = item.into_iter().collect::<Vec<_>>();
    while let [TokenTree::Group(group)] = &*item {
        item = group.stream().into_iter().collect();
    }
    let item = item;
    
    if item.len() < 3
        || item[0..item.len()-2].iter().any(|t| {
            !match t {
                TokenTree::Ident(_) => true,
                TokenTree::Punct(punct) => punct.as_char() == ':',
                _ => false,
            }
        })
        || !if let TokenTree::Punct(ref p) = item[item.len()-2] && p.as_char() == '!' {true} else {false}
    {
        return Err(error(&Span::call_site(), "using must be placed on an `expand` invocation"));
    }
    let TokenTree::Group(ref g) = item[item.len() - 1] else {return Err(error(&Span::call_site(), "using must be placed on an `expand` invocation"))};
    if args.is_empty() {
        return Ok(item.into_iter().collect());
    }
    let contents = g.stream();

    let mut args = args.into_iter().collect::<VecDeque<_>>();
    let is_list = if let TokenTree::Punct(ref p) = args[0] && p.as_char() == '*' {
        args.pop_front();
        true
    } else {
        false
    };
    let name = args.back().unwrap().clone();
    let args = args.into_iter().collect::<TokenStream>();
    
    let invocation = item[0..item.len()-1].iter().cloned().collect::<TokenStream>();

    let mut stream = TokenStream::new();
    
    stream.extend(args);
    stream.extend([
        punct('!'),
        group(Delimiter::Brace, [
            fold_into(if is_list {Delimiter::Parenthesis} else {Delimiter::Bracket}),
            fold_right({
                let mut stream = TokenStream::new();
                stream.extend(long_left_arrow());
                stream.extend([
                    TokenTree::Ident(Ident::new("let", Span::call_site())),
                    punct('$')
                ]);
                if is_list {
                    stream.extend([punct('*')]);
                }
                stream.extend([
                    name,
                    punct('='),
                ]);
                
                stream
            }),
            fold_left(contents),
            fold_into(Delimiter::Brace),
            fold_right(invocation),
        ].into_iter().collect())
    ]);
    
    Ok(stream)
}

fn punct(punct: char) -> TokenTree {
    TokenTree::Punct(Punct::new(punct, Spacing::Alone))
}
    
fn group(delimiter: Delimiter, stream: TokenStream) -> TokenTree {
    TokenTree::Group(Group::new(delimiter, stream))
}

fn fold_into(delimiter: Delimiter) -> TokenStream {
    group(delimiter, TokenStream::new()).into()
}

fn fold_left(stream: TokenStream) -> TokenStream {
    TokenStream::from_iter([
        punct('<'),
        group(Delimiter::Bracket, stream),
    ])
}

fn fold_right(stream: TokenStream) -> TokenStream {
    TokenStream::from_iter([
        punct('>'),
        group(Delimiter::Bracket, stream),
    ])
}

fn long_left_arrow() -> TokenStream {
    TokenStream::from_iter([punct('<'), punct('-'), punct('-')])
}
