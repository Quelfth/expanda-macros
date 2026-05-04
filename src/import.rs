use std::collections::VecDeque;

use quote::quote;

use proc_macro2::{
    TokenStream,
    TokenTree,
};

pub fn using(args: TokenStream, item: TokenStream) -> Result<TokenStream, TokenStream> {
    let item = item.into_iter().collect::<Vec<_>>();
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
        return Err(quote!{ compile_error!("using must be placed on an `expand` invocation") }.into());
    }
    let TokenTree::Group(ref group) = item[item.len() - 1] else {return Err(quote!{ compile_error!("using must be placed on an `expand` invocation") }.into())};
    if args.is_empty() {
        return Ok(item.into_iter().collect());
    }
    let contents= group.stream();

    let mut args = args.into_iter().collect::<VecDeque<_>>();
    let is_list = if let TokenTree::Punct(ref p) = args[0] && p.as_char() == '*' {
        args.pop_front();
        true
    } else {
        false
    };
    let name = args.back().unwrap().clone();
    let args = args.into_iter().collect::<TokenStream>();

    let first_part = if is_list {
        quote!{() >[<--let $*#name =]}
    } else {
        quote!{[] >[<--let $#name =]}
    };
    let invocation = item[0..item.len()-1].iter().cloned().collect::<TokenStream>();

    Ok(quote!{
        #args!{
            #first_part
            <[#contents]
            {}
            >[#invocation]
        }
    })
}