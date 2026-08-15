use std::{
    ffi::CString,
    num::ParseIntError,
};

use proc_macro::{Ident, Literal, Span};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum StringType {
    String,
    ByteString,
    CString,
}

impl StringType {
    pub fn make_literal(self, string: &str) -> Literal {
        match self {
            StringType::String => Literal::string(string),
            StringType::ByteString => Literal::byte_string(string.as_bytes()),
            StringType::CString => Literal::c_string(&CString::new(string).unwrap()),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum LitKind {
    Char,
    String,
    RawString(u8),
    ByteChar,
    ByteString,
    RawByteString(u8),
    CString,
    RawCString(u8),
    Number,
    Unknown,
}

impl LitKind {
    pub fn of(lit: &Literal) -> Self {
        let s = lit.to_string();
        let mut s = s.chars();
        let Some(c) = s.next() else { return Self::Unknown };
        match c {
            '\'' => Self::Char,
            '"' => Self::String,
            'r' => Self::RawString(s.take_while(|&c| c == '#').count() as _),
            'b' => {
                let Some(c) = s.next() else { return Self::Unknown };
                match c {
                    '\'' => Self::ByteChar,
                    '"' => Self::ByteString,
                    'r' => Self::RawByteString(s.take_while(|&c| c == '#').count() as _),
                    _ => Self::Unknown,
                }
            },
            'c' => {
                let Some(c) = s.next() else { return Self::Unknown };
                match c {
                    '"' => Self::CString,
                    'r' => Self::RawCString(s.take_while(|&c| c == '#').count() as _),
                    _ => Self::Unknown,
                }
            }
            '0'..='9' => Self::Number,
            _ => Self::Unknown,
        }
    }

    pub fn string_type(self) -> Option<StringType> {
        Some(match self {
            LitKind::String | LitKind::RawString(_) => StringType::String,
            LitKind::ByteString | LitKind::RawByteString(_) => StringType::ByteString,
            LitKind::CString | LitKind::RawCString(_) => StringType::CString,
            _ => return None
        })
    }

    pub fn is_raw(self) -> bool {
        matches!(self, LitKind::RawString(_) | LitKind::RawCString(_) | LitKind::RawByteString(_))
    }
}

pub fn parse_string(lit: &str) -> (&str, &str) {
    let i = lit.bytes().enumerate().find(|&(_, c)| c == b'"').unwrap().0;
    let j = lit.bytes().enumerate().rev().find(|&(_, c)| c == b'"').unwrap().0;
    let k = lit.bytes().enumerate().rev().find(|&(_, c)| matches!(c, b'"' | b'#')).unwrap().0;
    (&lit[i + 1..j], &lit[k + 1..])
}

pub fn unescape(str: &str) -> String {
    let mut chars = str.chars();
    let mut result = String::new();
    while let Some(char) = chars.next() {
        if char != '\\' {
            result.push(char);
            continue;
        }
        result.push(match chars.next().unwrap() {
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            '0' => '\0',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'x' => {
                u8::from_str_radix(&chars.by_ref().take(2).collect::<String>(), 0x10).unwrap().into()
            }
            'u' => {
                chars.next();
                u32::from_str_radix(&chars.by_ref().take_while(|&c| c != '}').collect::<String>(), 0x10).unwrap().try_into().unwrap()
            }
            _ => panic!(),
        })
    }
    result
}

pub fn parse_int(lit: &str) -> Result<u64, ParseIntError> {
    u64::from_str_radix(
        lit, 
        match lit.get(0..2) {
            Some("0x") => 16,
            Some("0b") => 2,
            Some("0o") => 8,
            _ => 10,
        },
    )
}

pub fn make_ident(string: &str, span: Span) -> Ident {
    match string {
        "as" | "async" | "await" | "break" | "const" | "continue" |
        "dyn" | "else" | "enum" | "extern" | "false" | "fn" | "for" |
        "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move" |
        "mut" | "pub" | "ref" | "return" | "static" | "struct" | "trait" |
        "true" | "type" | "unsafe" | "use" | "where" | "while" => Ident::new_raw(string, span),
        _ => Ident::new(string, span),
    }
}
