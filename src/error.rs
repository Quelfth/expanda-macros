
macro_rules! error {
    ($token:tt, $msg:literal) => {
        quote::quote_spanned!($token.span().into() => compile_error!($msg)).into()
    }
}
pub(crate) use error;

macro_rules! err {
    ($token:tt, $msg:literal) => {
        Err(crate::error::error!($token, $msg))
    }
}
pub(crate) use err;
