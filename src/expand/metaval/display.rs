use std::fmt::{Display, Formatter, Result};

use crate::expand::metaval::{Metaval, MetavalToken};


impl Display for Metaval {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.0.is_empty() {
            return write!(f, "-empty-");
        }
        for token in &self.0[..self.0.len() - 1] {
            write!(f, "{} ", token)?;
        }
        write!(f, "{}", self.0[self.0.len() - 1])?;
        Ok(())
    }
}

impl Display for MetavalToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let Self { order, token } = self;
        write!(f, "{order}[{token}]")
    }
}
