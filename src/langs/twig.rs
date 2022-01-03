use crate::ir::IR;
use crate::ToIR;

#[derive(Debug)]
pub enum Expression {}

impl ToIR for Expression {
    fn to_ir(&self) -> Option<IR> {
        None
    }
}
