use crate::ir::IR;
use crate::HTMLToken;
use crate::LangExpression;
use crate::ToIR;

#[derive(Debug)]
pub enum Expression {}

impl LangExpression for Expression {
    fn parse_expression<'a>(
        _tokens: &'a [HTMLToken<'a>],
    ) -> Result<Option<(Self, &'a [HTMLToken<'a>])>, String> {
        Ok(None)
    }
}

impl ToIR for Expression {
    fn to_ir(&self) -> Option<IR> {
        None
    }
}
