use crate::eat_whitespace;
use crate::ir::IR;
use crate::HTMLToken;
use crate::LangExpression;
use crate::MessageType;
use crate::SourceLocation;
use crate::ToIR;

#[derive(Debug)]
pub enum Expression {
    TwigTag(),
}

impl LangExpression for Expression {
    fn parse_expression<'a>(
        tokens: &'a [HTMLToken<'a>],
    ) -> Result<Option<(Self, &'a [HTMLToken<'a>])>, String> {
        match tokens {
            [HTMLToken::OpenCurly(open_curly_location), HTMLToken::Percent(percent_location), remaining_tokens @ ..] => {
                match eat_whitespace(remaining_tokens) {
                    [HTMLToken::Text(twig_tag_name_location), remaining_tokens @ ..] => {
                        let mut rremaining_tokens = remaining_tokens;
                        loop {
                            match rremaining_tokens {
                                [HTMLToken::Percent(_), HTMLToken::CloseCurly(_), ..] => {
                                    return Ok(None)
                                }
                                [_, inner_remaining_tokens @ ..] => {
                                    rremaining_tokens = inner_remaining_tokens;
                                }
                                [] => {
                                    return Err(format!(
                                        "Expected %}} to end Twig tag `{}`\n{}",
                                        twig_tag_name_location.text(),
                                        SourceLocation {
                                            start_index: open_curly_location.start_index,
                                            end_index: twig_tag_name_location.end_index,
                                            source_entire_text: twig_tag_name_location
                                                .source_entire_text
                                        }
                                        .code_frame("Twig tag began here", MessageType::Info)
                                    ));
                                }
                            };
                        }
                    }
                    _ => Err(format!(
                        "Expected Twig tag name after {{%\n{}",
                        SourceLocation {
                            start_index: open_curly_location.start_index,
                            end_index: percent_location.end_index,
                            source_entire_text: percent_location.source_entire_text,
                        }
                        .code_frame(
                            "Expected Twig tag name (like if, for, etc.) after this",
                            MessageType::Error
                        )
                    )),
                }
            }
            _ => Ok(None),
        }
    }
}

impl ToIR for Expression {
    fn to_ir(&self) -> Option<IR> {
        match self {
            Expression::TwigTag() => Some(IR::Text("!!".to_string())),
        }
    }
}
