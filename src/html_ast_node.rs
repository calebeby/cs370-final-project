use crate::ir::*;
use crate::SourceLocation;
use crate::WithSourceLocation;

#[derive(Debug)]
pub struct Attribute<'a> {
    pub source_location: SourceLocation<'a>,
    pub name: &'a str,
    pub val: Option<&'a str>,
}
impl<'a> WithSourceLocation for Attribute<'a> {
    fn source_location(&self) -> &SourceLocation {
        &self.source_location
    }
}
impl<'a> ToIR for Attribute<'a> {
    fn to_ir(&self) -> Option<IR> {
        Some(IR::Group(match self.val {
            Some(val) => vec![
                IR::Text(self.name.to_owned()),
                IR::Text("=".to_owned()),
                IR::Text(format!("\"{}\"", val)),
            ],
            // Attribute does not have a value, just print the name
            None => vec![IR::Text(self.name.to_owned())],
        }))
    }
}

#[derive(Debug)]
pub struct OpenTag<'a> {
    pub source_location: SourceLocation<'a>,
    pub tag_name: &'a str,
    pub attributes: Vec<Attribute<'a>>,
}
impl<'a> WithSourceLocation for OpenTag<'a> {
    fn source_location(&self) -> &SourceLocation {
        &self.source_location
    }
}
impl<'a> ToIR for OpenTag<'a> {
    fn to_ir(&self) -> Option<IR> {
        if self.attributes.is_empty() {
            return Some(IR::Text(format!("<{}>", self.tag_name)));
        }
        let mut ir = vec![IR::Text(format!("<{}", self.tag_name))];
        for attr in &self.attributes {
            let attr_ir = attr.to_ir();
            if let Some(attr_ir) = attr_ir {
                ir.push(IR::Text(" ".to_owned()));
                ir.push(attr_ir);
            }
        }
        ir.push(IR::Text(">".to_owned()));
        Some(IR::Group(ir))
    }
}

#[derive(Debug)]
pub struct CloseTag<'a> {
    pub source_location: SourceLocation<'a>,
    pub tag_name: &'a str,
}
impl<'a> WithSourceLocation for CloseTag<'a> {
    fn source_location(&self) -> &SourceLocation {
        &self.source_location
    }
}
impl<'a> ToIR for CloseTag<'a> {
    fn to_ir(&self) -> Option<IR> {
        Some(IR::Text(format!("</{}>", self.tag_name)))
    }
}

fn push_val_if_some<T>(vector: &mut Vec<T>, val: Option<T>) {
    if let Some(val) = val {
        vector.push(val);
    }
}

#[derive(Debug)]
pub struct Tag<'a, ExpressionType: ToIR> {
    pub open_tag: OpenTag<'a>,
    pub children: Children<'a, ExpressionType>,
    pub close_tag: CloseTag<'a>,
}
impl<'a, ExpressionType: ToIR> ToIR for Tag<'a, ExpressionType> {
    fn to_ir(&self) -> Option<IR> {
        let mut child_irs = vec![];
        push_val_if_some(&mut child_irs, self.open_tag.to_ir());
        push_val_if_some(&mut child_irs, self.children.to_ir());
        push_val_if_some(&mut child_irs, self.close_tag.to_ir());
        Some(IR::Group(child_irs))
    }
}

#[derive(Debug)]
pub struct Text<'a>(pub &'a str);
impl<'a> ToIR for Text<'a> {
    fn to_ir(&self) -> Option<IR> {
        Some(IR::Text(self.0.to_owned()))
    }
}

#[derive(Debug)]
pub enum Child<'a, ExpressionType: ToIR> {
    Tag(Tag<'a, ExpressionType>),
    Text(Text<'a>),
    Whitespace,
    Expression(ExpressionType),
}
impl<'a, ExpressionType: ToIR> ToIR for Child<'a, ExpressionType> {
    fn to_ir(&self) -> Option<IR> {
        match self {
            Child::Text(text) => text.to_ir(),
            Child::Tag(tag) => tag.to_ir(),
            // TODO: conditionally do whitespace or newline
            Child::Whitespace => Some(IR::IfBroken {
                // broken: Box::new(IR::Newline),
                broken: Box::new(IR::Text(" ".to_owned())),
                not_broken: Box::new(IR::Text(" ".to_owned())),
            }),
            Child::Expression(expr) => expr.to_ir(),
        }
    }
}

#[derive(Debug)]
pub struct Children<'a, ExpressionType: ToIR>(pub Vec<Child<'a, ExpressionType>>);
impl<'a, ExpressionType: ToIR> ToIR for Children<'a, ExpressionType> {
    fn to_ir(&self) -> Option<IR> {
        let mut child_irs = Vec::new();
        for child in &self.0 {
            push_val_if_some(&mut child_irs, child.to_ir());
        }
        return Some(IR::Group(child_irs));
    }
}
