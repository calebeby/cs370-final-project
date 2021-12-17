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
    fn to_ir(&self) -> IR {
        IR::Group(match self.val {
            Some(val) => vec![
                IR::Text(self.name.to_owned()),
                IR::Text("=".to_owned()),
                IR::Text(format!("\"{}\"", val)),
            ],
            None => vec![IR::Text(self.name.to_owned())],
        })
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
    fn to_ir(&self) -> IR {
        if self.attributes.is_empty() {
            return IR::Text(format!("<{}>", self.tag_name));
        }
        let mut ir = vec![IR::Text(format!("<{}", self.tag_name))];
        for attr in &self.attributes {
            ir.push(IR::Text(" ".to_owned()));
            ir.push(attr.to_ir());
        }
        ir.push(IR::Text(">".to_owned()));
        IR::Group(ir)
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
    fn to_ir(&self) -> IR {
        IR::Text(format!("</{}>", self.tag_name))
    }
}

#[derive(Debug)]
pub struct Tag<'a> {
    pub open_tag: OpenTag<'a>,
    pub children: Children<'a>,
    pub close_tag: CloseTag<'a>,
}
impl<'a> ToIR for Tag<'a> {
    fn to_ir(&self) -> IR {
        IR::BreakGroup(vec![
            self.open_tag.to_ir(),
            self.children.to_ir(),
            self.close_tag.to_ir(),
        ])
    }
}

#[derive(Debug)]
pub struct Text<'a>(pub &'a str);
impl<'a> ToIR for Text<'a> {
    fn to_ir(&self) -> IR {
        IR::Text(self.0.to_owned())
    }
}

#[derive(Debug)]
pub enum Child<'a> {
    Tag(Tag<'a>),
    Text(Text<'a>),
    Whitespace,
}
impl<'a> ToIR for Child<'a> {
    fn to_ir(&self) -> IR {
        match self {
            Child::Text(text) => text.to_ir(),
            Child::Tag(tag) => tag.to_ir(),
            // TODO: conditionally do whitespace or newline
            Child::Whitespace => IR::IfBroken {
                // broken: Box::new(IR::Newline),
                broken: Box::new(IR::Text(" ".to_owned())),
                not_broken: Box::new(IR::Text(" ".to_owned())),
            },
        }
    }
}

#[derive(Debug)]
pub struct Children<'a>(pub Vec<Child<'a>>);
impl<'a> ToIR for Children<'a> {
    fn to_ir(&self) -> IR {
        let mut child_irs = Vec::new();
        for child in &self.0 {
            child_irs.push(child.to_ir());
        }
        return IR::Group(child_irs);
    }
}
