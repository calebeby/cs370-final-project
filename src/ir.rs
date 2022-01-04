// "Intermediate Representation"
// This is used for determining when to break things onto multiple lines
// Idea is from Prettier: https://prettier.io/docs/en/technical-details.html
// More details: https://github.com/prettier/prettier/blob/master/commands.md

pub trait ToIR {
    fn to_ir(&self) -> Option<IR>;
}

#[derive(Debug)]
pub enum IR {
    Text(String),
    Group(Vec<IR>),
    /// A group where all the children break, or all the children don't break
    BreakGroup(Vec<IR>),
    /// A line break that is always included in the output, no matter if the expression fits on one line or not.
    HardLine,
    IfBroken {
        broken: Box<IR>,
        not_broken: Box<IR>,
    },
    /// Tokens who have a higher indentation level
    Indent(Vec<IR>),
}

impl IR {
    pub fn to_string(&self, group_broken: bool) -> String {
        match self {
            IR::Text(text) => text.to_owned(),
            IR::Group(children) => {
                let mut output = String::new();
                for child in children {
                    output += &child.to_string(group_broken);
                }
                output
            }
            IR::IfBroken { not_broken, broken } => {
                // to_string generally shouldn't be called directly on this.
                // It needs to have a parent BreakGroup to decide if it should break.
                // So here we will always print the not_broken version
                (if group_broken { broken } else { not_broken }).to_string(group_broken)
            }
            IR::BreakGroup(children) => {
                // TODO: handle conditionally breaking
                let mut output = String::new();
                for child in children {
                    // TODO: this should make a new group_broken variable
                    // (shouldn't cascade through here)
                    output += &child.to_string(group_broken);
                }
                output
            }
            IR::HardLine => "\n".to_string(),
            IR::Indent(children) => {
                let mut output = String::new();
                for child in children {
                    output += &child.to_string(group_broken);
                }
                output
                    .split("\n")
                    .map(|line| format!("  {}", line))
                    .collect::<Vec<String>>()
                    .join("\n")
            }
        }
    }
}
