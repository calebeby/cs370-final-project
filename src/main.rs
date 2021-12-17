use crate::ir::ToIR;
mod html_ast_node;
mod ir;
use colored::*;
use std::fmt;
use std::fs;

pub struct SourceLocation<'a> {
    source_entire_text: &'a str,
    /// Character index
    start_index: usize,
    /// Character index
    end_index: usize,
}

enum MessageType {
    Info,
    Error,
}

/// Counts the number of lines. Better than text.lines().count()
/// because it does not ignore trailing newlines
/// Also it handles the empty-string case,
/// where there shold be one line but text.lines().count() says there are zero
fn num_lines(text: &str) -> usize {
    let result = text.lines().count() + if text.ends_with('\n') { 1 } else { 0 };
    if result == 0 {
        1
    } else {
        result
    }
}

impl<'a> SourceLocation<'a> {
    fn text(&self) -> &str {
        &self.source_entire_text[self.start_index..self.end_index]
    }
    fn code_frame(&self, message: &str, message_type: MessageType) -> String {
        let color = |text: &str| match message_type {
            MessageType::Info => text.blue(),
            MessageType::Error => text.red(),
        };
        let lines: Vec<&str> = self.source_entire_text.lines().collect();
        let text_before_source_location = &self.source_entire_text[0..self.start_index];
        // The line number where the SourceLocation starts
        let start_line_number = num_lines(text_before_source_location) - 1;
        // The line number where the SourceLocation ends
        let end_line_number = start_line_number + num_lines(self.text()) - 1;
        // The character index _in the first line_ where the SourceLocation starts
        let start_char_index_in_line = match text_before_source_location.rfind('\n') {
            None => text_before_source_location.len(),
            Some(l) => text_before_source_location.len() - l - 1,
        };

        // Display a couple lines of "context" before and after actual SourceLocation
        let context_start_line = if start_line_number > 2 {
            start_line_number - 2
        } else {
            0
        };
        let context_end_line = std::cmp::min(end_line_number + 3, lines.len());

        let visible_lines = &lines[context_start_line..context_end_line];
        let mut output = "".to_owned();
        // The + 1 is because human-readable line numbers are 1-indexed
        // but array-based line numbers are 0-indexed
        let line_num_digits = (context_end_line + 1).to_string().len();
        for (i, line) in visible_lines.iter().enumerate() {
            let line_num = i + context_start_line;
            let line_num_str = (line_num + 1).to_string();
            let line_num_str_len = line_num_str.len();
            let line_num_padded = line_num_str + &" ".repeat(line_num_digits - line_num_str_len);
            if line_num < start_line_number || line_num > end_line_number {
                // Context lines
                output += &format!("{} | {}\n", line_num_padded, line);
            } else {
                // A line with (part of) the SourceLocation in it
                output += &format!("{} | {}\n", &color(&line_num_padded), line);
                if line_num == start_line_number && start_line_number == end_line_number {
                    output += &format!(
                        "{}{}\n",
                        " ".repeat(line_num_digits + 3 + start_char_index_in_line),
                        &color(&"^".repeat(self.end_index - self.start_index))
                    );
                }
                if line_num == start_line_number {
                    if start_line_number != end_line_number {
                        output += &format!(
                            "{}{}\n",
                            " ".repeat(line_num_digits + 3 + start_char_index_in_line),
                            &color(&"^")
                        );
                    }
                    output += &format!("{}{}\n", " ".repeat(line_num_digits + 3), &color(message));
                }
            }
        }

        output
    }
}

trait WithSourceLocation {
    fn source_location(&self) -> &SourceLocation;
}

impl fmt::Debug for SourceLocation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SourceLocation ({}:{}): {:?}",
            self.start_index,
            self.end_index,
            self.text()
        )
    }
}

/// Allowed HTML tokens
#[derive(Debug)]
enum HTMLToken<'a> {
    /// <
    StartBracket(SourceLocation<'a>),
    /// >
    EndBracket(SourceLocation<'a>),
    /// </
    ClosingTagStartBracket(SourceLocation<'a>),
    Equals(SourceLocation<'a>),
    Quote(SourceLocation<'a>),
    Whitespace(SourceLocation<'a>),
    Text(SourceLocation<'a>),
}

impl<'a> WithSourceLocation for HTMLToken<'a> {
    fn source_location(&self) -> &SourceLocation {
        match self {
            HTMLToken::StartBracket(source_location)
            | HTMLToken::EndBracket(source_location)
            | HTMLToken::ClosingTagStartBracket(source_location)
            | HTMLToken::Equals(source_location)
            | HTMLToken::Quote(source_location)
            | HTMLToken::Whitespace(source_location)
            | HTMLToken::Text(source_location) => source_location,
        }
    }
}

fn tokenize(text: &str) -> Vec<HTMLToken> {
    let chars: Vec<char> = text.chars().collect();
    let num_chars = chars.len();
    let mut i = 0;
    let mut tokens: Vec<HTMLToken> = Vec::new();
    let create_source_location = |start_index: usize, length: usize| SourceLocation {
        source_entire_text: &text,
        start_index,
        end_index: start_index + length,
    };
    loop {
        let new_token = match chars[i] {
            // Handle </
            '<' if chars[i + 1] == '/' => {
                let new_token = HTMLToken::ClosingTagStartBracket(create_source_location(i, 2));
                i += 1;
                Some(new_token)
            }
            '<' => Some(HTMLToken::StartBracket(create_source_location(i, 1))),

            '>' => Some(HTMLToken::EndBracket(create_source_location(i, 1))),
            '=' => Some(HTMLToken::Equals(create_source_location(i, 1))),
            '"' | '\'' => Some(HTMLToken::Quote(create_source_location(i, 1))),
            ' ' | '\t' | '\n' => {
                match tokens.last_mut() {
                    // If the last token was a whitespace token, add to that existing whitespace
                    Some(HTMLToken::Whitespace(source_location)) => {
                        source_location.end_index = source_location.end_index + 1;
                        None
                    }
                    // Otherwise make a new token
                    _ => Some(HTMLToken::Whitespace(create_source_location(i, 1))),
                }
            }
            _ => match tokens.last_mut() {
                // If the last token was a text token, add to it
                Some(HTMLToken::Text(source_location)) => {
                    source_location.end_index = source_location.end_index + 1;
                    None
                }
                // Otherwise make a new token
                _ => Some(HTMLToken::Text(create_source_location(i, 1))),
            },
        };

        if let Some(new_token) = new_token {
            tokens.push(new_token);
        }

        i += 1;
        if i >= num_chars {
            break;
        };
    }
    tokens
}

fn parse_tag_contents<'a>(
    tokens: &'a [HTMLToken<'a>],
) -> Result<(html_ast_node::Children<'a>, &'a [HTMLToken<'a>]), String> {
    let mut remaining_tokens = tokens;
    let mut children: Vec<html_ast_node::Child> = Vec::new();
    loop {
        match parse_tag(remaining_tokens) {
            Err(err) => return Err(err),
            Ok(Some((tag, inner_remaining_tokens))) => {
                remaining_tokens = inner_remaining_tokens;
                children.push(html_ast_node::Child::Tag(tag));
            }
            Ok(None) => {
                match parse_close_tag(remaining_tokens) {
                    Err(err) => return Err(err),
                    // If it sees a close tag (that wasn't handled by the nested parse_tag)
                    // Then it should end and let the parent function handle the close tag
                    Ok(Some(_)) => break,
                    Ok(None) => {}
                }
                // Did not match a tag, but there was no parsing error
                // So take the text as text
                match &remaining_tokens[0] {
                    HTMLToken::StartBracket(bracket_location) => {
                        return Err(format!(
                            "Unexpected token {}\n\
                             {}",
                            bracket_location.text(),
                            bracket_location.code_frame("Unexpected token", MessageType::Error)
                        ))
                    }
                    HTMLToken::Whitespace(_) => {
                        // Ignore leading whitespace children
                        if !children.is_empty() {
                            children.push(html_ast_node::Child::Whitespace);
                        }
                    }
                    token => children.push(html_ast_node::Child::Text(html_ast_node::Text(
                        token.source_location().text(),
                    ))),
                }
                remaining_tokens = &remaining_tokens[1..];
            }
        }

        if remaining_tokens.len() == 0 {
            break;
        }
    }
    // Remove trailing whitespace children
    while let [.., html_ast_node::Child::Whitespace] = children[..] {
        children.pop();
    }
    Ok((html_ast_node::Children(children), remaining_tokens))
}

fn eat_whitespace<'a>(tokens: &'a [HTMLToken<'a>]) -> &'a [HTMLToken<'a>] {
    let mut remaining_tokens = tokens;
    while let [HTMLToken::Whitespace(_), rest @ ..] = remaining_tokens {
        remaining_tokens = rest;
    }
    remaining_tokens
}

fn parse_attribute_value<'a>(
    tokens: &'a [HTMLToken<'a>],
) -> Result<Option<(&str, usize, &'a [HTMLToken<'a>])>, String> {
    match tokens {
        // Attributes with unquoted attributes
        [HTMLToken::Text(text_source_location), remaining_tokens @ ..] => Ok(Some((
            text_source_location.text(),
            text_source_location.end_index,
            remaining_tokens,
        ))),
        // Attributes with quoted attributes
        [HTMLToken::Quote(start_quote_source_location), remaining_tokens @ ..] => {
            let open_quote_type = start_quote_source_location.text();
            let start_index = start_quote_source_location.start_index + 1;
            let mut remaining_tokens = remaining_tokens;
            while let [token, remaining @ ..] = remaining_tokens {
                remaining_tokens = remaining;
                if let HTMLToken::EndBracket(_) = token {
                    break;
                }
                if let HTMLToken::Quote(end_quote_source_location) = token {
                    if end_quote_source_location.text() == open_quote_type {
                        let end_index = end_quote_source_location.start_index;
                        return Ok(Some((
                            &start_quote_source_location.source_entire_text[start_index..end_index],
                            end_quote_source_location.end_index,
                            remaining_tokens,
                        )));
                    }
                }
            }
            Err(format!(
                "Expected closing {} to end string\n\
                {}",
                open_quote_type,
                start_quote_source_location.code_frame("String began here", MessageType::Info)
            ))
        }
        _ => Ok(None),
    }
}

fn parse_attribute<'a>(
    tokens: &'a [HTMLToken<'a>],
) -> Result<Option<(html_ast_node::Attribute<'a>, &'a [HTMLToken<'a>])>, String> {
    if let [attr_name @ HTMLToken::Text(_), remaining_tokens @ ..] = tokens {
        // This handles attributes with values
        if let [HTMLToken::Equals(equals_source_location), remaining_tokens @ ..] =
            eat_whitespace(remaining_tokens)
        {
            return match parse_attribute_value(eat_whitespace(remaining_tokens)) {
                Err(err) => Err(err),
                Ok(None) => Err(format!(
                    "Expected value for {} attribute\n\
                     {}",
                    attr_name.source_location().text(),
                    equals_source_location.code_frame(
                        "Expected attribute value after equals sign",
                        MessageType::Error
                    ),
                )),
                Ok(Some((value, attr_end_index, remaining_tokens))) => Ok(Some((
                    html_ast_node::Attribute {
                        source_location: SourceLocation {
                            start_index: attr_name.source_location().start_index,
                            end_index: attr_end_index,
                            source_entire_text: attr_name.source_location().source_entire_text,
                        },
                        name: attr_name.source_location().text(),
                        val: Some(value),
                    },
                    remaining_tokens,
                ))),
            };
        } else if matches!(
            remaining_tokens,
            // This handles attributes without values
            [HTMLToken::Whitespace(_) | HTMLToken::EndBracket(_), ..]
        ) {
            return Ok(Some((
                html_ast_node::Attribute {
                    source_location: SourceLocation {
                        start_index: attr_name.source_location().start_index,
                        end_index: attr_name.source_location().end_index,
                        source_entire_text: attr_name.source_location().source_entire_text,
                    },
                    name: attr_name.source_location().text(),
                    val: None,
                },
                remaining_tokens,
            )));
        }
    }
    Ok(None)
}

fn parse_attributes<'a>(
    tokens: &'a [HTMLToken<'a>],
) -> Result<(Vec<html_ast_node::Attribute<'a>>, &'a [HTMLToken<'a>]), String> {
    let mut attributes = Vec::<html_ast_node::Attribute<'a>>::new();
    let mut remaining_tokens = tokens;
    // There should be some whitespace before/between each attribute
    while let [HTMLToken::Whitespace(_), tokens_without_whitespace @ ..] = remaining_tokens {
        remaining_tokens = tokens_without_whitespace;
        match parse_attribute(remaining_tokens) {
            Err(err) => return Err(err),
            Ok(Some((attr, remaining_tokens_after_attr))) => {
                attributes.push(attr);
                remaining_tokens = remaining_tokens_after_attr;
            }
            Ok(None) => break,
        }
    }
    Ok((attributes, remaining_tokens))
}

fn parse_open_tag<'a>(
    tokens: &'a [HTMLToken<'a>],
) -> Result<Option<(html_ast_node::OpenTag<'a>, &'a [HTMLToken<'a>])>, String> {
    match tokens {
        [HTMLToken::StartBracket(SourceLocation {
            start_index,
            source_entire_text,
            ..
        }), HTMLToken::Text(text_source_location), remaining_tokens @ ..] => {
            match parse_attributes(remaining_tokens) {
                Err(err) => Err(err),
                Ok((attributes, remaining_tokens)) => match remaining_tokens {
                    [HTMLToken::EndBracket(SourceLocation { end_index, .. }), remaining_tokens @ ..] =>
                    {
                        Ok(Some((
                            html_ast_node::OpenTag {
                                source_location: SourceLocation {
                                    start_index: *start_index,
                                    end_index: *end_index,
                                    source_entire_text: &source_entire_text,
                                },
                                tag_name: text_source_location.text(),
                                attributes,
                            },
                            // Return the remaining tokens
                            remaining_tokens,
                        )))
                    }
                    [unexpected_token, ..] => Err(format!(
                        "Unexpected token {:?}, expected attribute or > to end opening <{}> tag\n\
                         {}",
                        unexpected_token.source_location().text(),
                        text_source_location.text(),
                        unexpected_token
                            .source_location()
                            .code_frame("Unexpected token", MessageType::Error),
                    )),
                    [] => Err(format!(
                        "Unexpected end of input, expected >\n\
                         {}",
                        text_source_location
                            .code_frame("Expected > after this", MessageType::Error),
                    )),
                },
            }
        }

        _ => Ok(None),
    }
}

fn parse_close_tag<'a>(
    tokens: &'a [HTMLToken<'a>],
) -> Result<Option<(html_ast_node::CloseTag<'a>, &'a [HTMLToken<'a>])>, String> {
    match tokens {
        [HTMLToken::ClosingTagStartBracket(
            closing_tag_start_bracket_source_location
            @
            SourceLocation {
                start_index,
                source_entire_text,
                ..
            },
        ), remaining_tokens @ ..] => match remaining_tokens {
            [HTMLToken::Text(text_source_location), remaining_tokens @ ..] => {
                match eat_whitespace(remaining_tokens) {
                    [HTMLToken::EndBracket(SourceLocation { end_index, .. }), remaining_tokens @ ..] =>
                    {
                        Ok(Some((
                            html_ast_node::CloseTag {
                                source_location: SourceLocation {
                                    start_index: *start_index,
                                    end_index: *end_index,
                                    source_entire_text: &source_entire_text,
                                },
                                tag_name: text_source_location.text(),
                            },
                            // Return the remaining tokens
                            remaining_tokens,
                        )))
                    }
                    [unexpected_token, ..] => Err(format!(
                        "Unexpected token {:?}, expected > to end closing </{}> tag\n\
                         {}",
                        unexpected_token.source_location().text(),
                        text_source_location.text(),
                        unexpected_token
                            .source_location()
                            .code_frame("Unexpected token", MessageType::Error),
                    )),
                    [] => Err(format!(
                        "expected > to end closing </{}> tag\n\
                         {}",
                        text_source_location.text(),
                        text_source_location.code_frame("Expected >", MessageType::Error),
                    )),
                }
            }
            [unexpected_token, ..] => Err(format!(
                "Unexpected token {:?}, expected tag name to follow </ \n\
                 {}",
                unexpected_token.source_location().text(),
                unexpected_token
                    .source_location()
                    .code_frame("Unexpected token", MessageType::Error),
            )),
            [] => Err(format!(
                "expected tag name to follow </ \n\
                 {}",
                closing_tag_start_bracket_source_location
                    .code_frame("Unexpected token", MessageType::Error),
            )),
        },
        _ => Ok(None),
    }
}

fn parse_tag<'a>(
    tokens: &'a [HTMLToken<'a>],
) -> Result<Option<(html_ast_node::Tag<'a>, &'a [HTMLToken<'a>])>, String> {
    let open_tag = parse_open_tag(tokens);

    if let Err(err) = open_tag {
        return Err(err);
    }
    if let Ok(Some((open_tag, remaining_tokens))) = open_tag {
        match parse_tag_contents(remaining_tokens) {
            Err(err) => return Err(err),
            Ok((children, remaining_tokens)) => {
                let close_tag = parse_close_tag(remaining_tokens);
                return match close_tag {
                    Ok(None) => Err(format!(
                        "Expected to find closing tag </{}>\n\
                         {}",
                        open_tag.tag_name,
                        open_tag.source_location().code_frame(
                            &format!("<{}> was opened here", open_tag.tag_name),
                            MessageType::Info
                        ),
                    )),
                    Err(err) => Err(err),
                    Ok(Some((close_tag, remaining_tokens))) => {
                        if open_tag.tag_name != close_tag.tag_name {
                            Err(format!(
                                "Expected to find closing tag </{}>, but found closing tag </{}>\n\
                                 {}\n\
                                 {}",
                                open_tag.tag_name,
                                close_tag.tag_name,
                                open_tag.source_location().code_frame(
                                    &format!("<{}> was opened here", open_tag.tag_name),
                                    MessageType::Info
                                ),
                                close_tag.source_location().code_frame(
                                    &format!(
                                            "While looking for </{}>, found unexpected closing tag here",
                                        open_tag.tag_name
                                      ),
                                    MessageType::Error
                                )
                            ))
                        } else {
                            Ok(Some((
                                html_ast_node::Tag {
                                    open_tag,
                                    children,
                                    close_tag,
                                },
                                remaining_tokens,
                            )))
                        }
                    }
                };
            }
        }
    }
    Ok(None)
}

fn parse<'a>(
    tokens: &'a [HTMLToken<'a>],
) -> Result<html_ast_node::Children<'a>, std::string::String> {
    match parse_tag_contents(tokens) {
        Ok((contents @ html_ast_node::Children(_), remaining_tokens)) => {
            if remaining_tokens.is_empty() {
                return Ok(contents);
            }
            Err(format!(
                "\nRemaining tokens left over after parsing: {:?}",
                remaining_tokens
            ))
        }
        Err(err) => Err(format!("Error occured during parsing:\n{}", err)),
    }
}

fn main() {
    let contents =
        fs::read_to_string("examples/basic.html").expect("Something went wrong reading the file");
    println!("contents:\n{}", contents);

    let tokens = tokenize(&contents);
    let parsed = parse(&tokens);
    match parsed {
        Err(err) => {
            eprintln!("{}", err);
        }
        Ok(parsed) => {
            println!("{:?}", parsed);
            let ir = parsed.to_ir();
            println!("{:?}", ir);
            let output = ir.to_string(true);
            println!("{}", output);
            fs::write("examples/output.html", output)
                .expect("Something went wrong writing the file");
        }
    }
}
