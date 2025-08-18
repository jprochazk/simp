use crate::span::Span;
use logos::Logos;

#[rustfmt::skip]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
#[logos(skip r"[ \t\n\r]+")]
pub enum TokenKind {
    #[token("fn")] KW_FN,
    #[token("if")] KW_IF,
    #[token("else")] KW_ELSE,
    #[token("let")] KW_LET,

    #[token(";")] TOK_SEMI,
    #[token("(")] TOK_LPAREN,
    #[token(")")] TOK_RPAREN,
    #[token("{")] TOK_LBRACE,
    #[token("}")] TOK_RBRACE,
    #[token(",")] TOK_COMMA,

    #[token("-")] OP_MINUS,
    #[token("+")] OP_PLUS,
    #[token("*")] OP_STAR,
    #[token("/")] OP_SLASH,
    #[token("=")] OP_EQ,
    #[token("||")] OP_OR,
    #[token("&&")] OP_AND,
    #[token("==")] OP_EQEQ,
    #[token("!=")] OP_NEQ,
    #[token("<")] OP_LT,
    #[token("<=")] OP_LE,
    #[token(">")] OP_GT,
    #[token(">=")] OP_GE,
    #[token("!")] OP_BANG,

    #[regex(r"[0-9]+")]
    LIT_INT,

    #[regex(r#""([^"\\]|\\.)*""#)]
    LIT_STR,

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    LIT_IDENT,

    // The following two variants are here to make the parser simpler,
    // ensuring we don't have to explicitly deal with `Option` and `Result`.

    /// Something which was not recognized as a valid token.
    TOK_ERROR,

    /// End of file
    TOK_EOF,
}

use TokenKind::*;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn lexeme<'a>(&self, code: &'a str) -> &'a str {
        &code[self.span]
    }
}

pub fn lex(code: &str) -> Vec<Token> {
    // Logos gives us a `Option<Result<TokenKind>>`, but we don't
    // want to have to deal with that everywhere, so we output
    // special `ERROR` and `EOF` tokens instead.
    TokenKind::lexer(code)
        .spanned()
        .map(|item| match item {
            (Ok(kind), span) => Token {
                kind,
                span: span.into(),
            },
            (Err(()), span) => Token {
                kind: TOK_ERROR,
                span: span.into(),
            },
        })
        .chain([Token {
            kind: TOK_EOF,
            span: (code.len()..code.len()).into(),
        }])
        .collect()
}
