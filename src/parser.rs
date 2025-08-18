/*
Program = { Stmt } ;

Stmt = StmtFn | StmtLet | StmtExpr ;

StmtFn = "fn" Identifier "(" [ ParamList ] ")" Block ;
ParamList = Identifier { "," Identifier } ;

StmtLet = "let" Identifier "=" Expr ";" ;

StmtExpr = Expr ";" ;

Expr = ExprIf | ExprOr ;

ExprIf = "if" Expr Block "else" Block ;

ExprOr = ExprAnd { "||" ExprAnd } ;
ExprAnd = ExprEq { "&&" ExprEq } ;
ExprEq = ExprOrd { ( "==" | "!=" ) ExprOrd } ;
ExprOrd = ExprAdd { ( "<" | "<=" | ">" | ">=" ) ExprAdd } ;
ExprAdd = ExprMul { ( "+" | "-" ) ExprMul } ;
ExprMul = ExprUnary { ( "*" | "/" | "%" ) ExprUnary } ;
ExprUnary = [ "-" | "!" ] ExprPostfix ;
ExprPostfix = ExprPrimary { ExprCall } ;
ExprCall = "(" [ ArgList ] ")" ;
ArgList = Expr { "," Expr } ;

ExprPrimary = INTEGER
        | STRING
        | IDENTIFIER
        | Block
        | "(" Expr ")"
        ;

Block = "{" { Stmt } [ Expr ] "}" ;
*/

use crate::ast::*;
use crate::error::{Result, error};
use crate::token::{Token, TokenKind, lex};

// Refer to tokens directly
#[allow(unused_imports)]
use crate::token::TokenKind::*;

pub fn parse(code: &str) -> Result<Program> {
    let c = &mut Cursor::new(code);

    parse_program(c)
}

fn parse_program(c: &mut Cursor) -> Result<Program> {
    let mut body = Vec::new();
    while !c.at(TOK_EOF) {
        body.push(parse_stmt(c)?);
    }
    let trailing_semi = c.was(TOK_SEMI);

    let tail = if !trailing_semi
        && let Some(tail) = body.last()
        && let Stmt::Expr(_) = tail
    {
        let tail = match body.pop().unwrap() {
            Stmt::Expr(tail) => *tail,
            _ => unreachable!(),
        };
        Some(tail)
    } else {
        None
    };

    Ok(Program { body, tail })
}

fn parse_stmt(c: &mut Cursor) -> Result<Stmt> {
    match c.kind() {
        KW_FN => parse_stmt_fn(c),
        KW_LET => parse_stmt_let(c),
        _ => parse_stmt_expr(c),
    }
}

fn parse_stmt_fn(c: &mut Cursor) -> Result<Stmt> {
    assert!(c.eat(KW_FN));

    let name = parse_ident(c)?;
    let params = parse_param_list(c)?;
    let body = parse_block(c)?;

    Ok(Stmt::Fn(Box::new(StmtFn { name, params, body })))
}

fn parse_param_list(c: &mut Cursor) -> Result<Vec<String>> {
    parse_paren_list(c, parse_ident)
}

fn parse_paren_list<F, T>(c: &mut Cursor, mut elem: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Cursor) -> Result<T>,
{
    let mut list = Vec::new();

    c.must(TOK_LPAREN)?;
    if !c.at(TOK_RPAREN) {
        loop {
            list.push(elem(c)?);
            // stop if there's no comma, or if the comma is trailing.
            if !c.eat(TOK_COMMA) || c.at(TOK_RPAREN) {
                break;
            }
        }
    }
    c.must(TOK_RPAREN)?;

    Ok(list)
}

fn parse_stmt_let(c: &mut Cursor) -> Result<Stmt> {
    assert!(c.eat(KW_LET));

    let name = parse_ident(c)?;
    c.must(OP_EQ)?;
    let value = parse_expr(c)?;
    c.must(TOK_SEMI)?;

    Ok(Stmt::Let(Box::new(StmtLet { name, value })))
}

fn parse_stmt_expr(c: &mut Cursor) -> Result<Stmt> {
    let expr = parse_expr(c)?;

    let semi = c.eat(TOK_SEMI);
    if !semi && !c.at(TOK_EOF) && !c.at(TOK_RBRACE) {
        // this isn't a trailing expression,
        // so the semicolon is not optional.
        return error(
            format!("expected a semicolon, got {:?}", c.kind()),
            c.current().span,
        )
        .into();
    }

    Ok(Stmt::Expr(Box::new(expr)))
}

fn parse_expr(c: &mut Cursor) -> Result<Expr> {
    match c.kind() {
        KW_IF => parse_expr_if(c),
        _ => parse_expr_or(c),
    }
}

fn parse_expr_if(c: &mut Cursor) -> Result<Expr> {
    assert!(c.eat(KW_IF));

    let mut branches = vec![parse_if_branch(c)?];
    let mut tail = None;
    while c.eat(KW_ELSE) {
        if c.eat(KW_IF) {
            branches.push(parse_if_branch(c)?);
        } else {
            tail = Some(parse_block(c)?);
            break;
        }
    }

    Ok(Expr::If(Box::new(ExprIf { branches, tail })))
}

fn parse_if_branch(c: &mut Cursor) -> Result<IfBranch> {
    let cond = parse_expr(c)?;
    let body = parse_block(c)?;

    Ok(IfBranch { cond, body })
}

fn parse_expr_or(c: &mut Cursor) -> Result<Expr> {
    let mut lhs = parse_expr_and(c)?;
    while c.eat(OP_OR) {
        let op = BinaryOp::Or;
        let rhs = parse_expr_and(c)?;
        lhs = Expr::Binary(Box::new(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_and(c: &mut Cursor) -> Result<Expr> {
    let mut lhs = parse_expr_eq(c)?;
    while c.eat(OP_AND) {
        let op = BinaryOp::And;
        let rhs = parse_expr_eq(c)?;
        lhs = Expr::Binary(Box::new(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_eq(c: &mut Cursor) -> Result<Expr> {
    let mut lhs = parse_expr_ord(c)?;
    loop {
        let op = match c.kind() {
            OP_EQEQ => BinaryOp::Equal,
            OP_NEQ => BinaryOp::NotEqual,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_ord(c)?;
        lhs = Expr::Binary(Box::new(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_ord(c: &mut Cursor) -> Result<Expr> {
    let mut lhs = parse_expr_add(c)?;
    loop {
        let op = match c.kind() {
            OP_LT => BinaryOp::LessThan,
            OP_LE => BinaryOp::LessOrEqual,
            OP_GT => BinaryOp::GreaterThan,
            OP_GE => BinaryOp::GreaterOrEqual,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_add(c)?;
        lhs = Expr::Binary(Box::new(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_add(c: &mut Cursor) -> Result<Expr> {
    let mut lhs = parse_expr_mul(c)?;
    loop {
        let op = match c.kind() {
            OP_PLUS => BinaryOp::Add,
            OP_MINUS => BinaryOp::Subtract,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_mul(c)?;
        lhs = Expr::Binary(Box::new(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_mul(c: &mut Cursor) -> Result<Expr> {
    let mut lhs = parse_expr_unary(c)?;
    loop {
        let op = match c.kind() {
            OP_STAR => BinaryOp::Multiply,
            OP_SLASH => BinaryOp::Divide,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_unary(c)?;
        lhs = Expr::Binary(Box::new(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_unary(c: &mut Cursor) -> Result<Expr> {
    let op = match c.kind() {
        OP_MINUS => UnaryOp::Minus,
        OP_BANG => UnaryOp::Not,
        _ => return parse_expr_postfix(c),
    };
    c.advance(); // eat `op`
    let rhs = parse_expr_unary(c)?;
    Ok(Expr::Unary(Box::new(ExprUnary { op, rhs })))
}

fn parse_expr_postfix(c: &mut Cursor) -> Result<Expr> {
    let mut expr = parse_expr_primary(c)?;
    while c.at(TOK_LPAREN) {
        expr = parse_expr_call(c, expr)?;
    }

    Ok(expr)
}

fn parse_expr_call(c: &mut Cursor, callee: Expr) -> Result<Expr> {
    let args = parse_arg_list(c)?;

    Ok(Expr::Call(Box::new(ExprCall { callee, args })))
}

fn parse_arg_list(c: &mut Cursor) -> Result<Vec<Expr>> {
    let args = parse_paren_list(c, parse_expr)?;
    Ok(args)
}

fn parse_expr_primary(c: &mut Cursor) -> Result<Expr> {
    match c.kind() {
        LIT_INT => parse_expr_int(c),
        LIT_STR => parse_expr_str(c),
        LIT_IDENT => parse_expr_ident(c),
        TOK_LBRACE => parse_expr_block(c),
        TOK_LPAREN => parse_expr_group(c),
        _ => error(
            format!("unexpected token: {:?}", c.kind()),
            c.current().span,
        )
        .into(),
    }
}

fn parse_expr_int(c: &mut Cursor) -> Result<Expr> {
    let token = c.must(LIT_INT)?;
    let lexeme = c.lexeme(token);
    if lexeme.starts_with('0') && lexeme.len() > 1 {
        return error(format!("invalid integer"), token.span).into();
    }
    let value: i64 = lexeme
        .parse()
        .map_err(|err| error(format!("failed to parse integer: {err}"), token.span))?;
    Ok(Expr::Int(Box::new(ExprInt { value })))
}

fn parse_expr_str(c: &mut Cursor) -> Result<Expr> {
    let token = c.must(LIT_STR)?;
    let value = c
        .lexeme(token)
        .strip_prefix('"')
        .expect("string was lexed without opening quote")
        .strip_suffix('"')
        .expect("string was lexed without closing quote")
        .to_owned();
    Ok(Expr::Str(Box::new(ExprStr { value })))
}

fn parse_expr_ident(c: &mut Cursor) -> Result<Expr> {
    let token = c.must(LIT_IDENT)?;
    let name = c.lexeme(token).to_owned();
    Ok(Expr::Ident(Box::new(ExprIdent { name })))
}

fn parse_expr_block(c: &mut Cursor) -> Result<Expr> {
    let inner = parse_block(c)?;
    Ok(Expr::Block(Box::new(ExprBlock { inner })))
}

fn parse_expr_group(c: &mut Cursor) -> Result<Expr> {
    assert!(c.eat(TOK_LPAREN));
    let inner = parse_expr(c)?;
    c.must(TOK_RPAREN)?;

    Ok(inner)
}

fn parse_block(c: &mut Cursor) -> Result<Block> {
    c.must(TOK_LBRACE)?;
    let mut body = Vec::new();
    while !c.at(TOK_RBRACE) {
        body.push(parse_stmt(c)?);
    }
    let trailing_semi = c.was(TOK_SEMI);
    c.must(TOK_RBRACE)?;

    let tail = if !trailing_semi
        && let Some(tail) = body.last()
        && let Stmt::Expr(_) = tail
    {
        let tail = match body.pop().unwrap() {
            Stmt::Expr(tail) => *tail,
            _ => unreachable!(),
        };
        Some(tail)
    } else {
        None
    };

    Ok(Block { body, tail })
}

fn parse_ident(c: &mut Cursor) -> Result<String> {
    let token = c.must(LIT_IDENT)?;
    Ok(c.lexeme(token).to_owned())
}

/// Keeps track of where we are in the source code.
struct Cursor<'src> {
    code: &'src str,
    tokens: Vec<Token>,
    position: usize,
}

impl<'src> Cursor<'src> {
    #[inline]
    fn new(code: &'src str) -> Self {
        Self {
            code,
            tokens: lex(code),
            position: 0,
        }
    }

    /// Returns the slice of source code which belongs to
    /// the token.
    #[inline]
    fn lexeme(&self, token: Token) -> &'src str {
        &self.code[token.span]
    }

    /// Advance the cursor to the next token.
    #[inline]
    fn advance(&mut self) {
        if self.position + 1 >= self.tokens.len() {
            return;
        }

        self.position += 1;
    }

    /// Returns the token under the cursor.
    #[inline]
    fn current(&self) -> Token {
        self.tokens[self.position]
    }

    /// Returns the token before the cursor.
    #[inline]
    fn previous(&self) -> Token {
        self.tokens[self.position - 1]
    }

    /// Returns the current token kind.
    #[inline]
    fn kind(&self) -> TokenKind {
        self.current().kind
    }

    /// Returns `true` if the current token matches `kind`.
    #[inline]
    fn at(&self, kind: TokenKind) -> bool {
        self.current().kind == kind
    }

    /// Returns `true` if the previous token matched `kind`.
    #[inline]
    fn was(&self, kind: TokenKind) -> bool {
        self.position > 0 && self.previous().kind == kind
    }

    /// Returns `true` and advances if the current token matches `kind`.
    #[inline]
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Returns the current token if it matches `kind`,
    /// otherwise returns an error.
    #[inline]
    fn must(&mut self, kind: TokenKind) -> Result<Token> {
        let current = self.current();
        if self.eat(kind) {
            Ok(current)
        } else {
            error(
                format!("expected {kind:?}, found {:?}", current.kind),
                current.span,
            )
            .into()
        }
    }
}

#[cfg(test)]
mod tests;
