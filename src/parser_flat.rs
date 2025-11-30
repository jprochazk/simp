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

use crate::ast_flat::*;
use crate::error::{Result, error};
use crate::token::{Token, TokenKind, lex};

// Refer to tokens directly
#[allow(unused_imports)]
use crate::token::TokenKind::*;

pub fn parse(code: &str) -> Result<Ast> {
    let mut ast = Ast::builder();
    let c = &mut Cursor::new(code);

    let program = parse_program(c, &mut ast)?;

    Ok(ast.finish(program))
}

fn parse_program(c: &mut Cursor, ast: &mut AstBuilder) -> Result<Program> {
    let mut body = Vec::new();
    while !c.at(TOK_EOF) {
        body.push(parse_stmt(c, ast)?);
    }
    let trailing_semi = c.was(TOK_SEMI);

    let tail = if !trailing_semi
        && let Some(tail) = body.last()
        && let Stmt::Expr(tail) = ast.stmt(*tail)
    {
        body.pop().unwrap();
        Some(*tail)
    } else {
        None
    };

    Ok(Program { body, tail })
}

fn parse_stmt(c: &mut Cursor, ast: &mut AstBuilder) -> Result<StmtId> {
    match c.kind() {
        KW_FN => parse_stmt_fn(c, ast),
        KW_LET => parse_stmt_let(c, ast),
        _ => parse_stmt_expr(c, ast),
    }
}

fn parse_stmt_fn(c: &mut Cursor, ast: &mut AstBuilder) -> Result<StmtId> {
    assert!(c.eat(KW_FN));

    let name = parse_ident(c, ast)?;
    let params = parse_param_list(c, ast)?;
    let body = parse_expr_block(c, ast)?;

    Ok(ast.alloc_stmt(Stmt::Fn(StmtFn { name, params, body })))
}

fn parse_param_list(c: &mut Cursor, ast: &mut AstBuilder) -> Result<Vec<IdentId>> {
    parse_paren_list(c, ast, parse_ident)
}

fn parse_paren_list<F, T>(c: &mut Cursor, ast: &mut AstBuilder, mut elem: F) -> Result<Vec<T>>
where
    F: FnMut(&mut Cursor, &mut AstBuilder) -> Result<T>,
{
    let mut list = Vec::new();

    c.must(TOK_LPAREN)?;
    if !c.at(TOK_RPAREN) {
        loop {
            list.push(elem(c, ast)?);
            // stop if there's no comma, or if the comma is trailing.
            if !c.eat(TOK_COMMA) || c.at(TOK_RPAREN) {
                break;
            }
        }
    }
    c.must(TOK_RPAREN)?;

    Ok(list)
}

fn parse_stmt_let(c: &mut Cursor, ast: &mut AstBuilder) -> Result<StmtId> {
    assert!(c.eat(KW_LET));

    let name = parse_ident(c, ast)?;
    c.must(OP_EQ)?;
    let value = parse_expr(c, ast)?;
    c.must(TOK_SEMI)?;

    Ok(ast.alloc_stmt(Stmt::Let(StmtLet { name, value })))
}

fn parse_stmt_expr(c: &mut Cursor, ast: &mut AstBuilder) -> Result<StmtId> {
    let expr = parse_expr(c, ast)?;

    // attempt to consume a semicolon
    let semi = c.eat(TOK_SEMI);
    if !semi {
        if c.was(TOK_RBRACE) {
            // blocks don't require semicolons
        } else if !c.at(TOK_EOF) && !c.at(TOK_RBRACE) {
            // this isn't a trailing expression,
            // so the semicolon is not optional.
            return error(
                format!("expected a semicolon, got {:?}", c.kind()),
                c.current().span,
            )
            .into();
        }
    }

    Ok(ast.alloc_stmt(Stmt::Expr(expr)))
}

fn parse_expr(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    match c.kind() {
        KW_IF => parse_expr_if(c, ast),
        _ => parse_expr_or(c, ast),
    }
}

fn parse_expr_if(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    assert!(c.eat(KW_IF));

    let mut branches = vec![parse_if_branch(c, ast)?];
    let mut tail = None;
    while c.eat(KW_ELSE) {
        if c.eat(KW_IF) {
            branches.push(parse_if_branch(c, ast)?);
        } else {
            tail = Some(parse_expr_block(c, ast)?);
            break;
        }
    }

    Ok(ast.alloc_expr(Expr::If(ExprIf { branches, tail })))
}

fn parse_if_branch(c: &mut Cursor, ast: &mut AstBuilder) -> Result<IfBranch> {
    let cond = parse_expr(c, ast)?;
    let body = parse_expr_block(c, ast)?;

    Ok(IfBranch { cond, body })
}

fn parse_expr_or(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let mut lhs = parse_expr_and(c, ast)?;
    while c.eat(OP_OR) {
        let op = BinaryOp::Or;
        let rhs = parse_expr_and(c, ast)?;
        lhs = ast.alloc_expr(Expr::Binary(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_and(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let mut lhs = parse_expr_eq(c, ast)?;
    while c.eat(OP_AND) {
        let op = BinaryOp::And;
        let rhs = parse_expr_eq(c, ast)?;
        lhs = ast.alloc_expr(Expr::Binary(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_eq(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let mut lhs = parse_expr_ord(c, ast)?;
    loop {
        let op = match c.kind() {
            OP_EQEQ => BinaryOp::Equal,
            OP_NEQ => BinaryOp::NotEqual,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_ord(c, ast)?;
        lhs = ast.alloc_expr(Expr::Binary(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_ord(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let mut lhs = parse_expr_add(c, ast)?;
    loop {
        let op = match c.kind() {
            OP_LT => BinaryOp::LessThan,
            OP_LE => BinaryOp::LessOrEqual,
            OP_GT => BinaryOp::GreaterThan,
            OP_GE => BinaryOp::GreaterOrEqual,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_add(c, ast)?;
        lhs = ast.alloc_expr(Expr::Binary(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_add(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let mut lhs = parse_expr_mul(c, ast)?;
    loop {
        let op = match c.kind() {
            OP_PLUS => BinaryOp::Add,
            OP_MINUS => BinaryOp::Subtract,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_mul(c, ast)?;
        lhs = ast.alloc_expr(Expr::Binary(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_mul(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let mut lhs = parse_expr_unary(c, ast)?;
    loop {
        let op = match c.kind() {
            OP_STAR => BinaryOp::Multiply,
            OP_SLASH => BinaryOp::Divide,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_unary(c, ast)?;
        lhs = ast.alloc_expr(Expr::Binary(ExprBinary { lhs, op, rhs }))
    }

    Ok(lhs)
}

fn parse_expr_unary(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let op = match c.kind() {
        OP_MINUS => UnaryOp::Minus,
        OP_BANG => UnaryOp::Not,
        _ => return parse_expr_postfix(c, ast),
    };
    c.advance(); // eat `op`
    let rhs = parse_expr_unary(c, ast)?;
    Ok(ast.alloc_expr(Expr::Unary(ExprUnary { op, rhs })))
}

fn parse_expr_postfix(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let mut expr = parse_expr_primary(c, ast)?;
    while c.at(TOK_LPAREN) {
        expr = parse_expr_call(c, ast, expr)?;
    }

    Ok(expr)
}

fn parse_expr_call(c: &mut Cursor, ast: &mut AstBuilder, callee: ExprId) -> Result<ExprId> {
    let args = parse_arg_list(c, ast)?;

    Ok(ast.alloc_expr(Expr::Call(ExprCall { callee, args })))
}

fn parse_arg_list(c: &mut Cursor, ast: &mut AstBuilder) -> Result<Vec<ExprId>> {
    let args = parse_paren_list(c, ast, parse_expr)?;
    Ok(args)
}

fn parse_expr_primary(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    match c.kind() {
        LIT_INT => parse_expr_int(c, ast),
        LIT_STR => parse_expr_str(c, ast),
        LIT_IDENT => parse_expr_ident(c, ast),
        TOK_LBRACE => parse_expr_block(c, ast),
        TOK_LPAREN => parse_expr_group(c, ast),
        _ => error(
            format!("unexpected token: {:?}", c.kind()),
            c.current().span,
        )
        .into(),
    }
}

fn parse_expr_int(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let token = c.must(LIT_INT)?;
    let lexeme = c.lexeme(token);
    if lexeme.starts_with('0') && lexeme.len() > 1 {
        return error("invalid integer", token.span).into();
    }
    let value: i64 = lexeme
        .parse()
        .map_err(|err| error(format!("failed to parse integer: {err}"), token.span))?;
    Ok(ast.alloc_expr(Expr::Int(ExprInt { value })))
}

fn parse_expr_str(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let token = c.must(LIT_STR)?;
    let value = c
        .lexeme(token)
        .strip_prefix('"')
        .expect("string was lexed without opening quote")
        .strip_suffix('"')
        .expect("string was lexed without closing quote");
    let id = ast.intern_str(value);
    Ok(ast.alloc_expr(Expr::Str(ExprStr { value: id })))
}

fn parse_expr_ident(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    let token = c.must(LIT_IDENT)?;
    let name = c.lexeme(token);
    let id = ast.intern_ident(name);
    Ok(ast.alloc_expr(Expr::Ident(ExprIdent { name: id })))
}

fn parse_expr_block(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    c.must(TOK_LBRACE)?;
    let mut body = Vec::new();
    while !c.at(TOK_RBRACE) {
        body.push(parse_stmt(c, ast)?);
    }
    let trailing_semi = c.was(TOK_SEMI);
    c.must(TOK_RBRACE)?;

    let tail = if !trailing_semi
        && let Some(tail) = body.last()
        && let Stmt::Expr(tail) = ast.stmt(*tail)
    {
        body.pop().unwrap();
        Some(*tail)
    } else {
        None
    };

    Ok(ast.alloc_expr(Expr::Block(ExprBlock { body, tail })))
}

fn parse_expr_group(c: &mut Cursor, ast: &mut AstBuilder) -> Result<ExprId> {
    assert!(c.eat(TOK_LPAREN));
    let inner = parse_expr(c, ast)?;
    c.must(TOK_RPAREN)?;

    Ok(inner)
}

fn parse_ident(c: &mut Cursor, ast: &mut AstBuilder) -> Result<IdentId> {
    let token = c.must(LIT_IDENT)?;
    let name = c.lexeme(token);
    let id = ast.intern_ident(name);
    Ok(id)
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
