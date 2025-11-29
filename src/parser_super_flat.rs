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

use bumpalo::Bump;
use bumpalo::collections::Vec as Temp;
use bumpalo::vec as temp;

use crate::ast_super_flat::*;
use crate::error::{Result, error};
use crate::token::{Token, TokenKind, lex};

// Refer to tokens directly
#[allow(unused_imports)]
use crate::token::TokenKind::*;

pub fn parse(code: &str) -> Result<Ast> {
    let mut ast = Ast::builder();
    let c = &mut Cursor::new(code);
    let buf = Bump::new();

    let root = parse_root(c, &mut ast, &buf)?;

    Ok(ast.finish(root))
}

fn parse_root(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Root> {
    let mut body = temp![in buf];
    while !c.at(TOK_EOF) {
        body.push(parse_stmt(c, ast, buf)?);
    }
    let trailing_semi = c.was(TOK_SEMI);

    let tail = if !trailing_semi
        && let Some(tail) = body.last()
        && let StmtKind::Expr(tail) = tail.kind()
    {
        body.pop().unwrap();
        Opt::some(tail.inner(ast))
    } else {
        Opt::none()
    };

    Ok(Root::pack(ast, tail, &body))
}

fn parse_stmt(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Stmt> {
    match c.kind() {
        KW_FN => parse_stmt_fn(c, ast, buf),
        KW_LET => parse_stmt_let(c, ast, buf),
        _ => parse_stmt_expr(c, ast, buf),
    }
}

fn parse_stmt_fn(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Stmt> {
    assert!(c.eat(KW_FN));

    let name = parse_ident(c, ast, buf)?;
    let params = parse_param_list(c, ast, buf)?;
    let body = parse_expr_block(c, ast, buf)?;

    Ok(StmtFn::pack(ast, name, body, &params).into())
}

fn parse_param_list<'a>(
    c: &mut Cursor,
    ast: &mut AstBuilder,
    buf: &'a Bump,
) -> Result<Temp<'a, ExprIdent>> {
    parse_paren_list(c, ast, buf, parse_ident)
}

fn parse_paren_list<'a, F, T>(
    c: &mut Cursor,
    ast: &mut AstBuilder,
    buf: &'a Bump,
    mut elem: F,
) -> Result<Temp<'a, T>>
where
    F: FnMut(&mut Cursor, &mut AstBuilder, &Bump) -> Result<T>,
{
    let mut list = temp![in buf];

    c.must(TOK_LPAREN)?;
    if !c.at(TOK_RPAREN) {
        loop {
            list.push(elem(c, ast, buf)?);
            // stop if there's no comma, or if the comma is trailing.
            if !c.eat(TOK_COMMA) || c.at(TOK_RPAREN) {
                break;
            }
        }
    }
    c.must(TOK_RPAREN)?;

    Ok(list)
}

fn parse_stmt_let(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Stmt> {
    assert!(c.eat(KW_LET));

    let name = parse_ident(c, ast, buf)?;
    c.must(OP_EQ)?;
    let value = parse_expr(c, ast, buf)?;
    c.must(TOK_SEMI)?;

    Ok(StmtLet::pack(ast, name, value).into())
}

fn parse_stmt_expr(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Stmt> {
    let expr = parse_expr(c, ast, buf)?;

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

    Ok(StmtExpr::pack(ast, expr).into())
}

fn parse_expr(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    match c.kind() {
        KW_IF => parse_expr_if(c, ast, buf),
        _ => parse_expr_or(c, ast, buf),
    }
}

fn parse_expr_if(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    assert!(c.eat(KW_IF));

    let mut branches = temp![in buf; parse_if_branch(c, ast, buf)?];
    let mut tail = Opt::none();
    while c.eat(KW_ELSE) {
        if c.eat(KW_IF) {
            branches.push(parse_if_branch(c, ast, buf)?);
        } else {
            tail = Opt::some(parse_expr_block(c, ast, buf)?);
            break;
        }
    }

    Ok(ExprIf::pack(ast, tail, &branches).into())
}

fn parse_if_branch(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<IfBranch> {
    let cond = parse_expr(c, ast, buf)?;
    let body = parse_expr_block(c, ast, buf)?;

    Ok(IfBranch::pack(ast, cond, body))
}

fn parse_expr_or(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let mut lhs = parse_expr_and(c, ast, buf)?;
    while c.eat(OP_OR) {
        let op = BinaryOp::Or;
        let rhs = parse_expr_and(c, ast, buf)?;
        lhs = ExprBinary::pack(ast, lhs, rhs, op).into();
    }

    Ok(lhs)
}

fn parse_expr_and(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let mut lhs = parse_expr_eq(c, ast, buf)?;
    while c.eat(OP_AND) {
        let op = BinaryOp::And;
        let rhs = parse_expr_eq(c, ast, buf)?;
        lhs = ExprBinary::pack(ast, lhs, rhs, op).into();
    }

    Ok(lhs)
}

fn parse_expr_eq(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let mut lhs = parse_expr_ord(c, ast, buf)?;
    loop {
        let op = match c.kind() {
            OP_EQEQ => BinaryOp::Equal,
            OP_NEQ => BinaryOp::NotEqual,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_ord(c, ast, buf)?;
        lhs = ExprBinary::pack(ast, lhs, rhs, op).into();
    }

    Ok(lhs)
}

fn parse_expr_ord(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let mut lhs = parse_expr_add(c, ast, buf)?;
    loop {
        let op = match c.kind() {
            OP_LT => BinaryOp::LessThan,
            OP_LE => BinaryOp::LessOrEqual,
            OP_GT => BinaryOp::GreaterThan,
            OP_GE => BinaryOp::GreaterOrEqual,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_add(c, ast, buf)?;
        lhs = ExprBinary::pack(ast, lhs, rhs, op).into();
    }

    Ok(lhs)
}

fn parse_expr_add(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let mut lhs = parse_expr_mul(c, ast, buf)?;
    loop {
        let op = match c.kind() {
            OP_PLUS => BinaryOp::Add,
            OP_MINUS => BinaryOp::Subtract,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_mul(c, ast, buf)?;
        lhs = ExprBinary::pack(ast, lhs, rhs, op).into();
    }

    Ok(lhs)
}

fn parse_expr_mul(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let mut lhs = parse_expr_unary(c, ast, buf)?;
    loop {
        let op = match c.kind() {
            OP_STAR => BinaryOp::Multiply,
            OP_SLASH => BinaryOp::Divide,
            _ => break,
        };
        c.advance(); // eat `op`
        let rhs = parse_expr_unary(c, ast, buf)?;
        lhs = ExprBinary::pack(ast, lhs, rhs, op).into();
    }

    Ok(lhs)
}

fn parse_expr_unary(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let op = match c.kind() {
        OP_MINUS => UnaryOp::Minus,
        OP_BANG => UnaryOp::Not,
        _ => return parse_expr_postfix(c, ast, buf),
    };
    c.advance(); // eat `op`
    let rhs = parse_expr_unary(c, ast, buf)?;
    Ok(ExprUnary::pack(ast, rhs, op).into())
}

fn parse_expr_postfix(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let mut expr = parse_expr_primary(c, ast, buf)?;
    while c.at(TOK_LPAREN) {
        expr = parse_expr_call(c, ast, buf, expr)?;
    }

    Ok(expr)
}

fn parse_expr_call(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump, callee: Expr) -> Result<Expr> {
    let args = parse_arg_list(c, ast, buf)?;

    Ok(ExprCall::pack(ast, callee, &args).into())
}

fn parse_arg_list<'a>(
    c: &mut Cursor,
    ast: &mut AstBuilder,
    buf: &'a Bump,
) -> Result<Temp<'a, Expr>> {
    let args = parse_paren_list(c, ast, buf, parse_expr)?;
    Ok(args)
}

fn parse_expr_primary(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    match c.kind() {
        LIT_INT => parse_expr_int(c, ast, buf),
        LIT_STR => parse_expr_str(c, ast, buf),
        LIT_IDENT => parse_expr_ident(c, ast, buf).map(Into::into),
        TOK_LBRACE => parse_expr_block(c, ast, buf).map(Into::into),
        TOK_LPAREN => parse_expr_group(c, ast, buf),
        _ => error(
            format!("unexpected token: {:?}", c.kind()),
            c.current().span,
        )
        .into(),
    }
}

fn parse_expr_int(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let _ = buf;
    let token = c.must(LIT_INT)?;
    let lexeme = c.lexeme(token);
    if lexeme.starts_with('0') && lexeme.len() > 1 {
        return error("invalid integer", token.span).into();
    }
    let value: i64 = lexeme
        .parse()
        .map_err(|err| error(format!("failed to parse integer: {err}"), token.span))?;
    let id = ast.intern_int(value);
    Ok(ExprInt::pack(id).into())
}

fn parse_expr_str(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    let _ = buf;
    let token = c.must(LIT_STR)?;
    let unquoted = c
        .lexeme(token)
        .strip_prefix('"')
        .expect("string was lexed without opening quote")
        .strip_suffix('"')
        .expect("string was lexed without closing quote");
    let id = ast.intern_string(unquoted);
    Ok(ExprStr::pack(id).into())
}

fn parse_expr_ident(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<ExprIdent> {
    let _ = buf;
    let token = c.must(LIT_IDENT)?;
    let id = ast.intern_ident(c.lexeme(token));
    Ok(ExprIdent::pack(id))
}

fn parse_expr_block(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<ExprBlock> {
    c.must(TOK_LBRACE)?;
    let mut body = temp![in buf];
    while !c.at(TOK_RBRACE) {
        body.push(parse_stmt(c, ast, buf)?);
    }
    let trailing_semi = c.was(TOK_SEMI);
    c.must(TOK_RBRACE)?;

    let tail = if !trailing_semi
        && let Some(tail) = body.last()
        && let StmtKind::Expr(tail) = tail.kind()
    {
        body.pop().unwrap();
        Opt::some(tail.inner(ast))
    } else {
        Opt::none()
    };

    Ok(ExprBlock::pack(ast, tail, &body))
}

fn parse_expr_group(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<Expr> {
    assert!(c.eat(TOK_LPAREN));
    let inner = parse_expr(c, ast, buf)?;
    c.must(TOK_RPAREN)?;

    Ok(inner)
}

fn parse_ident(c: &mut Cursor, ast: &mut AstBuilder, buf: &Bump) -> Result<ExprIdent> {
    let _ = buf;
    let token = c.must(LIT_IDENT)?;
    let id = ast.intern_ident(c.lexeme(token));
    Ok(ExprIdent::pack(id))
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
