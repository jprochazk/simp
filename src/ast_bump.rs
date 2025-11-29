use bumpalo::Bump;
use bumpalo::collections::Vec;

use crate::intern::Interner;

pub struct Ast<'a> {
    pub program: Program<'a>,

    strings: Interner<StrId>,
    idents: Interner<IdentId>,
}

impl<'a> Ast<'a> {
    pub fn builder(buf: &'a Bump) -> AstBuilder<'a> {
        AstBuilder {
            bump: buf,
            strings: Interner::new(),
            idents: Interner::new(),
        }
    }

    pub fn str(&self, id: StrId) -> Option<&str> {
        self.strings.get(id)
    }

    pub fn ident(&self, id: IdentId) -> Option<&str> {
        self.idents.get(id)
    }
}

pub struct AstBuilder<'a> {
    bump: &'a Bump,
    strings: Interner<StrId>,
    idents: Interner<IdentId>,
}

impl<'a> AstBuilder<'a> {
    pub fn intern_str(&mut self, str: &str) -> StrId {
        self.strings.intern(str)
    }

    pub fn intern_ident(&mut self, str: &str) -> IdentId {
        self.idents.intern(str)
    }

    pub fn bump(&self) -> &'a Bump {
        self.bump
    }

    pub fn alloc<T>(&self, v: T) -> &'a T {
        self.bump.alloc(v)
    }

    pub fn finish(self, program: Program) -> Ast {
        Ast {
            program,
            strings: self.strings,
            idents: self.idents,
        }
    }
}

declare_intern_id!(pub StrId);
declare_intern_id!(pub IdentId);

/// A program is a block of statements, with an optional trailing expression.
#[derive(Debug)]
pub struct Program<'a> {
    pub body: Vec<'a, Stmt<'a>>,
    pub tail: Option<&'a Expr<'a>>,
}

pub enum Stmt<'a> {
    Fn(&'a StmtFn<'a>),
    Let(&'a StmtLet<'a>),
    Expr(&'a Expr<'a>),
}

#[derive(Debug)]
pub struct StmtFn<'a> {
    pub name: IdentId,
    pub params: Vec<'a, IdentId>,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct StmtLet<'a> {
    pub name: IdentId,
    pub value: Expr<'a>,
}

pub enum Expr<'a> {
    If(&'a ExprIf<'a>),
    Binary(&'a ExprBinary<'a>),
    Unary(&'a ExprUnary<'a>),
    Call(&'a ExprCall<'a>),
    Int(&'a ExprInt),
    Str(&'a ExprStr),
    Ident(&'a ExprIdent),
    Block(&'a ExprBlock<'a>),
}

#[derive(Debug)]
pub struct ExprIf<'a> {
    /// List of branches, including the first one.
    pub branches: Vec<'a, IfBranch<'a>>,

    /// The final `else` branch.
    pub tail: Option<Block<'a>>,
}

/// A single branch of an `if` expression.
#[derive(Debug)]
pub struct IfBranch<'a> {
    /// The condition under which [`Self::body`] should run.
    pub cond: Expr<'a>,

    /// The contents of the branch.
    pub body: Block<'a>,
}

/// An expression with two sub-expressions, to which
/// some operation is applied.
#[derive(Debug)]
pub struct ExprBinary<'a> {
    /// Left sub-expression.
    pub lhs: Expr<'a>,

    /// What should we do with the two sub-expressions.
    pub op: BinaryOp,

    /// Right sub-expression.
    pub rhs: Expr<'a>,
}

#[derive(Debug)]
pub enum BinaryOp {
    /// `+`
    Add,

    /// `-`
    Subtract,

    /// `*`
    Multiply,

    /// `/`
    Divide,

    /// `||`
    Or,

    /// `&&`
    And,

    /// `==`
    Equal,

    /// `!=`
    NotEqual,

    /// `<`
    LessThan,

    /// `<=`
    LessOrEqual,

    /// `>`
    GreaterThan,

    /// `>=`
    GreaterOrEqual,
}

/// An expression with one sub-expressions, to which
/// some operation is applied.
#[derive(Debug)]
pub struct ExprUnary<'a> {
    /// Right sub-expression.
    pub rhs: Expr<'a>,

    /// What should we do with the two sub-expressions.
    pub op: UnaryOp,
}

#[derive(Debug)]
pub enum UnaryOp {
    /// `-`
    Minus,

    /// `!`
    Not,
}

/// A function call.
#[derive(Debug)]
pub struct ExprCall<'a> {
    pub callee: Expr<'a>,
    pub args: Vec<'a, Expr<'a>>,
}

/// An integer.
#[derive(Debug)]
pub struct ExprInt {
    pub value: i64,
}

/// A string.
#[derive(Debug)]
pub struct ExprStr {
    pub value: StrId,
}

/// A variable use.
#[derive(Debug)]
pub struct ExprIdent {
    pub name: IdentId,
}

#[derive(Debug)]
pub struct ExprBlock<'a> {
    pub inner: Block<'a>,
}

/// A block is a list of statements, with an optional
/// trailing expression which is used as its value.
#[derive(Debug)]
pub struct Block<'a> {
    pub body: Vec<'a, Stmt<'a>>,
    pub tail: Option<&'a Expr<'a>>,
}

impl std::fmt::Debug for Stmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Fn(v) => std::fmt::Debug::fmt(v, f),
            Stmt::Let(v) => std::fmt::Debug::fmt(v, f),
            Stmt::Expr(v) => std::fmt::Debug::fmt(v, f),
        }
    }
}

impl std::fmt::Debug for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::If(v) => std::fmt::Debug::fmt(v, f),
            Expr::Binary(v) => std::fmt::Debug::fmt(v, f),
            Expr::Unary(v) => std::fmt::Debug::fmt(v, f),
            Expr::Call(v) => std::fmt::Debug::fmt(v, f),
            Expr::Int(v) => std::fmt::Debug::fmt(v, f),
            Expr::Str(v) => std::fmt::Debug::fmt(v, f),
            Expr::Ident(v) => std::fmt::Debug::fmt(v, f),
            Expr::Block(v) => std::fmt::Debug::fmt(v, f),
        }
    }
}
