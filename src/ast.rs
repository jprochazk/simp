/// A program is a block of statements, with an optional trailing expression.
#[derive(Debug)]
pub struct Program {
    pub body: Vec<Stmt>,
    pub tail: Option<Expr>,
}

pub enum Stmt {
    Fn(Box<StmtFn>),
    Let(Box<StmtLet>),
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub struct StmtFn {
    pub name: String,
    pub params: Vec<String>,
    pub body: Block,
}

#[derive(Debug)]
pub struct StmtLet {
    pub name: String,
    pub value: Expr,
}

pub enum Expr {
    If(Box<ExprIf>),
    Binary(Box<ExprBinary>),
    Unary(Box<ExprUnary>),
    Call(Box<ExprCall>),
    Int(Box<ExprInt>),
    Str(Box<ExprStr>),
    Ident(Box<ExprIdent>),
    Block(Box<ExprBlock>),
}

#[derive(Debug)]
pub struct ExprIf {
    /// List of branches, including the first one.
    pub branches: Vec<IfBranch>,

    /// The final `else` branch.
    pub tail: Option<Block>,
}

/// A single branch of an `if` expression.
#[derive(Debug)]
pub struct IfBranch {
    /// The condition under which [`Self::body`] should run.
    pub cond: Expr,

    /// The contents of the branch.
    pub body: Block,
}

/// An expression with two sub-expressions, to which
/// some operation is applied.
#[derive(Debug)]
pub struct ExprBinary {
    /// Left sub-expression.
    pub lhs: Expr,

    /// What should we do with the two sub-expressions.
    pub op: BinaryOp,

    /// Right sub-expression.
    pub rhs: Expr,
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
pub struct ExprUnary {
    /// Right sub-expression.
    pub rhs: Expr,

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
pub struct ExprCall {
    pub callee: Expr,
    pub args: Vec<Expr>,
}

/// An integer.
#[derive(Debug)]
pub struct ExprInt {
    pub value: i64,
}

/// A string.
#[derive(Debug)]
pub struct ExprStr {
    pub value: String,
}

/// A variable use.
#[derive(Debug)]
pub struct ExprIdent {
    pub name: String,
}

#[derive(Debug)]
pub struct ExprBlock {
    pub inner: Block,
}

/// A block is a list of statements, with an optional
/// trailing expression which is used as its value.
#[derive(Debug)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub tail: Option<Expr>,
}

impl std::fmt::Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Fn(v) => std::fmt::Debug::fmt(v, f),
            Stmt::Let(v) => std::fmt::Debug::fmt(v, f),
            Stmt::Expr(v) => std::fmt::Debug::fmt(v, f),
        }
    }
}

impl std::fmt::Debug for Expr {
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
