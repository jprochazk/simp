use std::cell::Cell;

/// A program is a block of statements, with an optional trailing expression.
#[derive(Debug)]
pub struct Program {
    pub body: Vec<Stmt>,
    pub tail: Option<Expr>,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

thread_local! {
    static INDENT: Cell<usize> = const { Cell::new(0) };
}

struct W;

impl std::fmt::Display for W {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..INDENT.with(|v| v.get()) {
            f.write_str("  ")?;
        }

        Ok(())
    }
}

fn indent() {
    INDENT.with(|v| v.set(v.get() + 1));
}

fn dedent() {
    INDENT.with(|v| v.set(v.get() - 1));
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        indent();
        for item in &self.body {
            writeln!(f, "{item}")?;
        }

        if let Some(tail) = &self.tail {
            writeln!(f, "(tail) {tail}")?;
        }
        dedent();

        Ok(())
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! nest {
            ($v:expr) => {{
                write!(f, "{}", $v)?;
            }};
        }
        match self {
            Stmt::Fn(v) => nest!(v),
            Stmt::Let(v) => nest!(v),
            Stmt::Expr(v) => nest!(v),
        }

        Ok(())
    }
}

impl std::fmt::Display for StmtFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Fn")?;
        writeln!(f, "{W}name: {:?}", self.name)?;
        writeln!(f, "{W}params: {:?}", self.params)?;
        writeln!(f, "{W}body: {}", self.body)?;

        Ok(())
    }
}

impl std::fmt::Display for StmtLet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Let")?;
        writeln!(f, "{W}name: {:?}", self.name)?;
        writeln!(f, "{W}value: {}", self.value)?;

        Ok(())
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! nest {
            ($v:expr) => {{
                write!(f, "{}", $v)?;
            }};
        }
        match self {
            Expr::If(v) => nest!(v),
            Expr::Binary(v) => nest!(v),
            Expr::Unary(v) => nest!(v),
            Expr::Call(v) => nest!(v),
            Expr::Int(v) => nest!(v),
            Expr::Str(v) => nest!(v),
            Expr::Ident(v) => nest!(v),
            Expr::Block(v) => nest!(v),
        }

        Ok(())
    }
}

impl std::fmt::Display for ExprIf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "If")?;
        for branch in &self.branches {
            writeln!(f, "{W}Branch")?;
            indent();
            writeln!(f, "{W}cond: {}", branch.cond)?;
            writeln!(f, "{W}body: {}", branch.body)?;
            dedent();
        }
        if let Some(tail) = &self.tail {
            writeln!(f, "{W}tail: {tail}")?;
        }

        Ok(())
    }
}

impl std::fmt::Display for ExprBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Binary")?;
        writeln!(f, "{W}op: {:?}", self.op)?;
        writeln!(f, "{W}lhs: {}", self.lhs)?;
        writeln!(f, "{W}rhs: {}", self.rhs)?;

        Ok(())
    }
}

impl std::fmt::Display for ExprUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Unary")?;
        writeln!(f, "{W}{:?}", self.op)?;
        writeln!(f, "{W}rhs: {}", self.rhs)?;

        Ok(())
    }
}

impl std::fmt::Display for ExprCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Call")?;
        writeln!(f, "{W}callee: {}", self.callee)?;
        writeln!(f, "{W}args: [")?;
        indent();
        for arg in &self.args {
            write!(f, "{W}{arg}")?;
        }
        dedent();
        writeln!(f, "{W}]")?;

        Ok(())
    }
}

impl std::fmt::Display for ExprInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Int {}", self.value)?;

        Ok(())
    }
}

impl std::fmt::Display for ExprStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "str {:?}", self.value)?;

        Ok(())
    }
}

impl std::fmt::Display for ExprIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ident {:?}", self.name)?;

        Ok(())
    }
}

impl std::fmt::Display for ExprBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.body.is_empty() && self.tail.is_none() {
            writeln!(f, "Block []")?;
            return Ok(());
        }
        writeln!(f, "Block [")?;
        indent();
        for item in &self.body {
            let w = W.to_string();
            indent();
            write!(f, "{w}{item}")?;
            dedent();
        }
        if let Some(tail) = &self.tail {
            let w = W.to_string();
            indent();
            write!(f, "{w}{tail}")?;
            dedent();
        }
        dedent();
        write!(f, "{W}]")?;

        Ok(())
    }
}
