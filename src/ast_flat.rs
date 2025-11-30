use crate::intern::{Intern as _, Interner};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(u32);

declare_intern_id!(pub StrId);
declare_intern_id!(pub IdentId);

impl ExprId {
    pub fn new(idx: u32) -> Self {
        Self(idx)
    }

    pub fn index(self) -> u32 {
        self.0
    }
}

impl StmtId {
    pub fn new(idx: u32) -> Self {
        Self(idx)
    }

    pub fn index(self) -> u32 {
        self.0
    }
}

pub struct Ast {
    pub program: Program,

    exprs: Vec<Expr>,
    stmts: Vec<Stmt>,

    strings: Interner<StrId>,
    idents: Interner<IdentId>,
}

impl Ast {
    pub fn builder() -> AstBuilder {
        AstBuilder {
            exprs: Vec::new(),
            stmts: Vec::new(),
            strings: Interner::new(),
            idents: Interner::new(),
        }
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id.0 as usize]
    }

    pub fn stmt(&self, id: StmtId) -> &Stmt {
        &self.stmts[id.0 as usize]
    }

    pub fn str(&self, id: StrId) -> Option<&str> {
        self.strings.get(id)
    }

    pub fn ident(&self, id: IdentId) -> Option<&str> {
        self.idents.get(id)
    }
}

pub struct AstBuilder {
    exprs: Vec<Expr>,
    stmts: Vec<Stmt>,

    strings: Interner<StrId>,
    idents: Interner<IdentId>,
}

impl AstBuilder {
    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id.0 as usize]
    }

    pub fn stmt(&self, id: StmtId) -> &Stmt {
        &self.stmts[id.0 as usize]
    }

    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        let id = ExprId(self.exprs.len() as u32);
        self.exprs.push(expr);
        id
    }

    pub fn alloc_stmt(&mut self, stmt: Stmt) -> StmtId {
        let id = StmtId(self.stmts.len() as u32);
        self.stmts.push(stmt);
        id
    }

    pub fn intern_str(&mut self, str: &str) -> StrId {
        self.strings.intern(str)
    }

    pub fn intern_ident(&mut self, str: &str) -> IdentId {
        self.idents.intern(str)
    }

    pub fn finish(self, program: Program) -> Ast {
        Ast {
            program,
            exprs: self.exprs,
            stmts: self.stmts,
            strings: self.strings,
            idents: self.idents,
        }
    }
}

/// A program is a block of statements, with an optional trailing expression.
pub struct Program {
    pub body: Vec<StmtId>,
    pub tail: Option<ExprId>,
}

pub enum Stmt {
    Fn(StmtFn),
    Let(StmtLet),
    Expr(ExprId),
}

pub struct StmtFn {
    pub name: IdentId,
    pub params: Vec<IdentId>,
    pub body: ExprId,
}

pub struct StmtLet {
    pub name: IdentId,
    pub value: ExprId,
}

pub enum Expr {
    If(ExprIf),
    Binary(ExprBinary),
    Unary(ExprUnary),
    Call(ExprCall),
    Int(ExprInt),
    Str(ExprStr),
    Ident(ExprIdent),
    Block(ExprBlock),
}

pub struct ExprIf {
    /// List of branches, including the first one.
    pub branches: Vec<IfBranch>,

    /// The final `else` branch.
    pub tail: Option<ExprId>,
}

/// A single branch of an `if` expression.
pub struct IfBranch {
    /// The condition under which [`Self::body`] should run.
    pub cond: ExprId,

    /// The contents of the branch.
    pub body: ExprId,
}

/// An expression with two sub-expressions, to which
/// some operation is applied.
pub struct ExprBinary {
    /// Left sub-expression.
    pub lhs: ExprId,

    /// What should we do with the two sub-expressions.
    pub op: BinaryOp,

    /// Right sub-expression.
    pub rhs: ExprId,
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
pub struct ExprUnary {
    /// Right sub-expression.
    pub rhs: ExprId,

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
pub struct ExprCall {
    pub callee: ExprId,
    pub args: Vec<ExprId>,
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

pub struct ExprBlock {
    pub body: Vec<StmtId>,
    pub tail: Option<ExprId>,
}

pub struct DebugExprId<'a>(&'a Ast, ExprId);
pub struct DebugStmtId<'a>(&'a Ast, StmtId);
pub struct DebugStrId<'a>(&'a Ast, StrId);
pub struct DebugIdentId<'a>(&'a Ast, IdentId);

impl ExprId {
    pub fn debug<'a>(self, ast: &'a Ast) -> DebugExprId<'a> {
        DebugExprId(ast, self)
    }
}

impl StmtId {
    pub fn debug<'a>(self, ast: &'a Ast) -> DebugStmtId<'a> {
        DebugStmtId(ast, self)
    }
}

impl StrId {
    pub fn debug<'a>(self, ast: &'a Ast) -> DebugStrId<'a> {
        DebugStrId(ast, self)
    }
}

impl IdentId {
    pub fn debug<'a>(self, ast: &'a Ast) -> DebugIdentId<'a> {
        DebugIdentId(ast, self)
    }
}

impl<'a> std::fmt::Debug for DebugExprId<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.expr(self.1) {
            Expr::If(v) => f
                .debug_struct("ExprIf")
                .field("branches", &DebugList(self.0, &v.branches))
                .field("tail", &v.tail.map(|id| DebugExprId(self.0, id)))
                .finish(),
            Expr::Binary(v) => f
                .debug_struct("ExprBinary")
                .field("lhs", &DebugExprId(self.0, v.lhs))
                .field("op", &v.op)
                .field("rhs", &DebugExprId(self.0, v.rhs))
                .finish(),
            Expr::Unary(v) => f
                .debug_struct("ExprUnary")
                .field("rhs", &DebugExprId(self.0, v.rhs))
                .field("op", &v.op)
                .finish(),
            Expr::Call(v) => f
                .debug_struct("ExprCall")
                .field("callee", &DebugExprId(self.0, v.callee))
                .field("args", &DebugIdList(self.0, &v.args))
                .finish(),
            Expr::Int(v) => f.debug_struct("ExprInt").field("value", &v.value).finish(),
            Expr::Str(v) => f
                .debug_struct("ExprStr")
                .field("value", &DebugStrId(self.0, v.value))
                .finish(),
            Expr::Ident(v) => f
                .debug_struct("ExprIdent")
                .field("name", &DebugIdentId(self.0, v.name))
                .finish(),
            Expr::Block(v) => f
                .debug_struct("ExprBlock")
                .field("body", &DebugIdList(self.0, &v.body))
                .field("tail", &v.tail.map(|id| DebugExprId(self.0, id)))
                .finish(),
        }
    }
}

impl<'a> std::fmt::Debug for DebugStmtId<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.stmt(self.1) {
            Stmt::Fn(v) => f
                .debug_struct("StmtFn")
                .field("name", &DebugIdentId(self.0, v.name))
                .field("params", &DebugIdList(self.0, &v.params))
                .field("body", &DebugExprId(self.0, v.body))
                .finish(),
            Stmt::Let(v) => f
                .debug_struct("StmtLet")
                .field("name", &DebugIdentId(self.0, v.name))
                .field("value", &DebugExprId(self.0, v.value))
                .finish(),
            Stmt::Expr(id) => f
                .debug_tuple("StmtExpr")
                .field(&DebugExprId(self.0, *id))
                .finish(),
        }
    }
}

impl<'a> std::fmt::Debug for DebugStrId<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.str(self.1) {
            Some(s) => f.write_str(s),
            None => write!(f, "<invalid StrId({})>", self.1.index()),
        }
    }
}

impl<'a> std::fmt::Debug for DebugIdentId<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.ident(self.1) {
            Some(s) => f.write_str(s),
            None => write!(f, "<invalid IdentId({})>", self.1.index()),
        }
    }
}

struct DebugIdList<'a, T>(&'a Ast, &'a [T]);

impl<'a> std::fmt::Debug for DebugIdList<'a, ExprId> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.1.iter().map(|&id| DebugExprId(self.0, id)))
            .finish()
    }
}

impl<'a> std::fmt::Debug for DebugIdList<'a, StmtId> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.1.iter().map(|&id| DebugStmtId(self.0, id)))
            .finish()
    }
}

impl<'a> std::fmt::Debug for DebugIdList<'a, IdentId> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.1.iter().map(|&id| DebugIdentId(self.0, id)))
            .finish()
    }
}

struct DebugList<'a, T>(&'a Ast, &'a [T]);

impl<'a> std::fmt::Debug for DebugList<'a, IfBranch> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.1.iter().map(|branch| DebugIfBranch(self.0, branch)))
            .finish()
    }
}

struct DebugIfBranch<'a>(&'a Ast, &'a IfBranch);

impl<'a> std::fmt::Debug for DebugIfBranch<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IfBranch")
            .field("cond", &DebugExprId(self.0, self.1.cond))
            .field("body", &DebugExprId(self.0, self.1.body))
            .finish()
    }
}

impl std::fmt::Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Program")
            .field("body", &DebugIdList(self, &self.program.body))
            .field("tail", &self.program.tail.map(|id| DebugExprId(self, id)))
            .finish()
    }
}
