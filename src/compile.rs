use std::hash::Hash;

use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Unit,
    Int(i64),
    Str(u32),
    Fn(u32),
    LoadLocal(u32),
    StoreLocal(u32),
    LoadGlobal(u32),
    StoreGlobal(u32),
    Binary(u8),
    Unary(u8),
    Call(u32),
    Jump(u32),
    JumpIfFalse(u32),
    JumpIfFalseOrPop(u32),
    JumpIfTrueOrPop(u32),
    Pop,
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub arity: u32,
    pub slots: u32,
    pub code: Vec<Op>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub functions: Vec<Function>,
    pub globals: u32,
    pub strings: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Summary {
    pub functions: usize,
    pub ops: usize,
    pub globals: u32,
    pub strings: u32,
}

impl Program {
    pub fn summary(&self) -> Summary {
        Summary {
            functions: self.functions.len(),
            ops: self.functions.iter().map(|f| f.code.len()).sum(),
            globals: self.globals,
            strings: self.strings,
        }
    }
}

#[derive(Clone, Copy)]
pub enum Var {
    Local(u32),
    Global(u32),
}

struct Frame<K> {
    code: Vec<Op>,
    locals: Vec<K>,
    scopes: Vec<u32>,
    arity: u32,
    slots: u32,
}

impl<K> Frame<K> {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            locals: Vec::new(),
            scopes: Vec::new(),
            arity: 0,
            slots: 0,
        }
    }
}

pub struct Compiler<K, S> {
    current: Frame<K>,
    outer: Vec<Frame<K>>,
    functions: Vec<Function>,
    globals: FxHashMap<K, u32>,
    strings: FxHashMap<S, u32>,
    jumps: Vec<u32>,
}

impl<K: Copy + Eq + Hash, S: Copy + Eq + Hash> Default for Compiler<K, S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Copy + Eq + Hash, S: Copy + Eq + Hash> Compiler<K, S> {
    pub fn new() -> Self {
        Self {
            current: Frame::new(),
            outer: Vec::new(),
            functions: Vec::new(),
            globals: FxHashMap::default(),
            strings: FxHashMap::default(),
            jumps: Vec::new(),
        }
    }

    #[inline]
    pub fn emit(&mut self, op: Op) -> u32 {
        let at = self.current.code.len() as u32;
        self.current.code.push(op);
        at
    }

    pub fn patch(&mut self, at: u32) {
        let target = self.current.code.len() as u32;
        match &mut self.current.code[at as usize] {
            Op::Jump(t) | Op::JumpIfFalse(t) | Op::JumpIfFalseOrPop(t) | Op::JumpIfTrueOrPop(t) => {
                *t = target
            }
            _ => unreachable!(),
        }
    }

    pub fn mark_jumps(&self) -> usize {
        self.jumps.len()
    }

    pub fn push_jump(&mut self, at: u32) {
        self.jumps.push(at);
    }

    pub fn patch_jumps(&mut self, mark: usize) {
        while self.jumps.len() > mark {
            let at = self.jumps.pop().unwrap();
            self.patch(at);
        }
    }

    pub fn begin_scope(&mut self) {
        self.current.scopes.push(self.current.locals.len() as u32);
    }

    pub fn end_scope(&mut self) {
        let len = self.current.scopes.pop().unwrap();
        self.current.locals.truncate(len as usize);
    }

    fn is_global_scope(&self) -> bool {
        self.outer.is_empty() && self.current.scopes.is_empty()
    }

    fn global(&mut self, name: K) -> u32 {
        let next = self.globals.len() as u32;
        *self.globals.entry(name).or_insert(next)
    }

    pub fn declare(&mut self, name: K) -> Var {
        if self.is_global_scope() {
            Var::Global(self.global(name))
        } else {
            let slot = self.current.locals.len() as u32;
            self.current.locals.push(name);
            self.current.slots = self.current.slots.max(slot + 1);
            Var::Local(slot)
        }
    }

    pub fn store(&mut self, var: Var) {
        match var {
            Var::Local(slot) => self.emit(Op::StoreLocal(slot)),
            Var::Global(index) => self.emit(Op::StoreGlobal(index)),
        };
    }

    pub fn load(&mut self, name: K) {
        match self.current.locals.iter().rposition(|&local| local == name) {
            Some(slot) => self.emit(Op::LoadLocal(slot as u32)),
            None => {
                let index = self.global(name);
                self.emit(Op::LoadGlobal(index))
            }
        };
    }

    pub fn string(&mut self, value: S) {
        let next = self.strings.len() as u32;
        let index = *self.strings.entry(value).or_insert(next);
        self.emit(Op::Str(index));
    }

    pub fn begin_function(&mut self) {
        let outer = std::mem::replace(&mut self.current, Frame::new());
        self.outer.push(outer);
    }

    pub fn param(&mut self, name: K) {
        self.declare(name);
        self.current.arity += 1;
    }

    pub fn end_function(&mut self) -> u32 {
        self.emit(Op::Return);
        let outer = self.outer.pop().unwrap();
        let frame = std::mem::replace(&mut self.current, outer);
        self.push_function(frame)
    }

    fn push_function(&mut self, frame: Frame<K>) -> u32 {
        let index = self.functions.len() as u32;
        self.functions.push(Function {
            arity: frame.arity,
            slots: frame.slots,
            code: frame.code,
        });
        index
    }

    pub fn finish(mut self) -> Program {
        self.emit(Op::Return);
        let main = std::mem::replace(&mut self.current, Frame::new());
        self.push_function(main);
        Program {
            functions: self.functions,
            globals: self.globals.len() as u32,
            strings: self.strings.len() as u32,
        }
    }
}

pub mod tree {
    use super::{Compiler, Op};
    use crate::ast::*;

    type C<'a> = Compiler<&'a str, &'a str>;

    pub fn compile(program: &Program) -> super::Program {
        let mut c = C::new();
        for s in &program.body {
            stmt(&mut c, s);
        }
        match &program.tail {
            Some(e) => expr(&mut c, e),
            None => {
                c.emit(Op::Unit);
            }
        }
        c.finish()
    }

    fn block<'a>(c: &mut C<'a>, b: &'a Block) {
        c.begin_scope();
        for s in &b.body {
            stmt(c, s);
        }
        match &b.tail {
            Some(e) => expr(c, e),
            None => {
                c.emit(Op::Unit);
            }
        }
        c.end_scope();
    }

    fn stmt<'a>(c: &mut C<'a>, s: &'a Stmt) {
        match s {
            Stmt::Fn(v) => {
                let var = c.declare(v.name.as_str());
                c.begin_function();
                for p in &v.params {
                    c.param(p.as_str());
                }
                block(c, &v.body);
                let index = c.end_function();
                c.emit(Op::Fn(index));
                c.store(var);
            }
            Stmt::Let(v) => {
                expr(c, &v.value);
                let var = c.declare(v.name.as_str());
                c.store(var);
            }
            Stmt::Expr(v) => {
                expr(c, v);
                c.emit(Op::Pop);
            }
        }
    }

    fn expr<'a>(c: &mut C<'a>, e: &'a Expr) {
        match e {
            Expr::If(v) => {
                let mark = c.mark_jumps();
                for b in &v.branches {
                    expr(c, &b.cond);
                    let next = c.emit(Op::JumpIfFalse(0));
                    block(c, &b.body);
                    let end = c.emit(Op::Jump(0));
                    c.push_jump(end);
                    c.patch(next);
                }
                match &v.tail {
                    Some(t) => block(c, t),
                    None => {
                        c.emit(Op::Unit);
                    }
                }
                c.patch_jumps(mark);
            }
            Expr::Binary(v) => match v.op {
                BinaryOp::And | BinaryOp::Or => {
                    expr(c, &v.lhs);
                    let skip = c.emit(match v.op {
                        BinaryOp::And => Op::JumpIfFalseOrPop(0),
                        _ => Op::JumpIfTrueOrPop(0),
                    });
                    expr(c, &v.rhs);
                    c.patch(skip);
                }
                op => {
                    expr(c, &v.lhs);
                    expr(c, &v.rhs);
                    c.emit(Op::Binary(op as u8));
                }
            },
            Expr::Unary(v) => {
                expr(c, &v.rhs);
                c.emit(Op::Unary(v.op as u8));
            }
            Expr::Call(v) => {
                expr(c, &v.callee);
                for a in &v.args {
                    expr(c, a);
                }
                c.emit(Op::Call(v.args.len() as u32));
            }
            Expr::Int(v) => {
                c.emit(Op::Int(v.value));
            }
            Expr::Str(v) => c.string(v.value.as_str()),
            Expr::Ident(v) => c.load(v.name.as_str()),
            Expr::Block(v) => block(c, &v.inner),
        }
    }
}

pub mod interned {
    use super::{Compiler, Op};
    use crate::ast_interned::*;
    use crate::intern::Intern as _;

    type C = Compiler<u32, u32>;

    pub fn compile(ast: &Ast) -> super::Program {
        let mut c = C::new();
        for s in &ast.program.body {
            stmt(&mut c, s);
        }
        match &ast.program.tail {
            Some(e) => expr(&mut c, e),
            None => {
                c.emit(Op::Unit);
            }
        }
        c.finish()
    }

    fn block(c: &mut C, b: &Block) {
        c.begin_scope();
        for s in &b.body {
            stmt(c, s);
        }
        match &b.tail {
            Some(e) => expr(c, e),
            None => {
                c.emit(Op::Unit);
            }
        }
        c.end_scope();
    }

    fn stmt(c: &mut C, s: &Stmt) {
        match s {
            Stmt::Fn(v) => {
                let var = c.declare(v.name.index());
                c.begin_function();
                for p in &v.params {
                    c.param(p.index());
                }
                block(c, &v.body);
                let index = c.end_function();
                c.emit(Op::Fn(index));
                c.store(var);
            }
            Stmt::Let(v) => {
                expr(c, &v.value);
                let var = c.declare(v.name.index());
                c.store(var);
            }
            Stmt::Expr(v) => {
                expr(c, v);
                c.emit(Op::Pop);
            }
        }
    }

    fn expr(c: &mut C, e: &Expr) {
        match e {
            Expr::If(v) => {
                let mark = c.mark_jumps();
                for b in &v.branches {
                    expr(c, &b.cond);
                    let next = c.emit(Op::JumpIfFalse(0));
                    block(c, &b.body);
                    let end = c.emit(Op::Jump(0));
                    c.push_jump(end);
                    c.patch(next);
                }
                match &v.tail {
                    Some(t) => block(c, t),
                    None => {
                        c.emit(Op::Unit);
                    }
                }
                c.patch_jumps(mark);
            }
            Expr::Binary(v) => match v.op {
                BinaryOp::And | BinaryOp::Or => {
                    expr(c, &v.lhs);
                    let skip = c.emit(match v.op {
                        BinaryOp::And => Op::JumpIfFalseOrPop(0),
                        _ => Op::JumpIfTrueOrPop(0),
                    });
                    expr(c, &v.rhs);
                    c.patch(skip);
                }
                op => {
                    expr(c, &v.lhs);
                    expr(c, &v.rhs);
                    c.emit(Op::Binary(op as u8));
                }
            },
            Expr::Unary(v) => {
                expr(c, &v.rhs);
                c.emit(Op::Unary(v.op as u8));
            }
            Expr::Call(v) => {
                expr(c, &v.callee);
                for a in &v.args {
                    expr(c, a);
                }
                c.emit(Op::Call(v.args.len() as u32));
            }
            Expr::Int(v) => {
                c.emit(Op::Int(v.value));
            }
            Expr::Str(v) => c.string(v.value.index()),
            Expr::Ident(v) => c.load(v.name.index()),
            Expr::Block(v) => block(c, &v.inner),
        }
    }
}

pub mod bump {
    use super::{Compiler, Op};
    use crate::ast_bump::*;
    use crate::intern::Intern as _;

    type C = Compiler<u32, u32>;

    pub fn compile(ast: &Ast) -> super::Program {
        let mut c = C::new();
        for s in &ast.program.body {
            stmt(&mut c, s);
        }
        match ast.program.tail {
            Some(e) => expr(&mut c, e),
            None => {
                c.emit(Op::Unit);
            }
        }
        c.finish()
    }

    fn block(c: &mut C, b: &Block) {
        c.begin_scope();
        for s in &b.body {
            stmt(c, s);
        }
        match b.tail {
            Some(e) => expr(c, e),
            None => {
                c.emit(Op::Unit);
            }
        }
        c.end_scope();
    }

    fn stmt(c: &mut C, s: &Stmt) {
        match s {
            Stmt::Fn(v) => {
                let var = c.declare(v.name.index());
                c.begin_function();
                for p in &v.params {
                    c.param(p.index());
                }
                block(c, &v.body);
                let index = c.end_function();
                c.emit(Op::Fn(index));
                c.store(var);
            }
            Stmt::Let(v) => {
                expr(c, &v.value);
                let var = c.declare(v.name.index());
                c.store(var);
            }
            Stmt::Expr(v) => {
                expr(c, v);
                c.emit(Op::Pop);
            }
        }
    }

    fn expr(c: &mut C, e: &Expr) {
        match e {
            Expr::If(v) => {
                let mark = c.mark_jumps();
                for b in &v.branches {
                    expr(c, &b.cond);
                    let next = c.emit(Op::JumpIfFalse(0));
                    block(c, &b.body);
                    let end = c.emit(Op::Jump(0));
                    c.push_jump(end);
                    c.patch(next);
                }
                match &v.tail {
                    Some(t) => block(c, t),
                    None => {
                        c.emit(Op::Unit);
                    }
                }
                c.patch_jumps(mark);
            }
            Expr::Binary(v) => match v.op {
                BinaryOp::And | BinaryOp::Or => {
                    expr(c, &v.lhs);
                    let skip = c.emit(match v.op {
                        BinaryOp::And => Op::JumpIfFalseOrPop(0),
                        _ => Op::JumpIfTrueOrPop(0),
                    });
                    expr(c, &v.rhs);
                    c.patch(skip);
                }
                op => {
                    expr(c, &v.lhs);
                    expr(c, &v.rhs);
                    c.emit(Op::Binary(op as u8));
                }
            },
            Expr::Unary(v) => {
                expr(c, &v.rhs);
                c.emit(Op::Unary(v.op as u8));
            }
            Expr::Call(v) => {
                expr(c, &v.callee);
                for a in &v.args {
                    expr(c, a);
                }
                c.emit(Op::Call(v.args.len() as u32));
            }
            Expr::Int(v) => {
                c.emit(Op::Int(v.value));
            }
            Expr::Str(v) => c.string(v.value.index()),
            Expr::Ident(v) => c.load(v.name.index()),
            Expr::Block(v) => block(c, &v.inner),
        }
    }
}

pub mod flat {
    use super::{Compiler, Op};
    use crate::ast_flat::*;
    use crate::intern::Intern as _;

    type C = Compiler<u32, u32>;

    pub fn compile(ast: &Ast) -> super::Program {
        let mut c = C::new();
        for &s in &ast.program.body {
            stmt(&mut c, ast, s);
        }
        match ast.program.tail {
            Some(e) => expr(&mut c, ast, e),
            None => {
                c.emit(Op::Unit);
            }
        }
        c.finish()
    }

    fn block_id(c: &mut C, ast: &Ast, id: ExprId) {
        let Expr::Block(b) = ast.expr(id) else {
            unreachable!()
        };
        block(c, ast, b);
    }

    fn block(c: &mut C, ast: &Ast, b: &ExprBlock) {
        c.begin_scope();
        for &s in &b.body {
            stmt(c, ast, s);
        }
        match b.tail {
            Some(e) => expr(c, ast, e),
            None => {
                c.emit(Op::Unit);
            }
        }
        c.end_scope();
    }

    fn stmt(c: &mut C, ast: &Ast, id: StmtId) {
        match ast.stmt(id) {
            Stmt::Fn(v) => {
                let var = c.declare(v.name.index());
                c.begin_function();
                for p in &v.params {
                    c.param(p.index());
                }
                block_id(c, ast, v.body);
                let index = c.end_function();
                c.emit(Op::Fn(index));
                c.store(var);
            }
            Stmt::Let(v) => {
                expr(c, ast, v.value);
                let var = c.declare(v.name.index());
                c.store(var);
            }
            Stmt::Expr(v) => {
                expr(c, ast, *v);
                c.emit(Op::Pop);
            }
        }
    }

    fn expr(c: &mut C, ast: &Ast, id: ExprId) {
        match ast.expr(id) {
            Expr::If(v) => {
                let mark = c.mark_jumps();
                for b in &v.branches {
                    expr(c, ast, b.cond);
                    let next = c.emit(Op::JumpIfFalse(0));
                    block_id(c, ast, b.body);
                    let end = c.emit(Op::Jump(0));
                    c.push_jump(end);
                    c.patch(next);
                }
                match v.tail {
                    Some(t) => block_id(c, ast, t),
                    None => {
                        c.emit(Op::Unit);
                    }
                }
                c.patch_jumps(mark);
            }
            Expr::Binary(v) => match v.op {
                BinaryOp::And | BinaryOp::Or => {
                    expr(c, ast, v.lhs);
                    let skip = c.emit(match v.op {
                        BinaryOp::And => Op::JumpIfFalseOrPop(0),
                        _ => Op::JumpIfTrueOrPop(0),
                    });
                    expr(c, ast, v.rhs);
                    c.patch(skip);
                }
                op => {
                    expr(c, ast, v.lhs);
                    expr(c, ast, v.rhs);
                    c.emit(Op::Binary(op as u8));
                }
            },
            Expr::Unary(v) => {
                expr(c, ast, v.rhs);
                c.emit(Op::Unary(v.op as u8));
            }
            Expr::Call(v) => {
                expr(c, ast, v.callee);
                for &a in &v.args {
                    expr(c, ast, a);
                }
                c.emit(Op::Call(v.args.len() as u32));
            }
            Expr::Int(v) => {
                c.emit(Op::Int(v.value));
            }
            Expr::Str(v) => c.string(v.value.index()),
            Expr::Ident(v) => c.load(v.name.index()),
            Expr::Block(v) => block(c, ast, v),
        }
    }
}

macro_rules! compile_super_flat {
    ($ast:path) => {
        use super::{Compiler, Op};
        use crate::intern::Intern as _;
        use $ast::*;

        type C = Compiler<u32, u32>;

        pub fn compile(ast: &Ast) -> super::Program {
            let mut c = C::new();
            let root = ast.root();
            for &s in root.body(ast) {
                stmt(&mut c, ast, s);
            }
            match root.tail(ast).into_option() {
                Some(e) => expr(&mut c, ast, e),
                None => {
                    c.emit(Op::Unit);
                }
            }
            c.finish()
        }

        fn block(c: &mut C, ast: &Ast, b: ExprBlock) {
            c.begin_scope();
            for &s in b.body(ast) {
                stmt(c, ast, s);
            }
            match b.tail(ast).into_option() {
                Some(e) => expr(c, ast, e),
                None => {
                    c.emit(Op::Unit);
                }
            }
            c.end_scope();
        }

        fn stmt(c: &mut C, ast: &Ast, s: Stmt) {
            match s.kind() {
                StmtKind::Fn(v) => {
                    let var = c.declare(v.name(ast).value().index());
                    c.begin_function();
                    for p in v.params(ast) {
                        c.param(p.value().index());
                    }
                    block(c, ast, v.body(ast));
                    let index = c.end_function();
                    c.emit(Op::Fn(index));
                    c.store(var);
                }
                StmtKind::Let(v) => {
                    expr(c, ast, v.value(ast));
                    let var = c.declare(v.name(ast).value().index());
                    c.store(var);
                }
                StmtKind::Expr(v) => {
                    expr(c, ast, v.inner(ast));
                    c.emit(Op::Pop);
                }
            }
        }

        fn expr(c: &mut C, ast: &Ast, e: Expr) {
            match e.kind() {
                ExprKind::ExprIf(v) => {
                    let mark = c.mark_jumps();
                    for b in v.branches(ast) {
                        expr(c, ast, b.cond(ast));
                        let next = c.emit(Op::JumpIfFalse(0));
                        block(c, ast, b.body(ast));
                        let end = c.emit(Op::Jump(0));
                        c.push_jump(end);
                        c.patch(next);
                    }
                    match v.tail(ast).into_option() {
                        Some(t) => block(c, ast, t),
                        None => {
                            c.emit(Op::Unit);
                        }
                    }
                    c.patch_jumps(mark);
                }
                ExprKind::ExprBinary(v) => match v.op() {
                    BinaryOp::And | BinaryOp::Or => {
                        expr(c, ast, v.lhs(ast));
                        let skip = c.emit(match v.op() {
                            BinaryOp::And => Op::JumpIfFalseOrPop(0),
                            _ => Op::JumpIfTrueOrPop(0),
                        });
                        expr(c, ast, v.rhs(ast));
                        c.patch(skip);
                    }
                    op => {
                        expr(c, ast, v.lhs(ast));
                        expr(c, ast, v.rhs(ast));
                        c.emit(Op::Binary(op as u8));
                    }
                },
                ExprKind::ExprUnary(v) => {
                    expr(c, ast, v.inner(ast));
                    c.emit(Op::Unary(v.op() as u8));
                }
                ExprKind::ExprCall(v) => {
                    expr(c, ast, v.callee(ast));
                    let args = v.args(ast);
                    for &a in args {
                        expr(c, ast, a);
                    }
                    c.emit(Op::Call(args.len() as u32));
                }
                ExprKind::ExprInt(v) => {
                    c.emit(Op::Int(int(ast, v)));
                }
                ExprKind::ExprStr(v) => c.string(v.value().index()),
                ExprKind::ExprIdent(v) => c.load(v.value().index()),
                ExprKind::ExprBlock(v) => block(c, ast, v),
            }
        }
    };
}

pub mod super_flat {
    compile_super_flat!(crate::ast_super_flat);

    fn int(ast: &Ast, v: ExprInt) -> i64 {
        ast.int(v.value()).unwrap()
    }
}

pub mod super_flat_unsafe {
    compile_super_flat!(crate::ast_super_flat_unsafe);

    fn int(ast: &Ast, v: ExprInt) -> i64 {
        ast.int(v.value())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str) -> Program {
        let tree = tree::compile(&crate::parser::parse(input).unwrap());
        let interned = interned::compile(&crate::parser_interned::parse(input).unwrap());
        let bump_arena = bumpalo::Bump::new();
        let bump = bump::compile(&crate::parser_bump::parse(input, &bump_arena).unwrap());
        let flat = flat::compile(&crate::parser_flat::parse(input).unwrap());
        let super_flat = super_flat::compile(&crate::parser_super_flat::parse(input).unwrap());
        let super_flat_unsafe =
            super_flat_unsafe::compile(&crate::parser_super_flat_unsafe::parse(input).unwrap());

        assert_eq!(tree, interned);
        assert_eq!(tree, bump);
        assert_eq!(tree, flat);
        assert_eq!(tree, super_flat);
        assert_eq!(tree, super_flat_unsafe);
        tree
    }

    #[test]
    fn all_compilers_agree() {
        let program = check(include_str!("../benches/inputs/input_100kb.simp"));
        let summary = program.summary();
        assert!(summary.functions > 1 && summary.ops > 0 && summary.globals > 0);
    }

    #[test]
    fn all_compilers_agree_on_test_inputs() {
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/inputs/valid");
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            let input = std::fs::read_to_string(&path).unwrap();
            if crate::parser::parse(&input).is_ok() {
                check(&input);
            }
        }
    }

    #[test]
    fn compiles_scopes_and_jumps() {
        let program = check(
            "fn f(a, b) { let c = a; if c { b } else if a { x } else { c || b } }\nlet y = f(1, 2);\n{ let y = 3; y }",
        );
        let f = &program.functions[0];
        assert_eq!(f.arity, 2);
        assert_eq!(f.slots, 3);
        assert_eq!(
            f.code,
            vec![
                Op::LoadLocal(0),
                Op::StoreLocal(2),
                Op::LoadLocal(2),
                Op::JumpIfFalse(6),
                Op::LoadLocal(1),
                Op::Jump(13),
                Op::LoadLocal(0),
                Op::JumpIfFalse(10),
                Op::LoadGlobal(1),
                Op::Jump(13),
                Op::LoadLocal(2),
                Op::JumpIfTrueOrPop(13),
                Op::LoadLocal(1),
                Op::Return,
            ]
        );
        let main = program.functions.last().unwrap();
        assert_eq!(
            main.code,
            vec![
                Op::Fn(0),
                Op::StoreGlobal(0),
                Op::LoadGlobal(0),
                Op::Int(1),
                Op::Int(2),
                Op::Call(2),
                Op::StoreGlobal(2),
                Op::Int(3),
                Op::StoreLocal(0),
                Op::LoadLocal(0),
                Op::Return,
            ]
        );
    }
}
