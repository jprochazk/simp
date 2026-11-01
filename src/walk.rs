#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Stats {
    pub stmts: u64,
    pub exprs: u64,
    pub int_sum: i64,
    pub ops: u64,
    pub idents: u64,
    pub ident_key: u64,
    pub strs: u64,
    pub str_key: u64,
}

impl Stats {
    #[inline]
    fn stmt(&mut self) {
        self.stmts += 1;
    }

    #[inline]
    fn expr(&mut self) {
        self.exprs += 1;
    }

    #[inline]
    fn int(&mut self, v: i64) {
        self.int_sum = self.int_sum.wrapping_add(v);
    }

    #[inline]
    fn binary(&mut self, op: u64) {
        self.ops = self.ops.wrapping_add(op + 1);
    }

    #[inline]
    fn unary(&mut self, op: u64) {
        self.ops = self.ops.wrapping_add((op + 1) << 32);
    }

    #[inline]
    fn ident(&mut self, key: u64) {
        self.idents += 1;
        self.ident_key = self.ident_key.wrapping_add(key);
    }

    #[inline]
    fn str(&mut self, key: u64) {
        self.strs += 1;
        self.str_key = self.str_key.wrapping_add(key);
    }
}

pub mod tree {
    use super::Stats;
    use crate::ast::*;

    pub fn walk(program: &Program) -> Stats {
        let mut s = Stats::default();
        block(&mut s, &program.body, program.tail.as_ref());
        s
    }

    fn block(s: &mut Stats, body: &[Stmt], tail: Option<&Expr>) {
        for st in body {
            stmt(s, st);
        }
        if let Some(tail) = tail {
            expr(s, tail);
        }
    }

    fn stmt(s: &mut Stats, st: &Stmt) {
        s.stmt();
        match st {
            Stmt::Fn(v) => {
                s.ident(v.name.len() as u64);
                for p in &v.params {
                    s.ident(p.len() as u64);
                }
                block(s, &v.body.body, v.body.tail.as_ref());
            }
            Stmt::Let(v) => {
                s.ident(v.name.len() as u64);
                expr(s, &v.value);
            }
            Stmt::Expr(v) => expr(s, v),
        }
    }

    fn expr(s: &mut Stats, e: &Expr) {
        s.expr();
        match e {
            Expr::If(v) => {
                for b in &v.branches {
                    expr(s, &b.cond);
                    block(s, &b.body.body, b.body.tail.as_ref());
                }
                if let Some(t) = &v.tail {
                    block(s, &t.body, t.tail.as_ref());
                }
            }
            Expr::Binary(v) => {
                s.binary(v.op as u64);
                expr(s, &v.lhs);
                expr(s, &v.rhs);
            }
            Expr::Unary(v) => {
                s.unary(v.op as u64);
                expr(s, &v.rhs);
            }
            Expr::Call(v) => {
                expr(s, &v.callee);
                for a in &v.args {
                    expr(s, a);
                }
            }
            Expr::Int(v) => s.int(v.value),
            Expr::Str(v) => s.str(v.value.len() as u64),
            Expr::Ident(v) => s.ident(v.name.len() as u64),
            Expr::Block(v) => block(s, &v.inner.body, v.inner.tail.as_ref()),
        }
    }
}

pub mod interned {
    use super::Stats;
    use crate::ast_interned::*;
    use crate::intern::Intern as _;

    pub fn walk(ast: &Ast) -> Stats {
        let mut s = Stats::default();
        block(&mut s, &ast.program.body, ast.program.tail.as_ref());
        s
    }

    fn block(s: &mut Stats, body: &[Stmt], tail: Option<&Expr>) {
        for st in body {
            stmt(s, st);
        }
        if let Some(tail) = tail {
            expr(s, tail);
        }
    }

    fn stmt(s: &mut Stats, st: &Stmt) {
        s.stmt();
        match st {
            Stmt::Fn(v) => {
                s.ident(v.name.index() as u64);
                for p in &v.params {
                    s.ident(p.index() as u64);
                }
                block(s, &v.body.body, v.body.tail.as_ref());
            }
            Stmt::Let(v) => {
                s.ident(v.name.index() as u64);
                expr(s, &v.value);
            }
            Stmt::Expr(v) => expr(s, v),
        }
    }

    fn expr(s: &mut Stats, e: &Expr) {
        s.expr();
        match e {
            Expr::If(v) => {
                for b in &v.branches {
                    expr(s, &b.cond);
                    block(s, &b.body.body, b.body.tail.as_ref());
                }
                if let Some(t) = &v.tail {
                    block(s, &t.body, t.tail.as_ref());
                }
            }
            Expr::Binary(v) => {
                s.binary(v.op as u64);
                expr(s, &v.lhs);
                expr(s, &v.rhs);
            }
            Expr::Unary(v) => {
                s.unary(v.op as u64);
                expr(s, &v.rhs);
            }
            Expr::Call(v) => {
                expr(s, &v.callee);
                for a in &v.args {
                    expr(s, a);
                }
            }
            Expr::Int(v) => s.int(v.value),
            Expr::Str(v) => s.str(v.value.index() as u64),
            Expr::Ident(v) => s.ident(v.name.index() as u64),
            Expr::Block(v) => block(s, &v.inner.body, v.inner.tail.as_ref()),
        }
    }
}

pub mod bump {
    use super::Stats;
    use crate::ast_bump::*;
    use crate::intern::Intern as _;

    pub fn walk(ast: &Ast) -> Stats {
        let mut s = Stats::default();
        block(&mut s, &ast.program.body, ast.program.tail);
        s
    }

    fn block(s: &mut Stats, body: &[Stmt], tail: Option<&Expr>) {
        for st in body {
            stmt(s, st);
        }
        if let Some(tail) = tail {
            expr(s, tail);
        }
    }

    fn stmt(s: &mut Stats, st: &Stmt) {
        s.stmt();
        match st {
            Stmt::Fn(v) => {
                s.ident(v.name.index() as u64);
                for p in &v.params {
                    s.ident(p.index() as u64);
                }
                block(s, &v.body.body, v.body.tail);
            }
            Stmt::Let(v) => {
                s.ident(v.name.index() as u64);
                expr(s, &v.value);
            }
            Stmt::Expr(v) => expr(s, v),
        }
    }

    fn expr(s: &mut Stats, e: &Expr) {
        s.expr();
        match e {
            Expr::If(v) => {
                for b in &v.branches {
                    expr(s, &b.cond);
                    block(s, &b.body.body, b.body.tail);
                }
                if let Some(t) = &v.tail {
                    block(s, &t.body, t.tail);
                }
            }
            Expr::Binary(v) => {
                s.binary(v.op as u64);
                expr(s, &v.lhs);
                expr(s, &v.rhs);
            }
            Expr::Unary(v) => {
                s.unary(v.op as u64);
                expr(s, &v.rhs);
            }
            Expr::Call(v) => {
                expr(s, &v.callee);
                for a in &v.args {
                    expr(s, a);
                }
            }
            Expr::Int(v) => s.int(v.value),
            Expr::Str(v) => s.str(v.value.index() as u64),
            Expr::Ident(v) => s.ident(v.name.index() as u64),
            Expr::Block(v) => block(s, &v.inner.body, v.inner.tail),
        }
    }
}

pub mod flat {
    use super::Stats;
    use crate::ast_flat::*;
    use crate::intern::Intern as _;

    pub fn walk(ast: &Ast) -> Stats {
        let mut s = Stats::default();
        block(&mut s, ast, &ast.program.body, ast.program.tail);
        s
    }

    fn block(s: &mut Stats, ast: &Ast, body: &[StmtId], tail: Option<ExprId>) {
        for &st in body {
            stmt(s, ast, st);
        }
        if let Some(tail) = tail {
            expr(s, ast, tail);
        }
    }

    fn block_expr(s: &mut Stats, ast: &Ast, id: ExprId) {
        match ast.expr(id) {
            Expr::Block(v) => block(s, ast, &v.body, v.tail),
            _ => unreachable!(),
        }
    }

    fn stmt(s: &mut Stats, ast: &Ast, id: StmtId) {
        s.stmt();
        match ast.stmt(id) {
            Stmt::Fn(v) => {
                s.ident(v.name.index() as u64);
                for p in &v.params {
                    s.ident(p.index() as u64);
                }
                block_expr(s, ast, v.body);
            }
            Stmt::Let(v) => {
                s.ident(v.name.index() as u64);
                expr(s, ast, v.value);
            }
            Stmt::Expr(v) => expr(s, ast, *v),
        }
    }

    fn expr(s: &mut Stats, ast: &Ast, id: ExprId) {
        s.expr();
        match ast.expr(id) {
            Expr::If(v) => {
                for b in &v.branches {
                    expr(s, ast, b.cond);
                    block_expr(s, ast, b.body);
                }
                if let Some(t) = v.tail {
                    block_expr(s, ast, t);
                }
            }
            Expr::Binary(v) => {
                s.binary(v.op as u64);
                expr(s, ast, v.lhs);
                expr(s, ast, v.rhs);
            }
            Expr::Unary(v) => {
                s.unary(v.op as u64);
                expr(s, ast, v.rhs);
            }
            Expr::Call(v) => {
                expr(s, ast, v.callee);
                for &a in &v.args {
                    expr(s, ast, a);
                }
            }
            Expr::Int(v) => s.int(v.value),
            Expr::Str(v) => s.str(v.value.index() as u64),
            Expr::Ident(v) => s.ident(v.name.index() as u64),
            Expr::Block(v) => block(s, ast, &v.body, v.tail),
        }
    }
}

pub mod super_flat {
    use super::Stats;
    use crate::ast_super_flat::*;
    use crate::intern::Intern as _;

    pub fn walk(ast: &Ast) -> Stats {
        let mut s = Stats::default();
        let root = ast.root();
        block(&mut s, ast, root.body(ast), root.tail(ast));
        s
    }

    fn block(s: &mut Stats, ast: &Ast, body: &[Stmt], tail: Opt<Expr>) {
        for &st in body {
            stmt(s, ast, st);
        }
        if let Some(tail) = tail.into_option() {
            expr(s, ast, tail);
        }
    }

    fn block_expr(s: &mut Stats, ast: &Ast, v: ExprBlock) {
        block(s, ast, v.body(ast), v.tail(ast));
    }

    fn ident(s: &mut Stats, v: ExprIdent) {
        s.ident(v.value().index() as u64);
    }

    fn stmt(s: &mut Stats, ast: &Ast, st: Stmt) {
        s.stmt();
        match st.kind() {
            StmtKind::Fn(v) => {
                ident(s, v.name(ast));
                for &p in v.params(ast) {
                    ident(s, p);
                }
                block_expr(s, ast, v.body(ast));
            }
            StmtKind::Let(v) => {
                ident(s, v.name(ast));
                expr(s, ast, v.value(ast));
            }
            StmtKind::Expr(v) => expr(s, ast, v.inner(ast)),
        }
    }

    fn expr(s: &mut Stats, ast: &Ast, e: Expr) {
        s.expr();
        match e.kind() {
            ExprKind::ExprIf(v) => {
                for &b in v.branches(ast) {
                    expr(s, ast, b.cond(ast));
                    block_expr(s, ast, b.body(ast));
                }
                if let Some(t) = v.tail(ast).into_option() {
                    block_expr(s, ast, t);
                }
            }
            ExprKind::ExprBinary(v) => {
                s.binary(v.op() as u64);
                expr(s, ast, v.lhs(ast));
                expr(s, ast, v.rhs(ast));
            }
            ExprKind::ExprUnary(v) => {
                s.unary(v.op() as u64);
                expr(s, ast, v.inner(ast));
            }
            ExprKind::ExprCall(v) => {
                expr(s, ast, v.callee(ast));
                for &a in v.args(ast) {
                    expr(s, ast, a);
                }
            }
            ExprKind::ExprInt(v) => s.int(ast.int(v.value()).unwrap()),
            ExprKind::ExprStr(v) => s.str(v.value().index() as u64),
            ExprKind::ExprIdent(v) => ident(s, v),
            ExprKind::ExprBlock(v) => block_expr(s, ast, v),
        }
    }
}

pub mod super_flat_unsafe {
    use super::Stats;
    use crate::ast_super_flat_unsafe::*;
    use crate::intern::Intern as _;

    pub fn walk(ast: &Ast) -> Stats {
        let mut s = Stats::default();
        let root = ast.root();
        block(&mut s, ast, root.body(ast), root.tail(ast));
        s
    }

    fn block(s: &mut Stats, ast: &Ast, body: &[Stmt], tail: Opt<Expr>) {
        for &st in body {
            stmt(s, ast, st);
        }
        if let Some(tail) = tail.into_option() {
            expr(s, ast, tail);
        }
    }

    fn block_expr(s: &mut Stats, ast: &Ast, v: ExprBlock) {
        block(s, ast, v.body(ast), v.tail(ast));
    }

    fn ident(s: &mut Stats, v: ExprIdent) {
        s.ident(v.value().index() as u64);
    }

    fn stmt(s: &mut Stats, ast: &Ast, st: Stmt) {
        s.stmt();
        match st.kind() {
            StmtKind::Fn(v) => {
                ident(s, v.name(ast));
                for &p in v.params(ast) {
                    ident(s, p);
                }
                block_expr(s, ast, v.body(ast));
            }
            StmtKind::Let(v) => {
                ident(s, v.name(ast));
                expr(s, ast, v.value(ast));
            }
            StmtKind::Expr(v) => expr(s, ast, v.inner(ast)),
        }
    }

    fn expr(s: &mut Stats, ast: &Ast, e: Expr) {
        s.expr();
        match e.kind() {
            ExprKind::ExprIf(v) => {
                for &b in v.branches(ast) {
                    expr(s, ast, b.cond(ast));
                    block_expr(s, ast, b.body(ast));
                }
                if let Some(t) = v.tail(ast).into_option() {
                    block_expr(s, ast, t);
                }
            }
            ExprKind::ExprBinary(v) => {
                s.binary(v.op() as u64);
                expr(s, ast, v.lhs(ast));
                expr(s, ast, v.rhs(ast));
            }
            ExprKind::ExprUnary(v) => {
                s.unary(v.op() as u64);
                expr(s, ast, v.inner(ast));
            }
            ExprKind::ExprCall(v) => {
                expr(s, ast, v.callee(ast));
                for &a in v.args(ast) {
                    expr(s, ast, a);
                }
            }
            ExprKind::ExprInt(v) => s.int(ast.int(v.value())),
            ExprKind::ExprStr(v) => s.str(v.value().index() as u64),
            ExprKind::ExprIdent(v) => ident(s, v),
            ExprKind::ExprBlock(v) => block_expr(s, ast, v),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str) -> Stats {
        let tree = tree::walk(&crate::parser::parse(input).unwrap());
        let interned = interned::walk(&crate::parser_interned::parse(input).unwrap());
        let bump_arena = bumpalo::Bump::new();
        let bump = bump::walk(&crate::parser_bump::parse(input, &bump_arena).unwrap());
        let flat = flat::walk(&crate::parser_flat::parse(input).unwrap());
        let super_flat = super_flat::walk(&crate::parser_super_flat::parse(input).unwrap());
        let super_flat_unsafe =
            super_flat_unsafe::walk(&crate::parser_super_flat_unsafe::parse(input).unwrap());

        assert_eq!(interned, bump);
        assert_eq!(interned, flat);
        assert_eq!(interned, super_flat);
        assert_eq!(interned, super_flat_unsafe);

        let without_keys = |s: Stats| Stats {
            ident_key: 0,
            str_key: 0,
            ..s
        };
        assert_eq!(without_keys(tree), without_keys(interned));
        interned
    }

    #[test]
    fn all_walkers_agree() {
        let stats = check(include_str!("../benches/inputs/input_100kb.simp"));
        assert!(stats.stmts > 0 && stats.exprs > 0 && stats.int_sum != 0 && stats.ops > 0);
    }

    #[test]
    fn all_walkers_agree_on_test_inputs() {
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/inputs/valid");
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            let input = std::fs::read_to_string(&path).unwrap();
            if crate::parser::parse(&input).is_ok() {
                check(&input);
            }
        }
    }
}
