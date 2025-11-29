use std::{
    marker::PhantomData,
    mem::{transmute, transmute_copy},
};

use crate::intern::{Intern, Interner, simple::SimpleInterner};

pub struct Ast {
    root: Root,
    nodes: Vec<Packed>,
    ints: SimpleInterner<IntId, i64>,
    strings: Interner<StrId>,
    idents: Interner<IdentId>,
}

impl Ast {
    pub fn builder() -> AstBuilder {
        AstBuilder {
            nodes: Vec::new(),
            ints: SimpleInterner::new(),
            strings: Interner::new(),
            idents: Interner::new(),
        }
    }

    pub fn root(&self) -> Root {
        self.root
    }

    pub fn int(&self, id: IntId) -> Option<i64> {
        self.ints.get(id).copied()
    }

    pub fn str(&self, id: StrId) -> Option<&str> {
        self.strings.get(id)
    }

    pub fn ident(&self, id: IdentId) -> Option<&str> {
        self.idents.get(id)
    }
}

pub struct AstBuilder {
    nodes: Vec<Packed>,
    ints: SimpleInterner<IntId, i64>,
    strings: Interner<StrId>,
    idents: Interner<IdentId>,
}

impl AstBuilder {
    pub fn intern_int(&mut self, value: i64) -> IntId {
        self.ints.intern(value)
    }

    pub fn intern_string(&mut self, str: &str) -> StrId {
        self.strings.intern(str)
    }

    pub fn intern_ident(&mut self, ident: &str) -> IdentId {
        self.idents.intern(ident)
    }

    pub fn finish(self, root: Root) -> Ast {
        Ast {
            root,
            nodes: self.nodes,
            ints: self.ints,
            strings: self.strings,
            idents: self.idents,
        }
    }
}

declare_intern_id!(pub IntId);
declare_intern_id!(pub StrId);
declare_intern_id!(pub IdentId);

impl private::Sealed for IntId {}
impl Inline56 for IntId {
    fn into_u56(self) -> u64 {
        self.index() as u64
    }

    fn from_u56(v: u64) -> Self {
        // SAFETY: `self` is `u32`, so it can be stored as-is in a `u56`
        unsafe { Self::from_index(v as u32) }
    }
}
impl<'a> std::fmt::Debug for DebugInline56<'a, IntId> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.ints.get(self.1).unwrap().fmt(f)
    }
}

impl private::Sealed for StrId {}
impl Inline56 for StrId {
    fn into_u56(self) -> u64 {
        self.index() as u64
    }

    fn from_u56(v: u64) -> Self {
        // SAFETY: `self` is `u32`, so it can be stored as-is in a `u56`
        unsafe { Self::from_index(v as u32) }
    }
}
impl<'a> std::fmt::Debug for DebugInline56<'a, StrId> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.strings.get(self.1).unwrap().fmt(f)
    }
}

impl private::Sealed for IdentId {}
impl Inline56 for IdentId {
    fn into_u56(self) -> u64 {
        self.index() as u64
    }

    fn from_u56(v: u64) -> Self {
        // SAFETY: `self` is `u32`, so it can be stored as-is in a `u56`
        unsafe { Self::from_index(v as u32) }
    }
}
impl<'a> std::fmt::Debug for DebugInline56<'a, IdentId> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.idents.get(self.1).unwrap().fmt(f)
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Packed {
    repr: u64,
}

const U56_MASK: u64 = 0b00000000_11111111_11111111_11111111_11111111_11111111_11111111_11111111;
const U24_MASK: u64 = 0b00000000_11111111_11111111_11111111;

const TAG_MASK: u64 = 0b11111111;

impl Packed {
    fn tag(self) -> u8 {
        (self.repr & TAG_MASK) as u8
    }

    fn leaf(self) -> Leaf {
        Leaf { packed: self }
    }

    fn tree(self) -> Tree {
        Tree { packed: self }
    }

    fn dyn_tree(self) -> DynTree {
        DynTree { packed: self }
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
struct Leaf {
    packed: Packed,
}

impl Leaf {
    /// `[ value:56 tag:8 ]`
    fn new(tag: u8, value: u64) -> Self {
        let tag = tag as u64;

        let packed = tag;
        let packed = packed | ((value & U56_MASK) << 8);
        Self {
            packed: Packed { repr: packed },
        }
    }

    fn value(self) -> u64 {
        self.packed.repr >> 8
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
struct Tree {
    packed: Packed,
}

impl Tree {
    /// `[ value:32 child:24 tag:8 ]`
    fn new(tag: u8, child: u32, value: u32) -> Self {
        let tag = tag as u64;
        let child = child as u64;
        let value = value as u64;

        let packed = tag;
        let packed = packed | ((child & U24_MASK) << 8);
        let packed = packed | (value << 32);
        Self {
            packed: Packed { repr: packed },
        }
    }

    fn child(self) -> u32 {
        ((self.packed.repr >> 8) & U24_MASK) as u32
    }

    fn value(self) -> u32 {
        (self.packed.repr >> 32) as u32
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
struct DynTree {
    packed: Packed,
}

impl DynTree {
    /// `[ len:32 child:24 tag:8 ]`
    fn new(tag: u8, child: u32, len: u32) -> Self {
        let tag = tag as u64;
        let child = child as u64;
        let len = len as u64;

        let packed = tag;
        let packed = packed | ((child & U24_MASK) << 8);
        let packed = packed | (len << 32);
        Self {
            packed: Packed { repr: packed },
        }
    }

    fn child(self) -> u32 {
        ((self.packed.repr >> 8) & U24_MASK) as u32
    }

    fn len(self) -> u32 {
        (self.packed.repr >> 32) as u32
    }
}

#[repr(transparent)]
pub struct Opt<T> {
    packed: Packed,
    _marker: PhantomData<T>,
}

impl<T: Node> Opt<T> {
    pub fn some(v: T) -> Self {
        Self {
            // SAFETY: all `Node` have the same size (u64)
            packed: unsafe { transmute_copy(&v) },
            _marker: PhantomData,
        }
    }

    pub fn none() -> Self {
        Self {
            packed: Leaf::new(u8::MAX, 0).packed,
            _marker: PhantomData,
        }
    }

    pub fn is_some(self) -> bool {
        !self.is_none()
    }

    pub fn is_none(self) -> bool {
        self.packed.tag() == u8::MAX
    }

    pub fn into_option(self) -> Option<T> {
        if self.is_some() {
            // SAFETY: all `Node` have the same size (u64)
            Some(unsafe { transmute_copy(&self.packed) })
        } else {
            None
        }
    }
}

/// SAFETY: T is `#[repr(transparent)]` over `Packed`
unsafe impl<T: Node> Node for Opt<T> {
    fn check_tag(tag: u8) -> bool {
        tag == 255 || T::check_tag(tag)
    }
}

impl<'a, T: Node> std::fmt::Debug for DebugNode<'a, Opt<T>>
where
    DebugNode<'a, T>: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.1
            .into_option()
            .map(|node| DebugNode(self.0, node))
            .fmt(f)
    }
}

impl<T: private::Sealed> private::Sealed for Opt<T> {}

impl<T: Node> Clone for Opt<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Node> Copy for Opt<T> {}

/// ## Safety
///
/// - Implementations of this trait must be on types which are
///   `#[repr(transparent)]` structs over `Packed`.
pub unsafe trait Node: Sized + Copy + private::Sealed {
    fn check_tag(tag: u8) -> bool;
}

pub trait Inline24: Sized + Copy + private::Sealed {
    fn into_u24(self) -> u32;
    fn from_u24(v: u32) -> Self;
}

pub trait Inline56: Sized + Copy + private::Sealed {
    fn into_u56(self) -> u64;
    fn from_u56(v: u64) -> Self;
}

pub trait NodeStorage {
    fn get(&self, index: usize) -> Option<Packed>;
    fn get_slice(&self, range: std::ops::Range<usize>) -> Option<&[Packed]>;
}

impl NodeStorage for AstBuilder {
    fn get(&self, index: usize) -> Option<Packed> {
        self.nodes.get(index).copied()
    }

    fn get_slice(&self, range: std::ops::Range<usize>) -> Option<&[Packed]> {
        self.nodes.get(range)
    }
}

impl NodeStorage for Ast {
    fn get(&self, index: usize) -> Option<Packed> {
        self.nodes.get(index).copied()
    }

    fn get_slice(&self, range: std::ops::Range<usize>) -> Option<&[Packed]> {
        self.nodes.get(range)
    }
}

mod private {
    pub trait Sealed {}
}

macro_rules! count {
    ($ident:ident $($tail:ident)*) => { 1 + count!($($tail)*) };
    ($ident:ident) => { 1 };
    () => { 0 };
}

macro_rules! declare_node {
    (
        #[leaf]
        $vis:vis struct $name:ident;
    ) => {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        $vis struct $name {
            packed: Packed,
        }

        impl $name {
            const NUM_STATIC_FIELDS: usize = 0;

            pub fn pack(
                ast: &mut AstBuilder,
            ) -> Self {
                Self {
                    packed: Leaf::new(
                        Tag::$name as u8,
                        0,
                    ).packed
                }
            }
        }

        impl private::Sealed for $name {}
        /// SAFETY: T is `#[repr(transparent)]` over `Packed`
        unsafe impl Node for $name {
            fn check_tag(tag: u8) -> bool {
                tag == Tag::$name as u8
            }
        }

        impl<'a> std::fmt::Debug for DebugNode<'a, $name> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", stringify!($name));
            }
        }

        impl $name {
            pub fn debug<'a>(self, ast: &'a Ast) -> DebugNode<'a, Self> {
                DebugNode(ast, self)
            }
        }
    };

    (
        #[leaf]
        $vis:vis struct $name:ident ($inline_name:ident : $inline_ty:ty);
    ) => {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        $vis struct $name {
            packed: Packed,
        }

        impl $name {
            pub fn pack(
                $inline_name: $inline_ty,
            ) -> Self {
                let value = $inline_name.into_u56();
                assert!(value <= U56_MASK);

                Self {
                    packed: Leaf::new(
                        Tag::$name as u8,
                        value,
                    ).packed
                }
            }

            pub fn $inline_name(self) -> $inline_ty {
                debug_assert!(Self::check_tag(self.packed.tag()));

                let value = self.packed.leaf().value();
                <$inline_ty as Inline56>::from_u56(value)
            }
        }

        impl private::Sealed for $name {}
        /// SAFETY: T is `#[repr(transparent)]` over `Packed`
        unsafe impl Node for $name {
            fn check_tag(tag: u8) -> bool {
                tag == Tag::$name as u8
            }
        }

        impl<'a> std::fmt::Debug for DebugNode<'a, $name> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_tuple(stringify!($name))
                    .field(
                        &DebugInline56(self.0, self.1.$inline_name())
                    )
                    .finish()
            }
        }

        impl $name {
            pub fn debug<'a>(self, ast: &'a Ast) -> DebugNode<'a, Self> {
                DebugNode(ast, self)
            }
        }
    };

    (
        #[tree]
        $vis:vis struct $name:ident {
            $($field_name:ident : $field_ty:ty = $field_index:literal),*
            $(,)?
        }
    ) => {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        $vis struct $name {
            packed: Packed,
        }

        impl $name {
            pub fn pack(
                ast: &mut AstBuilder,

                $($field_name : $field_ty,)*
            ) -> Self {
                let index = ast.nodes.len() as u64;
                assert!(index <= u32::MAX as u64);
                let index = index as u32;

                $(
                    ast.nodes.push($field_name.packed);
                )*


                Self {
                    packed: Tree::new(
                        Tag::$name as u8,
                        index,
                        0,
                    ).packed
                }
            }

            $(
              pub fn $field_name(self, ast: &impl NodeStorage) -> $field_ty {
                debug_assert!(Self::check_tag(self.packed.tag()));

                let index = self.packed.tree().child() as usize
                            + $field_index;
                let node = ast.get(index).unwrap();
                // SAFETY: this field is guaranteed to be this type
                unsafe { transmute_node(node) }
              }
            )*
        }

        impl private::Sealed for $name {}
        /// SAFETY: T is `#[repr(transparent)]` over `Packed`
        unsafe impl Node for $name {
            fn check_tag(tag: u8) -> bool {
                tag == Tag::$name as u8
            }
        }

        impl<'a> std::fmt::Debug for DebugNode<'a, $name> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($name))
                    $(
                      .field(
                          stringify!($field_name),
                          &DebugNode(self.0, self.1.$field_name(self.0))
                      )
                    )*
                    .finish()
            }
        }

        impl $name {
            pub fn debug<'a>(self, ast: &'a Ast) -> DebugNode<'a, Self> {
                DebugNode(ast, self)
            }
        }
    };

    (
        #[tree]
        $vis:vis struct $name:ident ($inline_name:ident : $inline_ty:ty) {
            $($field_name:ident : $field_ty:ty = $field_index:literal),*
            $(,)?
        }
    ) => {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        $vis struct $name {
            packed: Packed,
        }

        impl $name {
            pub fn pack(
                ast: &mut AstBuilder,

                $($field_name : $field_ty,)*

                $inline_name: $inline_ty,
            ) -> Self {
                let index = ast.nodes.len() as u64;
                assert!(index <= u32::MAX as u64);
                let index = index as u32;

                let value = $inline_name.into_u24();
                assert!(value <= U24_MASK as u32);

                $(
                    ast.nodes.push($field_name.packed);
                )*


                Self {
                    packed: Tree::new(
                        Tag::$name as u8,
                        index,
                        value,
                    ).packed
                }
            }

            $(
              pub fn $field_name(self, ast: &impl NodeStorage) -> $field_ty {
                debug_assert!(Self::check_tag(self.packed.tag()));

                let index = self.packed.tree().child() as usize
                            + $field_index;
                let node = ast.get(index).unwrap();
                // SAFETY: this field is guaranteed to be this type
                unsafe { transmute_node(node) }
              }
            )*

            pub fn $inline_name(self) -> $inline_ty {
                debug_assert!(Self::check_tag(self.packed.tag()));

                let value = self.packed.tree().value();
                <$inline_ty as Inline24>::from_u24(value)
            }
        }

        impl private::Sealed for $name {}
        /// SAFETY: T is `#[repr(transparent)]` over `Packed`
        unsafe impl Node for $name {
            fn check_tag(tag: u8) -> bool {
                tag == Tag::$name as u8
            }
        }

        impl<'a> std::fmt::Debug for DebugNode<'a, $name> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($name))
                    .field(
                        stringify!($inline_name),
                        &DebugInline24(self.1.$inline_name())
                    )
                    $(
                      .field(
                          stringify!($field_name),
                          &DebugNode(self.0, self.1.$field_name(self.0))
                      )
                    )*
                    .finish()
            }
        }

        impl $name {
            pub fn debug<'a>(self, ast: &'a Ast) -> DebugNode<'a, Self> {
                DebugNode(ast, self)
            }
        }
    };

    (
        #[dyn_tree]
        $vis:vis struct $name:ident {
            $($field_name:ident : $field_ty:ty = $field_index:literal,)*

            #[tail]
            $tail_name:ident : [$tail_ty:ty] $(,)?
        }
    ) => {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        $vis struct $name {
            packed: Packed,
        }

        impl $name {
            const NUM_STATIC_FIELDS: usize = count!($($field_name)*);

            pub fn pack(
                ast: &mut AstBuilder,

                $($field_name : $field_ty,)*

                $tail_name : &[$tail_ty],
            ) -> Self {
                let index = ast.nodes.len() as u64;
                assert!(index <= u32::MAX as u64);
                let index = index as u32;

                let len = $tail_name.len() as u64;
                assert!(len <= U24_MASK);
                let len = len as u32;

                $(
                    ast.nodes.push($field_name.packed);
                )*

                for node in $tail_name {
                    ast.nodes.push(node.packed);
                }

                Self {
                    packed: DynTree::new(
                        Tag::$name as u8,
                        index,
                        len,
                    ).packed
                }
            }

            $(
              pub fn $field_name(self, ast: &impl NodeStorage) -> $field_ty {
                debug_assert!(Self::check_tag(self.packed.tag()));

                let index = self.packed.dyn_tree().child() as usize
                            + $field_index;
                // SAFETY: this field is guaranteed to be this type
                let node = ast.get(index).unwrap();
                unsafe { transmute_node(node) }
              }
            )*

            pub fn $tail_name(self, ast: &impl NodeStorage) -> &[$tail_ty] {
                debug_assert!(Self::check_tag(self.packed.tag()));

                let index = self.packed.dyn_tree().child() as usize
                            + Self::NUM_STATIC_FIELDS;
                let len = self.packed.dyn_tree().len() as usize;
                let nodes = ast.get_slice(index..index+len).unwrap();
                // SAFETY: this field is guaranteed to be this type
                unsafe { transmute_node_slice(nodes) }
            }
        }

        impl private::Sealed for $name {}
        /// SAFETY: T is `#[repr(transparent)]` over `Packed`
        unsafe impl Node for $name {
            fn check_tag(tag: u8) -> bool {
                tag == Tag::$name as u8
            }
        }

        impl<'a> std::fmt::Debug for DebugNode<'a, $name> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($name))
                    $(
                      .field(
                          stringify!($field_name),
                          &DebugNode(self.0, self.1.$field_name(self.0))
                      )
                    )*
                    .field(
                        stringify!($tail_name),
                        &DebugNodeList(self.0, self.1.$tail_name(self.0))
                    )
                    .finish()
            }
        }

        impl $name {
            pub fn debug<'a>(self, ast: &'a Ast) -> DebugNode<'a, Self> {
                DebugNode(ast, self)
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Tag {
    Root = 1,

    // Statements
    StmtFn = 2,
    StmtLet = 3,
    StmtExpr = 4,

    // Expressions
    ExprIf = 5,
    ExprBinary = 6,
    ExprUnary = 7,
    ExprCall = 8,
    ExprInt = 9,
    ExprStr = 10,
    ExprIdent = 11,
    ExprBlock = 12,

    // Misc
    IfBranch = 13,

    None = 255,
}

const STMT_MIN: u8 = Tag::StmtFn as u8;
const STMT_MAX: u8 = Tag::StmtExpr as u8;

const EXPR_MIN: u8 = Tag::ExprIf as u8;
const EXPR_MAX: u8 = Tag::ExprBlock as u8;

fn tag(packed: Packed) -> Tag {
    // SAFETY: every `packed` holds a valid `Tag`
    unsafe { transmute(packed.tag()) }
}

unsafe fn transmute_node<T: Node>(packed: Packed) -> T {
    debug_assert!(T::check_tag(packed.tag()));
    unsafe { transmute_copy(&packed) }
}

unsafe fn transmute_node_slice<T: Node>(packed: &[Packed]) -> &[T] {
    debug_assert!(packed.iter().all(|node| T::check_tag(node.tag())));
    unsafe { transmute(packed) }
}

declare_node! {
    #[dyn_tree]
    pub struct Root {
        tail: Opt<Expr> = 0,

        #[tail]
        body: [Stmt],
    }
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Stmt {
    packed: Packed,
}

impl private::Sealed for Stmt {}
/// SAFETY: T is `#[repr(transparent)]` over `Packed`
unsafe impl Node for Stmt {
    fn check_tag(tag: u8) -> bool {
        (STMT_MIN..=STMT_MAX).contains(&tag)
    }
}

impl<'a> std::fmt::Debug for DebugNode<'a, Stmt> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.1.kind() {
            StmtKind::Fn(inner) => DebugNode(self.0, inner).fmt(f),
            StmtKind::Let(inner) => DebugNode(self.0, inner).fmt(f),
            StmtKind::Expr(inner) => DebugNode(self.0, inner).fmt(f),
        }
    }
}

impl Stmt {
    pub fn debug<'a>(self, ast: &'a Ast) -> DebugNode<'a, Self> {
        DebugNode(ast, self)
    }
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum StmtKind {
    Fn(StmtFn),
    Let(StmtLet),
    Expr(StmtExpr),
}

impl Stmt {
    pub fn pack(kind: StmtKind) -> Stmt {
        match kind {
            StmtKind::Fn(inner) => Stmt {
                packed: inner.packed,
            },
            StmtKind::Let(inner) => Stmt {
                packed: inner.packed,
            },
            StmtKind::Expr(inner) => Stmt {
                packed: inner.packed,
            },
        }
    }

    pub fn kind(self) -> StmtKind {
        debug_assert!(Self::check_tag(self.packed.tag()));

        match tag(self.packed) {
            Tag::StmtFn => StmtKind::Fn(StmtFn {
                packed: self.packed,
            }),
            Tag::StmtLet => StmtKind::Let(StmtLet {
                packed: self.packed,
            }),
            Tag::StmtExpr => StmtKind::Expr(StmtExpr {
                packed: self.packed,
            }),
            _ => unreachable!(),
        }
    }
}

macro_rules! into_stmt {
    ($name:ident) => {
        impl From<$name> for Stmt {
            fn from(v: $name) -> Self {
                Self { packed: v.packed }
            }
        }
    };
}

declare_node! {
    #[dyn_tree]
    pub struct StmtFn {
        name: ExprIdent = 0,
        body: ExprBlock = 1,
        #[tail]
        params: [ExprIdent],
    }
}
into_stmt!(StmtFn);

declare_node! {
    #[tree]
    pub struct StmtLet {
        name: ExprIdent = 0,
        value: Expr = 1,
    }
}
into_stmt!(StmtLet);

declare_node! {
    #[tree]
    pub struct StmtExpr {
        inner: Expr = 0,
    }
}
into_stmt!(StmtExpr);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Expr {
    packed: Packed,
}

impl private::Sealed for Expr {}
/// SAFETY: T is `#[repr(transparent)]` over `Packed`
unsafe impl Node for Expr {
    fn check_tag(tag: u8) -> bool {
        (EXPR_MIN..=EXPR_MAX).contains(&tag)
    }
}

impl<'a> std::fmt::Debug for DebugNode<'a, Expr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.1.kind() {
            ExprKind::ExprIf(inner) => DebugNode(self.0, inner).fmt(f),
            ExprKind::ExprBinary(inner) => DebugNode(self.0, inner).fmt(f),
            ExprKind::ExprUnary(inner) => DebugNode(self.0, inner).fmt(f),
            ExprKind::ExprCall(inner) => DebugNode(self.0, inner).fmt(f),
            ExprKind::ExprInt(inner) => DebugNode(self.0, inner).fmt(f),
            ExprKind::ExprStr(inner) => DebugNode(self.0, inner).fmt(f),
            ExprKind::ExprIdent(inner) => DebugNode(self.0, inner).fmt(f),
            ExprKind::ExprBlock(inner) => DebugNode(self.0, inner).fmt(f),
        }
    }
}

impl Expr {
    pub fn debug<'a>(self, ast: &'a Ast) -> DebugNode<'a, Self> {
        DebugNode(ast, self)
    }
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum ExprKind {
    ExprIf(ExprIf),
    ExprBinary(ExprBinary),
    ExprUnary(ExprUnary),
    ExprCall(ExprCall),
    ExprInt(ExprInt),
    ExprStr(ExprStr),
    ExprIdent(ExprIdent),
    ExprBlock(ExprBlock),
}

impl Expr {
    pub fn kind(self) -> ExprKind {
        debug_assert!(Self::check_tag(self.packed.tag()));

        match tag(self.packed) {
            Tag::ExprIf => ExprKind::ExprIf(ExprIf {
                packed: self.packed,
            }),
            Tag::ExprBinary => ExprKind::ExprBinary(ExprBinary {
                packed: self.packed,
            }),
            Tag::ExprUnary => ExprKind::ExprUnary(ExprUnary {
                packed: self.packed,
            }),
            Tag::ExprCall => ExprKind::ExprCall(ExprCall {
                packed: self.packed,
            }),
            Tag::ExprInt => ExprKind::ExprInt(ExprInt {
                packed: self.packed,
            }),
            Tag::ExprStr => ExprKind::ExprStr(ExprStr {
                packed: self.packed,
            }),
            Tag::ExprIdent => ExprKind::ExprIdent(ExprIdent {
                packed: self.packed,
            }),
            Tag::ExprBlock => ExprKind::ExprBlock(ExprBlock {
                packed: self.packed,
            }),
            _ => unreachable!(),
        }
    }
}

macro_rules! into_expr {
    ($name:ident) => {
        impl From<$name> for Expr {
            fn from(v: $name) -> Self {
                Self { packed: v.packed }
            }
        }
    };
}

declare_node! {
    #[dyn_tree]
    pub struct ExprIf {
        tail: Opt<ExprBlock> = 0,

        #[tail]
        branches: [IfBranch],
    }
}
into_expr!(ExprIf);

declare_node! {
    #[tree]
    pub struct IfBranch {
        cond: Expr = 0,
        body: ExprBlock = 1,
    }
}

declare_node! {
    #[tree]
    pub struct ExprBinary (op: BinaryOp) {
        lhs: Expr = 0,
        rhs: Expr = 1,
    }
}
into_expr!(ExprBinary);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
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

impl private::Sealed for BinaryOp {}
impl Inline24 for BinaryOp {
    fn into_u24(self) -> u32 {
        self as u32
    }

    fn from_u24(v: u32) -> Self {
        assert!(v <= BinaryOp::GreaterOrEqual as u32);
        // SAFETY: we checked that `v` is a valid `BinaryOp`
        unsafe { transmute(v as u8) }
    }
}

impl std::fmt::Debug for DebugInline24<BinaryOp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

declare_node! {
    #[tree]
    pub struct ExprUnary (op: UnaryOp) {
        inner: Expr = 0,
    }
}
into_expr!(ExprUnary);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// `-`
    Minus,

    /// `!`
    Not,
}

impl private::Sealed for UnaryOp {}
impl Inline24 for UnaryOp {
    fn into_u24(self) -> u32 {
        self as u32
    }

    fn from_u24(v: u32) -> Self {
        assert!(v <= UnaryOp::Not as u32);
        // SAFETY: we checked that `v` is a valid `UnaryOp`
        unsafe { transmute(v as u8) }
    }
}

impl std::fmt::Debug for DebugInline24<UnaryOp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

declare_node! {
    #[dyn_tree]
    pub struct ExprCall {
        callee: Expr = 0,

        #[tail]
        args: [Expr],
    }
}
into_expr!(ExprCall);

declare_node! {
    #[leaf]
    pub struct ExprInt (value: IntId);
}
into_expr!(ExprInt);

declare_node! {
    #[leaf]
    pub struct ExprStr (value: StrId);
}
into_expr!(ExprStr);

declare_node! {
    #[leaf]
    pub struct ExprIdent (value: IdentId);
}
into_expr!(ExprIdent);

declare_node! {
    #[dyn_tree]
    pub struct ExprBlock {
        tail: Opt<Expr> = 0,

        #[tail]
        body: [Stmt],
    }
}
into_expr!(ExprBlock);

pub struct DebugNode<'a, T: Node>(&'a Ast, T);

pub struct DebugInline24<T: Inline24>(T);
pub struct DebugInline56<'a, T: Inline56>(&'a Ast, T);

pub struct DebugNodeList<'a, T: Node>(&'a Ast, &'a [T]);

impl<'a, T: Node> std::fmt::Debug for DebugNodeList<'a, T>
where
    DebugNode<'a, T>: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        for entry in self.1 {
            list.entry(&DebugNode(self.0, *entry));
        }
        list.finish()
    }
}

impl std::fmt::Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Program")
            .field("body", &DebugNodeList(self, self.root.body(self)))
            .field("tail", &DebugNode(self, self.root.tail(self)))
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn packed_round_trip() {
        let u56 = u64::MAX & U56_MASK;
        eprintln!("{u56:064b}");
        assert_eq!(Leaf::new(0, u56).value(), u56);

        let u24 = u32::MAX & U24_MASK as u32;
        eprintln!("{u24:032b}");
        assert_eq!(Tree::new(0, 1, u24).value(), u24);
        assert_eq!(Tree::new(0, 1, u24).value(), u24);
    }
}
