pub(crate) trait Expr {}

pub(crate) type BoxedExpr = Box<dyn Expr>;

pub(crate) struct ConditionalExpr {
    pub condition: Box<dyn Expr>,
    pub when_true: Box<dyn Expr>,
    pub when_false: Box<dyn Expr>,
}

impl Expr for ConditionalExpr {
}

pub(crate) enum BinaryOperator {
    LessThan,
    LessEqual,
    GreaterEqual,
    GreaterThan,
    Equal,
    Add,
    Sub,
    Multiply,
    Divide,
    Remainder,
}

pub(crate) struct BinaryExpr {
    pub left: Box<dyn Expr>,
    pub operator: BinaryOperator,
    pub right: Box<dyn Expr>,
}

impl Expr for BinaryExpr {
}

pub(crate) struct CallExpr {
    pub line: usize,
    pub column: usize,
    pub function: String,
    pub arguments: Vec<Box<dyn Expr>>,
}

impl Expr for CallExpr {

}

pub(crate) struct IdentifierExpr {
    pub line: usize,
    pub column: usize,
    pub identifier: String,
}

impl Expr for IdentifierExpr {

}

impl Expr for f64 {

}
