use crate::value::Value;

pub(crate) trait Expr {}

pub(crate) type BoxedExpr = Box<dyn Expr>;

pub(crate) struct ConditionalExpr {
    pub line: usize,
    pub column: usize,
    pub condition: Box<dyn Expr>,
    pub when_true: Box<dyn Expr>,
    pub when_false: Box<dyn Expr>,
}

impl Expr for ConditionalExpr {
}

pub(crate) enum BinaryOperator {
    Or,
    And,
    LessThan,
    LessEqual,
    GreaterEqual,
    GreaterThan,
    Equal,
    NotEqual,
    In,
    Add,
    Sub,
    Multiply,
    Divide,
    Remainder,
}

pub(crate) struct BinaryExpr {
    pub line: usize,
    pub column: usize,
    pub left: Box<dyn Expr>,
    pub operator: BinaryOperator,
    pub right: Box<dyn Expr>,
}

impl Expr for BinaryExpr {
}

pub(crate) enum UnaryOperator {
    Not, Negate
}

pub(crate) struct UnaryExpr {
    pub line: usize,
    pub column: usize,
    pub operator: UnaryOperator,
    pub expr: Box<dyn Expr>,
}

impl Expr for UnaryExpr {

}

pub(crate) struct PropertyExpr {
    pub line: usize,
    pub column: usize,
    pub expr: Box<dyn Expr>,
    pub property: String,
}

impl Expr for PropertyExpr {

}

pub(crate) struct CallExpr {
    pub line: usize,
    pub column: usize,
    pub expr: Box<dyn Expr>,
    pub arguments: Vec<Box<dyn Expr>>,
}

impl Expr for CallExpr {

}

pub(crate) struct IdentifierExpr {
    pub identifier: String,
}

impl Expr for IdentifierExpr {

}

pub(crate) struct ValueExpr {
    pub value: Value,
}

impl Expr for ValueExpr {

}
