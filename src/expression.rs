use crate::value::Value;

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
    pub operator: UnaryOperator,
    pub expr: Box<dyn Expr>,
}

impl Expr for UnaryExpr {

}

pub(crate) struct AccessorExpr {
    pub expr: Box<dyn Expr>,
    pub accessor: Box<dyn Expr>,
}

impl Expr for AccessorExpr {

}

pub(crate) struct CallExpr {
    pub expr: Box<dyn Expr>,
    pub arguments: Vec<Box<dyn Expr>>,
}

impl Expr for CallExpr {

}

pub(crate) struct GroupingExpr {
    pub expr: Box<dyn Expr>,
}

impl Expr for GroupingExpr {

}

pub(crate) struct ListExpr {
    pub expressions: Vec<Box<dyn Expr>>,
}

impl Expr for ListExpr {

}

pub(crate) struct MapExpr {
    pub pairs: Vec<(Box<dyn Expr>, Box<dyn Expr>)>,
}

impl Expr for MapExpr {

}

pub(crate) struct GlobalExpr {
    pub identifier: String,
}

impl Expr for GlobalExpr {

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
