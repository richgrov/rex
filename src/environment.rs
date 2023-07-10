pub type Function = fn(&[f64]) -> f64;

pub struct Environment {
    pub locals: Vec<String>,
    pub functions: Vec<(String, Function)>,
}

impl Environment {
    pub(crate) fn index_of_item(&self, name: &str) -> Option<usize> {
        self.locals.iter().position(|s| s == name)
    }

    pub(crate) fn function_info(&self, name: &str) -> Option<(usize, Function)> {
        self.functions.iter()
            .enumerate()
            .find(|(_index, (s, _func))| s == name)
            .map(|(index, (_, func))| (index, func.clone()))
    }

    pub(crate) fn get_function(&self, func_index: usize) -> Option<Function> {
        self.functions.get(func_index).map(|(_name, func)| func).copied()
    }
}
