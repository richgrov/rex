use rex::{Function, Environment};

fn all(a: f64, b: f64) -> f64 {
    if a == 1. && b == 1. {
        1.
    } else {
        0.
    }
}

fn main() {
    let mut env = rex::Environment::new(&["one", "two"]);
    env.add_function("and", Function::Double(all));

    for line in std::io::stdin().lines() {
        if let Err(e) = eval(&line.unwrap(), &env, &[1.0, 2.0]) {
            println!("error: {}", e,);
        }
    }
}

fn eval(mut src: &str, env: &Environment, input: &[f64]) -> Result<(), rex::Error> {
    let disassemble = if src.ends_with("#disassemble") {
        src = src.trim_end_matches("#disassemble");
        true
    } else {
        false
    };

    let expr = rex::compile(&src, &env).unwrap();
    if disassemble {
        println!("{}", expr.code_dump());
    }

    println!("= {}", expr.eval(input)?);
    Ok(())
}
