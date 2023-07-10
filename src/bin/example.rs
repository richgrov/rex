use rex::Environment;

fn all(args: &[f64]) -> f64 {
    for arg in args {
        if *arg == 0. {
            return 0.
        }
    }

    1.
}

fn main() {
    let env = rex::Environment {
        locals: vec!["one".to_owned(), "two".to_owned()],
        functions: vec![("all".to_owned(), all)],
        optimization_level: rex::OptimizationLevel::Basic,
    };

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
