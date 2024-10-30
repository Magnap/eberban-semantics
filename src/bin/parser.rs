type Res<T> = Result<T, Box<dyn std::error::Error>>;

use std::time::Instant;

use chumsky::{error::Simple, Parser};
use eberban::{expr::to_expr, lexer::lexer, parser::parser};

fn main() -> Res<()> {
    let example_sentences = [
        "zi mio tiho a ol ahu nu",
        "zimiotiho'a'ol'ahu'nu",
        "kavda",
        "kavdanakla",
        "kavda'nakla",
        "mi dona pe pe mian pei pei",
        "pe dona pei pe pe mian pei pei mavda",
        "pe dona pei pe pe mian pei mavda",
        "mi duna vo mo vei meon",
        "pe mi duna vo mo vei meon",
        "pe mi duna pei vo mo vei meon",
        "mian",
        "tce mian",
        "dona tce mian",
        "mi dona tce mian",
        "mi katmi va sae tuli mo",
        "mi katmi via sae tuli mo",
        "mi katmi va sae tuli mo dona mian",
        "mi katmi via sae tuli mo dona mian",
        "mo via mian fia meon",
        "mian se bure blan",
        "meon sae bure mian",
        "tce mian",
        "tce sia mian",
        "mi dona jnu jvao mian",
        "mi bure sre miun",
        "mi bure sia sre miun",
        "sre bure",
        "sia sre bure",
        "mi dona sae dona mo",
        "zaor cma kan gule ceru",
        "blarin seo kce soai kolon cila",
        "mi dona va ke be mian bure ke",
    ];

    let lexer = lexer::<Simple<_>>();
    let parser = parser::<Simple<_>>();
    for s in example_sentences {
        println!("{s}");
        let start = Instant::now();
        let s = lexer.parse(s).unwrap();
        let lexing = start.elapsed();
        if let Ok(tree) = parser.parse(s) {
            let expr = to_expr(tree);
            let parsing = start.elapsed();
            dbg!(expr);
            println!(
                "lexed in {} µs, parsed in {} µs",
                lexing.as_micros(),
                (parsing - lexing).as_micros()
            );
        } else {
            println!("lexed in {} µs", lexing.as_micros());
        }
    }
    Ok(())
}
