use std::fs;
use std::error::Error;
use std::env;
use std::path::{
    Path
};
use std::process::{
    Command
};
use std::time::{
    SystemTime, UNIX_EPOCH
};

// use clap::{
//     App, Arg, SubCommand
// };
use clap::{
    clap_app
};

use bac::compiler::{
    compile
};


fn unique_suffix() -> String {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis()
        .to_string()
}

fn main() -> Result<(), Box<dyn Error>> {
    let matches = clap_app!(_bac =>
        (version: "0.1.0")
        (author: "minghu6")
        (about: "Bare Language Compiler")
        (@arg FILE: +required "input source file (exp *.ba)")
        (@arg lib: --lib "generate library code")
        (@arg output: -o --output +takes_value "object file output path")
    ).get_matches();

    let source_file_path = matches.value_of("FILE").unwrap();
    let codestr
    = fs::read_to_string(source_file_path)
        .expect(&format!("Unable to read file {}", source_file_path));

    if let Some(_islib) = matches.value_of("lib") {
        unimplemented!()
    }
    else {
        let mut tmp_out_fn = String::from("__barelang_output");
        tmp_out_fn.push_str(&unique_suffix());

        compile(&codestr, &tmp_out_fn)?;

        let bare_home_str = env::var("BARE_HOME").unwrap_or(".".to_string());
        let bare_home = Path::new(
            &bare_home_str
        );

        let lib_path = bare_home.join("libbare.so");

        let output = match matches.value_of("output") {
            Some(output) => output,
            None => "a.out"
        };

        //gcc output.o libbare.so -Xlinker -rpath ./ -o main
        let gcc_link_st = Command::new("gcc")
        .arg(tmp_out_fn.as_str())
        .arg(lib_path.to_str().unwrap())
        .arg("-Xlinker")
        .arg("-rpath")
        .arg(bare_home_str)
        .arg("-o")
        .arg(output)
        .status()?;

        assert!(gcc_link_st.success(), "gcc link {}", gcc_link_st);

        fs::remove_file(tmp_out_fn)?;
    }

    Ok(())
}
