use std::fs::{File};
use std::io::{BufRead, BufReader, Write};
use walkdir::WalkDir;
use regex::Regex;
use clap::{Arg, Command};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    // Define and parse command-line arguments
    let matches = Command::new("File Concatenator")
        .version("1.0")
        .author("Jfast")
        .about("Concatenates files with a specified extension from a directory")
        .arg(
            Arg::new("directory")
                .short('d')
                .long("directory")
                .value_name("DIRECTORY")
                .help("The directory to scan")
                .required(true)
        )
        .arg(
            Arg::new("extension")
                .short('e')
                .long("extension")
                .value_name("EXTENSION")
                .help("The file extension to look for")
                .required(true)
        )
        .get_matches();

    let directory = match matches.try_get_one::<String>("directory") {
        Ok(x) => match x {
            Some(x) => x,
            None => panic!("No value for directory")
        },
        Err(e) => panic!("{}", e),
    };
    let extension = match matches.try_get_one::<String>("extension") {
        Ok(x) => match x {
            Some(x) => x,
            None => panic!("No value for extension")
        },
        Err(e) => panic!("{}", e)
    };

    concatenate_files_recursively(directory, extension)?;

    Ok(())
}

fn concatenate_files_recursively(directory: &str, extension: &str) -> Result<()> {
    let output_file = format!("{}/concatenated.{}", directory, extension);
    let mut output = File::create(output_file)?;

    let re = Regex::new(&format!(r"\.{}$", regex::escape(extension)))?;

    for entry in WalkDir::new(directory).into_iter().filter_map(std::result::Result::ok) {
        let path = entry.path();
        if path.is_file() && re.is_match(path.to_str().unwrap_or("")) {
            let file = File::open(path)?;
            let reader = BufReader::new(file);

            for line in reader.lines() {
                writeln!(output, "{}", line?)?;
            }
        }
    }

    Ok(())
}
