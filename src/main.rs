use anyhow::{bail, Result};
use std::io::prelude::*;
use std::{fs::File, path::PathBuf};

fn main() -> Result<()> {
    let mut args = pico_args::Arguments::from_env();

    match args.subcommand()?.as_ref().map(|s| s.as_str()) {
        None => print_help(),
        Some("pack") => {
            let fast = args.contains("--fast");

            let infile = args.free_from_os_str::<PathBuf, bool>(|s| Ok(s.into()))?;
            let outfile = args.free_from_os_str::<PathBuf, bool>(|s| Ok(s.into()))?;

            let mut data = vec![];
            File::open(infile)?.read_to_end(&mut data)?;
            let packed_data = if fast {
                upkr::pack_fast(&data)
            } else {
                upkr::pack(&data)
            };
            File::create(outfile)?.write_all(&packed_data)?;
        }
        Some("unpack") => {
            let infile = args.free_from_os_str::<PathBuf, bool>(|s| Ok(s.into()))?;
            let outfile = args.free_from_os_str::<PathBuf, bool>(|s| Ok(s.into()))?;

            let mut data = vec![];
            File::open(infile)?.read_to_end(&mut data)?;
            let packed_data = upkr::unpack(&data);
            File::create(outfile)?.write_all(&packed_data)?;
        }
        Some(other) => {
            bail!("Unknown subcommand '{}'", other);
        }
    }

    Ok(())
}

fn print_help() {
    eprintln!("Usage:");
    eprintln!("  upkr pack <infile> <outfile>");
    eprintln!("  upkr unpack <infile> <outfile>");
    std::process::exit(1);
}
