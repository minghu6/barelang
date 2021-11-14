use std::{env, fs, path::{Path, PathBuf}};



/// BareLang Installed Home
#[inline]
pub fn bare_home() -> PathBuf {
    let bare_home_str
    = env::var("BARE_HOME").unwrap_or(".".to_string());

    fs::canonicalize(Path::new(
        &bare_home_str
    )).unwrap()
}


/// Full path of librsc.so
#[inline]
pub fn librsc_path() -> PathBuf {
    runtime_dir().join("librsc.so")
}


#[inline]
pub fn libcore_path() -> PathBuf {
    runtime_dir().join("libbacore.so")
}


#[inline]
pub fn runtime_dir() -> PathBuf {
    bare_home().join("runtime")
}


#[inline]
pub fn core_src_dir() -> PathBuf {
    runtime_dir().join("core")
}

