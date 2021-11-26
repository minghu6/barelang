use std::path::{Path, PathBuf};

use super::CompileContext;



#[inline]
pub(crate) fn name_to_filename(name: &str) -> String {
    name.to_string() + ".bacore.lisp"
}


#[inline]
pub(crate) fn name_to_path(name: &str) -> PathBuf {
    PathBuf::from(name_to_filename(name))
}


#[inline]
pub(crate) fn extract_index_path_from_dir(dir: &Path) -> PathBuf {
    let index_filename = name_to_path("mod");
    dir.join(index_filename)
}

