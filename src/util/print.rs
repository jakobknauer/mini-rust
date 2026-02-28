mod hlr;
mod mlr;

pub use hlr::print_hlr;
pub use mlr::print_mlr;

fn reinsert_escape_sequences(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\'', "\\'")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}
