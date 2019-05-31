extern crate cc;

fn main() {
    cc::Build::new()
        .file("src/c/varargs.c")
        .compile("cbindings");
}
