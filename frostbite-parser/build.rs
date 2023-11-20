fn main() {
    println!("cargo:rerun-if-changed=src/*.lalrpop");

    lalrpop::process_root().unwrap()
}
