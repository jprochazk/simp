fn main() {
    println!("cargo:rerun-if-changed=src/parser/inputs/valid");
    println!("cargo:rerun-if-changed=src/parser/inputs/invalid");
}
