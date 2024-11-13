#[rustler::nif]
pub fn add(left: i64, right: i64) -> i64 {
    println!("Hello from Rust!");
    left + right
}

rustler::init!("rustnif");
