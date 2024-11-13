import gleam/io

@external(erlang, "rustnif", "add")
fn add(x: Int, y: Int) -> Int

pub fn main() {
  io.debug(add(34, 35))
  Nil
}
