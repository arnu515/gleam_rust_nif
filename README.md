# A test with NIF in Gleam

Run rust code in gleam with [rustler](https://lib.rs/crates/rustler) and [Erlang Native Implemented Functions (NIFs)](https://www.erlang.org/doc/system/nif.html).

## Example

### Create a Rust dylib

Create a rust project with `cargo new <name> --lib`.

Add some Rust code and annotate it with the `rustler::nif` procmacro, and then call the `rustler::init!` macro in `lib.rs`. For example:

```rs
#[rustler::nif]
pub fn add(left: i64, right: i64) -> i64 {
    println!("Hello from Rust!");
    left + right
}

rustler::init!("rustnif");
```

Next, enable the correct version of nif as a [feature](https://lib.rs/crates/rustler/features) of rustler in `Cargo.toml`. Also make the library a dylib.

```toml
[lib]
crate-type = ["dylib"]

[dependencies]
rustler = { version = "0.35.0", features = ["nif_version_2_17"] }
```

Finally run `cargo build --release` and move the compiled `.so` file to `priv/`.

### Use it in Gleam

Create an Erlang module with the same name as the rust library, in this example, `src/<name>.erl`:

```erlang
-module(rustnif).
-export([add/2]).
-nifs([add/2]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("priv/librustnif", 0).

add(_X, _Y) ->
    erlang:nif_error(nif_library_not_loaded).
```

Then call it in gleam
```gleam
import gleam/io

@external(erlang, "rustnif", "add")
fn add(x: Int, y: Int) -> Int

pub fn main() {
  io.debug(add(34, 35))
  Nil
}
```

```command
$ gleam run
...
Hello from Rust!
69
```

