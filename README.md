# The Mol Programming Panguage

Client, server & build chain all in one code base

How Mol looks so far:

```mol
function fibonacci(num) {
  if (num <= 1) {
      1
  } else {
      fibonacci(num - 1) + fibonacci(num - 2)
  }
};

console.log('fibonacci of 16:', fibonacci(16));
```

## Getting Stated

Make sure you have [Rust installed](https://www.rust-lang.org/tools/install), then run:

```sh
# Start the REPL
cargo run

# Execute a file
cargo run -- examples/fibonacci.mol
```

## Development

```sh
cargo test
```
