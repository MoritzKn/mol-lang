# The Mol Programming Language

An unfinished, experimental programming language

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

```mol
let list = [1, 2, 3, 4];

let sumOfSquares = list
    :map((n) => n * n)
    :reduce((acc, curr) => acc + curr, 0);

console.log(sumOfSquares);
```

## Getting Started

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
