# The Mol Programming Language

An experimental programming language

How Mol looks so far:

```mol
let list = [1, 2, 3, 4];

let sumOfSquares = list
    :map((n) => n * n)
    :reduce((acc, curr) => acc + curr, 0);

console.log(sumOfSquares);
```

## Features

- Expression based grammar, blocks yield the value of the last statement
- Four basic data types: `Void`, `Number`, `String`, and `Boolean`
- And three complex data types: `List`, `Maps`, and `Function`
- Higher-order functions, Closures, and Lambda expressions
- Full-fledged REPL with autocomplete and everything
- A "bind" operator (`value:method()`)

## Goals

The goal of Mol is to demonstrate an alternative approach to "build stacks" in web development. Instead of having a build stack with a _bunch_ of compilers controlled by either an opinionated or complex "bundler", Mol lets you do it all in the source language.

Instead of having a bundler look for file references, compress files, run preprocessing, and put them in the bundle, you would just call a function. Instead of stitching complicated configurations together to archive your goal, you can just define step by step what happens. The same approach should also work for templates, optimizations, server-side rendering, and everything else that we expect in a modern web build stack.

To allow you to call functions at compile-time, Mol is intended to have a very elaborate macro system. But it's not what you might expect:
1. "Macros" are regular code, no special syntax
2. Syntax is enforced, macros operate on the AST and can not generate broken syntax
3. Macros can include macros

To function as a web language interoperability with JavaScript is also a priority.

In the end it should work something like this:

```
function getContent() {
    "It is: " ++ Date()
};

function updateDoc() {
    document.body.innerText = getContent()
};

function getConfig(name) {
    env(name)
};

function app() {
    // Call getConfig at compile-time, then inline the result
    let interval = <> getConfig("interval");

    setInterval(() => {
        updateDoc()
    }, interval)
};

function generateHtml(includes) {
    // NOTE: getContent is called in the build and the client!
    `
    <!DOCTYPE html>
    <html>
        <head>
            <title>It is</title>
            ${includes}
        </head>
        <body>
            ${getContent()}
        </body>
    </html>
    `
};

function build() {
    // The `>` operator here is called a "lift up". It generates the code
    // to call a function including all its dependencies.
    // It's actually just one half of the macro you saw call earlier:
    // `<> foo()` is the same as `< ( > foo())`
    // A macro call is a "lift up" and then a "place down". The "place down" inlines the AST.
    let clientCode = > app();

    let js = toJavaScript(clientCode);
    let jsOutput = minifyJavaScript(js);
    let jsFile = 'main.' ++ hashString(jsOutput) ++ '.js';

    writeFile('./dist/' ++ jsFile, jsOutput);

    let favIcon = loadFile('./favicon.ico');
    writeFile('./dist/favicon.ico', favIcon);

    let html = generateHtml(`
        <script src="${jsFile}"></script>
        <link rel="icon" href="favicon.ico" />
    `);
    let htmlOutput = minifyHtml(html);
    writeFile('./dist/index.html', htmlOutput);
};

build();
```

## Getting Started

Make sure you have [Rust installed](https://www.rust-lang.org/tools/install), then run:

```sh
# Start the REPL
cargo run

# Execute a file
cargo run -- examples/example.mol
```

## Development

```sh
# Run unit test suite
cargo test
```
