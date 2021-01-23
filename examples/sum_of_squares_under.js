function seq(min, max) {
  let arr = []
  for (let i = min; i <= max; i++) {
    arr.push(i)
  }
  return arr
}

function sumOfSquaresUnder(n) {
  return seq(0, n).reduce((acc, curr) => acc + curr * curr, 0)
}

console.log("Sum of squares of numbers under 100:        ", sumOfSquaresUnder(100));
console.log("Sum of squares of numbers under 1000:       ", sumOfSquaresUnder(1000));
console.log("Sum of squares of numbers under 10000:      ", sumOfSquaresUnder(10000));
console.log("Sum of squares of numbers under 100000:     ", sumOfSquaresUnder(100000));

// === hard ===

// console.log("Sum of squares of numbers under 1000000:    ", sumOfSquaresUnder(1000000));
// console.log("Sum of squares of numbers under 10000000:   ", sumOfSquaresUnder(10000000));
// console.log("Sum of squares of numbers under 100000000:  ", sumOfSquaresUnder(100000000));

// === really hard ===

// console.log("Sum of squares of numbers under 1000000000: ", sumOfSquaresUnder(1000000000));
// console.log("Sum of squares of numbers under 10000000000:", sumOfSquaresUnder(10000000000));
