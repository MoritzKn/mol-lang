console.log("hello!");

let list = [1, 9, 4, 2];
list.filter((n) => n > 2);

function seq(min, max) {
  let arr = []
  for (let i = min; i < max; i++) {
    arr.push(i)
  }
  return arr
}

function randomValues(n) {
	return seq(0, n).map(() => Math.random() * 100);
};

console.log(randomValues(10).map(Math.round));

function randomGrid(size) {
	return seq(0, size).map(() => randomValues(size))
};

let grid = randomGrid(10);

function valueToChar(value) {
	if (value > 70) {
		return "●"
	} else if (value > 60) {
		return "◉"
	} else if (value > 40) {
		return " "
	} else if (value > 30) {
		return "·"
	} else {
		return "◇"
	}
};

valueToChar(grid[0][0]);

function plot(grid) {
	return grid.map((line) => line
			.map(valueToChar)
			.reduce((a, b) => a + " " + b, ""))
		.forEach(c => console.log(c))
};

plot(grid);

plot(randomGrid(15));

let sum = (a, b) => a + b;
let concat = (a, b) => a.concat(b);
let avg = (list) => list.reduce(sum, 0) / list.length;

function smoothen(grid, factor) {
	return seq(0, grid.length).map((y) => {
		let line = grid[y];

		return seq(0, line.length).map((x) => {
			let point = grid[y][x];

			let area = seq(y - 1, y + 2)
				.map((yy) => {
					return seq(x - 1, x + 2)
						.map((xx) => (grid[yy] || {})[xx])
				})
				.reduce(concat, [])
				.filter((point) => point);

			 return avg(area) * factor + point * (1-factor);
		});
	});
};

grid = randomGrid(30);

console.log('Smoothen 0');
console.log();
plot(grid);
console.log();
console.log('Smoothen 0.5');
console.log();
plot(smoothen(grid, 0.5));
console.log();
console.log('Smoothen 1');
console.log();
plot(smoothen(grid, 1));
console.log();
console.log('Smoothen 0.3 then 0.3');
console.log();
plot(smoothen(smoothen(grid, 0.2), 0.3));
