```
Programm:
	StatementList
```

```
StatementList:
	Statement* Expression?
```

```
Statement:
	ExpressionStatement
	EmptyStatement
	Assigment
```

```
ExpressionStatement
	Expression ";"
```

````
EmptyStatement
	";"
```

```
Assigment
	Id "=" Expression ";"
````

```
Expression:
	Call
	MemberAccess
	Id
	NumberLiteral
	StringLiteral
```

```
Call:
	Expression "(" {, Expression} ")"
```

```
MemberAccess:
	Expression "." Id
```

```
Id
	Letter {LetterOrDigit}

LetterOrDigit
	Letter
	[0-9]

Letter
	[a-z]
	[A-Z]
```

NumberLiteral
StringLiteral
