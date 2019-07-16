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
	Identifier "=" Expression ";"
````

```
Expression:
	Call
	MemberAccess
	Identifier
	NumberLiteral
	StringLiteral
```

```
Call:
	Expression "(" {, Expression} ")"
```

```
MemberAccess:
	Expression "." Identifier
```

```
Identifier
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
