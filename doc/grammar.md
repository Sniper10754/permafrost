# Frostbite grammar

## Expr
An expression may be composed by one of the following elements: 
- Ints (regex: `[0-9]+`)
- Floats (regex: `(-?[0-9]*)?\.[0-9]+`)
- Idents (regex: `[a-zA-Z][a-zA-Z0-9]+`)
- Strings

`"<.*>"`

- Assigns

`a = 5`

```js
print_hi = 
    function() = print("Hello world")
```

`<ident> = <expr>`

- Functions 

`function name(x, y) = x + y`