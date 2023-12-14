# Frostbite grammar

## Expr
An expression may be composed by one of the following elements: 
- Ints (regex: `[0-9]+`)
- Floats (regex: `(-?[0-9]*)?\.[0-9]+`)
- Idents (regex: `[a-zA-Z][a-zA-Z0-9]+`)
- Strings (regex: `".*"`)
- Assigns ()