# Naming system description

Scoping is very simple, side notes:

In an assign, the body cannot reference the identifier its being bond onto.

```js

// Invalid!
a = a

a = 1

// Valid (altought noop)
a = a
```

# Type System description

> https://en.wikipedia.org/wiki/Subtyping

Every expression has a type

Unifying types is acceptable in these cases:

- They're the same
$$ A = B $$
$$ Int = Int $$
$$ Float = Float $$

- When `A` is a subtype of `B`

$$ A = Int $$
$$ B = Number $$
$$ B >: A $$

# Module system sketchs

- Every file is a module
- Every module has locals
- These locals have types
