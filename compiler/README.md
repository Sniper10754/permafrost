# Naming system description

Scoping is very simple, side notes:

In an assign, the body cannot reference the identifier its being bond onto.

```js

// Invalid!
a = a

a = 1

// Valid (although noop)
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

# Namespace system sketches

- Every file is a Namespace
- Every Namespace has locals
- These locals have types

A Namespace is just a set of exported symbols from a file

in this codebase its represented grossly in this way
```rs
struct Namespace {
    exports: Vec<Export>
}

enum Export {
    Local,
    Namespace,
}
```
