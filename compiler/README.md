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
