---
title: "Haskell type classes: incidental non-examples"
---

This post is a tour of some Haskell type classes, and why some types don't fit
them, but in a non-fundamental way. That means that one could change the
encoding of these type classes in order to make these types fit into them,
without changing their mathematical meaning.

This list will evolve in time.

---

# Functor

The most common type class on Haskell, inspired by the category theory
[concept](https://en.wikipedia.org/wiki/Functor) with same name. In Haskell all
functors are endofunctors, that is, they have the same domain and target
categories.

```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

## Non-examples

The most common non-example is `Set`. A reason for this is that `Set` contains
only unique elements, following their `Eq` instance, but Haskell functions are
not required to follow this instance. That is, `x == y` does not imply `f x ==
f y`, which is the property known as [Leibniz
equality](https://en.wikipedia.org/wiki/Equality_(mathematics)#Equality_in_mathematical_logic).
If we define for any type a newtype:

```
newtype Same a = Same { getSame :: a }

instance Eq (Same a) where
  _ == _ = True

instance Ord (Same a) where
  _ <= _ = True

```

then it easy to see that `Set.map getSame . Set.map Same` is not the same as
`Set.map (getSame . Same) = Set.map id = id`.

Another reason for `Set` not to fit the class is its `Ord` restriction on
elements. To see why this is a problem, we can write the full type signature
for `fmap`, with explicit quantification:

```
forall a b . Functor f => (a -> b) -> f a -> f b
```

which shows that there is no place for restrictions in this signature.

Other examples of this kind are the `Storable` and `Unbox` variants of `Vector`
from the [vector](https://hackage.haskell.org/package/vector) package. These
limitations make numerical applications in Haskell unpleasant, since they often
depend on constraints. The most well known library that tries to solve this
problem is [SubHask](https://github.com/mikeizbicki/subhask), and I have my own
personal take on the subject
[here](https://github.com/guaraqe/constraint-classes).

---

# Applicative

The class is defined as follows:

```
class Functor f => Applicative f where
  pure :: a -> f a
  <*> :: f (a -> b) -> f a -> f b
```

Both functions are related to the concept of [monoidal
categories](https://en.wikipedia.org/wiki/Monoidal_category) in different ways.
The function `pure` is related to the concept of [tensorial
strength](https://ncatlab.org/nlab/show/tensorial+strength), while `<*>` is
related to [lax monoidal
functors](https://ncatlab.org/nlab/show/monoidal+functor).

It is worth noting that the property of having `<*>` is equivalent to have a
function `zip :: (f a, f b) -> f (a,b)`, that is, pairs of functor images are mapped
to functor images of pairs.

## Non-examples

A common type that is `Functor` but not `Applicative` is `Map k`. `Map k a` is
semantically a function `k -> Maybe a`, the composition of `(k ->)` and
`Maybe`, both `Applicatives`. Since for `(k ->)` we have `pure = const` and for
`Maybe` we have `pure = Just`, we would need to have values for all the keys,
which may be infinite in number.

However, `<*>` is perfectly implementable, and could be defined as
`instersectWith ($)`, which is equivalent to the composition of `<*>` for `(k
->)` and `Maybe`.

Another similar example are `ZipVector`s, which are vectors where `(<*>) =
zipWith ($)`. For this instance to be lawful, one would need `pure` to produce
an infinite vector, which is not possible. This is however possible for lists
and is implemented in
[Control.Applicative](http://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Applicative.html#t:ZipList).

A solution to this problem is to split the `Applicative` class in two parts.
You can find these parts as `Pointed` in
[pointed](https://hackage.haskell.org/package/pointed) and `Apply` in
[semigroupoids](https://hackage.haskell.org/package/semigroupoids).  This
separation has been also implemented in
[purescript](https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Control.Apply).
