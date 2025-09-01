---
title: "Haskell type classes: a compilation of laws"
summary: Reference compilation of mathematical laws for Haskell's core type classes
---

This post is a reference for type classes' laws. It will be expanded with time.

---

# Functor

```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

## Laws

- `fmap id = id`
- `fmap f . fmap g = fmap (f . g)`

---

# Applicative

```
class Functor f => Applicative f where
  pure :: a -> f a
  <*> :: f (a -> b) -> f a -> f b
```

An alternative definition could be:

```
class Functor f => Applicative f where
  pure :: a -> f a
  zip :: (f a, f b) -> f (a, b)
```

## Laws

- `pure id <*> v = v`
- `pure f <*> pure x = pure (f x)`
- `f <*> pure x = pure ($ x) <*> f`
- `pure (.) <*> x <*> y <*> z = x <*> (y <*> z)`

or:

- `zip (pure x) (pure y) = pure (x,y)`
- `zip (pure x) y = fmap (x,) y`
- `zip x (pure y) = fmap (,y) x`
- `(\(a,(b,c)) -> ((a,b),c)) $ zip x (zip y z) = zip (zip x y) z`

---

# Monad

```
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
```

An alternative definition could be:
```
class Applicative m => Monad m where
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
```

## Laws

- `pure a >>= f = f a`
- `x >>= pure = x`
- `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`

or:

- `pure >=> f = f`
- `f >=> pure = f`
- `f >=> (g >=> h) = (f >=> g) >=> h`

---

# Alternative

```
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

## Laws

- `empty <|> x = x`
- `x <|> empty = x`
- `x <|> (y <|> z) = (x <|> y) <|> z`

and:

- `(f <|> g) <*> a = (f <*> a) <|> (g <|> a)`
- `empty <*> a = empty`
- `f <$> (a <|> b) = (f <$> a) <|> (f <$> b)`
- `f <$> empty = empty`

---

# Foldable

```
class Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m
```

## Laws

There are no laws. However, when it is also a functor, it is expected that:

- `foldMap f . fmap g = foldMap (f . g)`

also, if `t` is *monoid morphism*, that is:

```
t mempty = mempty
t (a <> b) = t a <> t b
```

it is expected that:

- `t . foldMap f = foldMap (t . f)`

---

# Traversable

```
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
```

## Laws

Let `t` be an *applicative transformation*, that is:

```
t (pure a) = pure a
t (a <*> b) = t a <*> t b
```

then:

- `t . traverse f = traverse (t . f)`
- `traverse Identity = Identity`
- `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`

and:

- `traverse (Identity . f) = Identity . fmap f`
- `traverse (Const . f) = Const . foldMap f`
