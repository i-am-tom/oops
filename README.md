# Oops 🍌

When we're writing functional code involving errors, we often find ourselves
reaching for a type like `Either` (usually `ExceptT`): we put our "success
type" on the `Right`, and our "error type" on the `Left`. When our code gets
more complicated, however, we're going to find ourselves introducing *multiple*
error types (see [Matt Parsons'
blog](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html)
for a nice introduction to this practice). This is great, but the solution is
also a new problem: our error types are not all the same! In order to use the
monad instance, we need all our results to have the same `Left` type. How do we
have both?

One solution is the nested `Either` type. As our error catalogue grows, so does
our type signature:

| Possible errors | Type                                               |
| --------------- | -------------------------------------------------- |
| 1               | `ExceptT a IO ()`                                  |
| 2               | `ExceptT (Either a b) IO ()`                       |
| 3               | `ExceptT (Either a (Either b c)) IO ()`            |
| 4               | `ExceptT (Either a (Either b (Either c d))) IO ()` |
| ...             | ...                                                |

This is _fine_: we can use some type synonyms to hide all this noise (`type
Errors = Either ...`), or maybe even alias `Either` (`type (+) = Either`) to
something smaller. Both are acceptable, but it comes with a big maintenance
burden. The structure of the `Either` type is quite fragile, and adding more
errors to the catalogue will invariably break other code (what was once added
with `Right . Right . Right` is now `Right . Right . Left`). Add to that the
fact that it's just _noisy_. What if we had...

| Either                             | Variant                 |
| ---------------------------------- | ----------------------- |
| `a`                                | `Variant '[a]`          |
| `Either a b`                       | `Variant '[a, b]`       |
| `Either a (Either b c)`            | `Variant '[a, b, c]`    |
| `Either a (Either b (Either c d))` | `Variant '[a, b, c, d]` |

With the `Variant` type, we declare (in the type) the list of possible values,
just as we do with `Either`. The only real difference at this point is that the
syntax is nicer! Still, there must be more to it; what can we do with a
`Variant`?

> _The library also defines `VariantF`, which works in the same way, but the
> type also mentions a type constructor, and the list of types are applied to
> it. For example, `VariantF IO '[Int, String]` is actually either `IO Int` or
> `IO String`. We can think of `Variant` as the special case of `VariantF
> Identity`._

Typically, a module involving a `Variant` may need some of the following
extensions, depending on what you're doing with it:

```haskell
{-# LANGUAGE
      DataKinds
    , FlexibleContexts
    , MonoLocalBinds
    , RankNTypes
    , ScopedTypeVariables
    , TypeApplications
    , TypeOperators #-}
```

## "Throwing"

```haskell
throw :: xs `CouldBe` x => x -> Variant xs
```

Given some variant of types `xs` (e.g. `'[Int, String, Bool]`), if we have some
type `x` in that variant, we say that the variant _could be_ `x`. `throw` lets
us lift any type into a variant that _could be_ that type! In other words:

```haskell
eg0 :: Int -> Variant '[Int]
eg0 = throw

eg1 :: Bool -> Variant '[Bool, String]
eg1 = throw

eg2 :: IO () -> Variant '[Int, IO (), Bool]
eg2 = throw
```

Now, _why do we call it throw_?

## "Catching"

```haskell
catch :: Catch x xs ys => Variant xs -> Either (Variant ys) x
```

The `catch` function effectively "plucks" a type _out_ of the constraint. In
other words, if I `catch @String` on a `Variant '[Int, String, Bool]`, the
result is `Either (Variant '[Int, Bool]) String`. This allows us to remove
errors from the catalogue as we go up up the call stack.

The name is a reference to the `throw`/`catch` exception systems in other
languages. In Java, I may see a definition like this:

```java
public static void whatever() throws ExceptionA, ExceptionB
```

The equivalent in Haskell using _this_ library would be:

```haskell
main
  :: ( e `CouldBe` ExceptionA
     , e `CouldBe` ExceptionB
     )
  => String -> Either e ()
```

## "Throwing" _and_ "Catching"

The interesting thing about the above two functions is that you should almost
_never_ see the `Catch` constraint in one of your signatures. Let's see an
example:

```haskell
data NetworkError      = NetworkError
data UserNotFoundError = UserNotFoundError

getUser
  :: ( e `CouldBe` NetworkError
     , e `CouldBe` UserNotFoundError
     )
  => String
  -> ExceptT (Variant e) IO String

getUser = \case
  "Alice" -> throwM NetworkError
  "Tom"   -> pure "Hi, Tom!"
  _       -> throwM UserNotFoundError
```

We've got ourselves a fresh (and extremely contrived) bit of business logic!
Notice that, according to the constraints, a couple things could go wrong: we
could have a network error, or fail to find the user!

Now, let's say we're calling this from another function that does some more
contrived business logic:

```haskell
renderProfile :: e `CouldBe` NetworkError => ExceptT (Variant e) IO ()
renderProfile = do
  name <- catchM @UserNotFoundError getUser \_ -> do
    liftIO (putStrLn "ERROR! USER NOT FOUND. Defaulting to 'Alice'.")

    pure "Alice"

  liftIO (putStrLn name)
```

Here, we've tried to call `getUser`, and handled the `UserNotFoundError`
explicitly. You'll notice that, as a result, _this_ signature doesn't mention
it! Thanks to some (_very careful_) use of `INCOHERENT`, a `CouldBe` and a
`Catch` constraint will actually cancel each other out!

This library gives us all the benefits of Haskell's type system, forcing us to
be explicit about all the possible errors we encounter, but doesn't force us to
stick to a concrete error stack throughout. Our code is less fragile, our
functions are decoupled, and error-handling is actually bearable!
