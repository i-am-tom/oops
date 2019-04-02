# Oops 🍌

## Introduction

When we're writing functional code involving errors, we often find ourselves
reaching for a type like `Either`. When our code gets more complicated,
however, we're going to find ourselves introducing multiple error types (see
[Matt Parsons' blog](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html)
for a nice introduction to this practice).

The problem with multiple error types is that they become difficult to juggle.
If we go for a nested `Either` type, we end up with one big problem:
*Maintenance*. Not only does it look _horrible_, but it looks worse when we
need to add or remove types. Add that to the fact that any structural change to
that nesting will break a _lot_ of code, and it's just not ideal.

### "`x` is _one of_ `xs`"

The `Variant` type in this library captures the same information, but in a less
fragile way. A `Variant '[Int, String, Bool]` is _either_ an `Int`, a `String`,
or a `Bool`. Rather than worrying about the internal structure, let's just
focus on the API!

### Throwing

```haskell
throw :: xs `CouldBe` x => x -> Variant xs
```

If we have a value of a type that is in our `Variant`, the `throw` function
will "lift" that type in. In other words, if I want `Int -> Variant '[Int,
String, Bool]`, or `String -> Variant '[Int, String, Bool]` or _even_ `Bool ->
Variant '[Int, String, Bool]`, then `throw` is the function for me!

As long as we can prove that our type exists inside our `Variant` (or, in other
words, our type _could be_ the one inside it), then we can lift the value! Now,
why do we call it throw?

### Catching

```haskell
catch :: Catch x xs ys => Variant xs -> Either (Variant ys) x
```

The `catch` function effectively "plucks" a type _out_ of the constraint. In
other words, if I `catch @String` on a `Variant '[Int, String, Bool]` the
result is `Either (Variant '[Int, Bool]) String`. The name is a reference to
the `throw`/`catch` exception systems in other languages: in Java, I may see a
definition like this:

```java
public static void whatever() throws ExceptionA, ExceptionB
```

The equivalent using this library would be:

```haskell
main
  :: ( e `CouldBe` ExceptionA
     , e `CouldBe` ExceptionB
     )
  => String -> Either e ()
```

## Throwing _and_ Catching

The interesting thing about the above two functions is that you should almost
_never_ see the `Catch` constraint in one of your signatures. Let's see an
example:

```haskell
fetchUser
  :: forall e m
   . ( e `CouldBe` DeletedUserError
     , e `CouldBe` NetworkError
     , MonadError e m
     , MonadAccounts m
     )
  => UserId
  -> m User

fetchUser userId = do
  let throw' :: forall oops a. e `CouldBe` oops => oops -> m a
      throw' = throwError . throw

  ...

  queryResult <- runQuery >>= \case
    Just result -> parse result
    Nothing     -> throw' NetworkConnectionDropped

  ...

  user <- case queryResult of
    Just user -> pure user
    Nothing   -> throw' (DeletedUserError userId)

  ...
```

Our app might have some business logic that looks similar to this, and we
declare in the type signature that it _might_ throw one of a couple possible
errors. We can then "throw" something into a variant using `throw`, followed by
`throwError` to lift the variant into `MonadError`. Let's say we call this from
a function that renders a user profile:

```haskell
profile
  :: forall e m
   . ( e `CouldBe` NetworkError
     , MonadAccounts m
     , MonadError e m
     )
  => UserId
  -> m Aeson.Value

profile userId = do
  ...

  user <- runExceptT (fetchUserId userId) >>= \case
    Right user -> render user

    Left errors ->
      case catch @DeletedUserError errors of
        Right error -> render404
        Left errors -> throwError errors

  ...
```

Here, we've called the function, then specialised it to an `ExceptT` in order
to get hold of the error variant. At that point, we `catch` a
`DeletedUserError`, and throw anything else.

What's interesting here is that the `catch` call doesn't _add_ a constraint to
the `profile` function - it removes one! 

TODO
