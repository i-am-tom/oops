{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Data.Variant
Description : Generalised coproducts and methods for working with them.
Copyright   : (c) Tom Harding, 2019
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

Traditionally in Haskell, we use @Either a b@ to represent a choice of two
types. If we want to represent /three/ types, we use @Either a (Either b c)@,
and this nesting can continue as far as it needs to. However, this approach
comes with some difficulties: it's quite difficult to manipulate, and makes for
some rather unwieldy type signatures.

Thankfully, though, GHC provides us with GADTs, and they allow us to construct
a type that encompasses a coproduct of any number of arguments: the 'Variant'.
Just as @Left 3@ and @Right True@ are of type @Either Int Bool@, we can write
@Here 3@ and @There (Here True)@ to do the same thing (ignoring 'Identity'
wrappers). We can think of the 'Here' and 'There' constructors as an "index":
the index of the type we're storing is the number of occurrences of 'There'.

>>> :t [ Here (Identity 3), There (There (Here (Identity True))) ]
[ Here (Identity 3), There (There (Here (Identity True))) ]
  :: Num x1 => [VariantF Identity (x1 : x2 : Bool : xs)]
-}
module Data.Variant
  ( -- * Generalised coproducts
    VariantF (..)
  , Variant

    -- * Scott encodings
  , variantF
  , variant

    -- * Church encodings
  , case_
  , caseF

    -- * Injections
  , CouldBeF (..)
  , CouldBe  (..)

    -- * Projections
  , CatchF (..)
  , Catch  (..)

    -- * Conversions to and from @Either@s
  , EithersF (..)
  , Eithers  (..)

    -- * Folds
  , FoldF (..)
  , Fold  (..)

    -- * Void conversions
  ,  preposterous
  , postposterous

    -- * MTL/transformer utilities
  , catchFM
  , catchM

  , throwFM
  , throwM
  ) where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Trans.Except (ExceptT, mapExceptT, runExceptT)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.Void (Void, absurd)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

-- | The type @VariantF f '[x, y, z]@ is /either/ @f x@, @f y@, or @f z@. The
-- We construct these with @Here@, @There . Here@, and @There . There . Here@
-- respectively, and we can think o fthe number of 'There'-nestings as being
-- the index of our chosen type in the type-level list of options.
--
-- Often, however, we'll want to avoid being too explicit about our list of
-- types, preferring instead to describe it with constraints. See the methods
-- below for more information!
--
-- >>> :t [ Here (pure "Hello"), There (Here (pure True)) ]
-- [ Here (pure "Hello"), There (Here (pure True)) ]
--   :: Applicative f => [VariantF f ([Char] : Bool : xs)]
data VariantF (f :: k -> Type) (xs :: [k]) where
  Here  ::          f x  -> VariantF f (x ': xs)
  There :: VariantF f xs -> VariantF f (x ': xs)

type family AllF (c :: Type -> Constraint) (f :: k -> Type) (xs :: [k]) :: Constraint where
  AllF c f '[     ]  = ()
  AllF c f (x ': xs) = (c (f x), AllF c f xs)

instance (EithersF f xs nested, Arbitrary nested)
    => Arbitrary (VariantF f xs) where
  arbitrary = fmap fromEithersF arbitrary

deriving instance AllF Eq   f xs => Eq   (VariantF f xs)
deriving instance AllF Show f xs => Show (VariantF f xs)
deriving instance (AllF Eq f xs, AllF Ord f xs) => Ord (VariantF f xs)

instance (AllF Semigroup f xs) => Semigroup (VariantF f xs) where
  Here  x <> Here  y = Here  (x <> y)
  Here  _ <> There y = There  y
  There x <> Here  _ = There  x
  There x <> There y = There (x <> y)

instance (Monoid (f x), Semigroup (VariantF f (x ': xs)))
    => Monoid (VariantF f (x ': xs)) where
  mempty = Here mempty

-- | Often, you'll want to have a choice of types that /aren't/ all wrapped in
-- a functor. For this, we provide the 'Variant' type synonym, as well as
-- equivalents of all the functions below. These functions take care of
-- wrapping and unwrapping the 'Identity' wrapper, too, so it should be
-- invisible to users.
type Variant (xs :: [Type])
  = VariantF Identity xs

-- | Remove the first possibility from a variant. One nice possibility here is
-- a function that tells us whether the first type was the one in our variant:
-- @variantF Left Right@. For example:
--
-- >>> :set -XDataKinds
-- >>> variantF Left Right (Here (Identity True) :: Variant '[Bool])
-- Left (Identity True)
--
-- >>> variantF Left Right (There (Here (Identity 3)) :: Variant '[Bool, Int])
-- Right (Here (Identity 3))
variantF :: (f x -> r) -> (VariantF f xs -> r) -> VariantF f (x ': xs) -> r
variantF here there = \case Here x -> here x; There xs -> there xs

-- | Same as 'VariantF', but the value will be unwrapped (not in 'Identity') if
-- found.
--
-- >>> variant Left Right (Here (Identity True) :: Variant '[Bool])
-- Left True
--
-- >>> variant Left Right (There (Here (Identity 3)) :: Variant '[Bool, Int])
-- Right (Here (Identity 3))
variant :: (x -> r) -> (Variant xs -> r) -> Variant (x ': xs) -> r
variant here there = variantF (here . runIdentity) there

class CaseF (xs :: [Type]) (f :: Type -> Type) (r :: Type) (o :: Type)
    | xs f r -> o, o -> f r xs where
  caseF' :: Either r (VariantF f xs) -> o

instance CaseF '[x] f r ((f x -> r) -> r) where
  caseF' (Left   r) _ = r
  caseF' (Right xs) f = xs & variantF f \_ ->
    error $ "Impossible case - something isn't happy when performing the "
         <> "exhaustivity check as this case shouldn't need a pattern-match."

instance CaseF (y ': zs) f r ((f y -> r) -> o)
    => CaseF (x ': y ': zs) f r ((f x -> r) -> (f y -> r) -> o) where
  caseF' xs f = caseF' (xs >>= variantF (Left . f) Right)

-- | The 'either' function provides us with a way of folding an 'Either' by
-- providing a function for each possible constructor: 'Left' and 'Right'. In
-- our case, we could have any number of functions to supply, depending on how
-- many types are in our type-level index.
--
-- This function specialises depending on the variant provided:
--
-- >>> :t caseF (throw True :: Variant '[Bool])
-- caseF (throw True :: Variant '[Bool]) :: (Identity Bool -> r) -> r
--
-- >>> :t caseF (throwF (pure True) :: VariantF IO '[Int, Bool])
-- caseF (throwF (pure True) :: VariantF IO '[Int, Bool])
--   :: (IO Int -> o) -> (IO Bool -> o) -> o
caseF :: CaseF xs f r fold => VariantF f xs -> fold
caseF = caseF' . Right

class Case (xs :: [Type]) (r :: Type) (o :: Type)
    | xs r -> o, o -> r xs where
  case_' :: Either r (Variant xs) -> o

instance Case '[x] r ((x -> r) -> r) where
  case_' (Left   r) _ = r
  case_' (Right xs) f = xs & variantF (f . runIdentity) \_ ->
    error $ "Impossible case - something isn't happy when performing the "
         <> "exhaustivity check as this case shouldn't need a pattern-match."

instance Case (y ': zs) r ((y -> r) -> o)
    => Case (x ': y ': zs) r ((x -> r) -> (y -> r) -> o) where
  case_' xs f = case_' (xs >>= variantF (Left . f . runIdentity) Right)

-- | Same as 'caseF', but without the functor wrappers. Again, this function
-- will specialise according to the provided variant:
--
-- >>> :t case_ (throw True :: Variant '[Bool, Int])
-- case_ (throw True :: Variant '[Bool, Int])
--   :: (Bool -> o) -> (Int -> o) -> o
--
-- You can also use @TypeApplications@ to check the specialisation for a
-- particular variant:
--
-- >>> :set -XTypeApplications
-- >>> :t case_ @'[Int, Bool, String]
-- case_ @'[Int, Bool, String]
--   :: Variant '[Int, Bool, String]
--      -> (Int -> o) -> (Bool -> o) -> ([Char] -> o) -> o
case_ :: Case xs r fold => Variant xs -> fold
case_ = case_' . Right

type family TypeNotFound (x :: k) :: l where
  TypeNotFound x
    = TypeError ( 'Text "Uh oh! I couldn't find " ':<>: 'ShowType x
        ':<>: 'Text " inside the variant!"
        ':$$: 'Text "If you're pretty sure I'm wrong, perhaps the variant "
        ':<>: 'Text "type is ambiguous;"
        ':$$: 'Text "could you add some annotations?" )

-- | When dealing with larger (or polymorphic) variants, it becomes difficult
-- (or impossible) to construct 'VariantF' values explicitly. In that case, the
-- 'throwF' function gives us a polymorphic way to lift values into variants.
--
-- >>> throwF (pure "Hello") :: VariantF Maybe '[Bool, Int, Double, String]
-- There (There (There (Here (Just "Hello"))))
--
-- >>> throwF (pure True) :: VariantF Maybe '[Bool, Int, Double, String]
-- Here (Just True)
--
-- >>> throwF (pure True) :: VariantF IO '[Int, Double, String]
-- ...
-- ... • Uh oh! I couldn't find Bool inside the variant!
-- ...   If you're pretty sure I'm wrong, perhaps the variant type is ambiguous;
-- ...   could you add some annotations?
-- ...
class CouldBeF (xs :: [k]) (x :: k) where
  throwF :: f x -> VariantF f xs 

instance CouldBeF (x ': xs) x where
  throwF = Here

instance {-# OVERLAPPABLE #-} CouldBeF xs x
    => CouldBeF (y ': xs) x where
  throwF = There . throwF

instance TypeNotFound x => CouldBeF '[] x where
  throwF = error "Impossible!"

-- | Just as with 'CouldBeF', we can "throw" values /not/ in a functor context
-- into a regular 'Variant'.
--
-- >>> throw (3 :: Int) :: Variant '[Bool, Int, Double, String]
-- There (Here (Identity 3))
--
-- >>> throw "Woo!" :: Variant '[Bool, Int, Double, String]
-- There (There (There (Here (Identity "Woo!"))))
class CouldBeF xs x => CouldBe (xs :: [Type]) (x :: Type) where
  throw :: x -> Variant xs

instance CouldBeF xs x => CouldBe xs x where
  throw = throwF . Identity

-- | This is an odd constraint, as you should rarely need to /see/ it. GHC's
-- partial instantiation tricks should mean that mentions of this class "cancel
-- out" mentions of 'CouldBeF'. As an example, let's imagine a function that
-- represents some business logic that potentially "throws" either an 'Int' or
-- 'Bool' while it runs:
--
-- >>> :set -XFlexibleContexts -XMonoLocalBinds -XTypeOperators
-- >>> :{
-- f :: (e `CouldBe` Int, e `CouldBe` Bool) => VariantF IO e
-- f = throwF (pure True)
-- :}
--
-- As we can see, there are two constraints here. However, if we "catch" one of
-- these possible errors, we don't just add the 'CatchF' constraint: we /cancel
-- out/ the constraint corresponding to the type we caught:
--
-- >>> :{
-- g :: e `CouldBe` Int => Either (VariantF IO e) (IO Bool)
-- g = catchF @Bool f
-- :}
--
-- This means that constraints only propagate for __uncaught__ exceptions, just
-- as Java functions only need declare exceptions they /haven't/ caught. Once
-- we've caught all the errors, the constraint disappears! This can be a nice
-- way to work if you combine it with something like @ExceptT@.
class CatchF x xs ys | xs x -> ys, xs ys -> x, x ys -> xs where
  catchF :: VariantF f xs -> Either (VariantF f ys) (f x)

instance CatchF x (x ': xs) xs where
  catchF = \case
    Here  x  -> Right x
    There xs -> Left  xs

instance {-# INCOHERENT #-} (y ~ z, CatchF x xs ys)
    => CatchF x (y ': xs) (z ': ys) where
  catchF = \case
    There xs -> first There (catchF xs)
    Here  _  ->
      error $ "Impossible case - something isn't happy when performing the "
           <> "exhaustivity check as this case shouldn't need a pattern-match."

-- | 'throwF' is to 'catchF' as 'throw' is to @catch@. This function allows us
-- to discharge constraints for 'Variant' types. We can revisit the 'catchF'
-- example without the functor wrapper:
--
-- >>> :{
-- f :: (e `CouldBe` Int, e `CouldBe` Bool) => Variant e
-- f = throw True
-- :}
--
-- ... and be similarly excited when we make one of the constraints disappear:
--
-- >>> :{
-- g :: e `CouldBe` Int => Either (Variant e) Bool
-- g = catch @Bool f
-- :}
class CatchF x xs ys => Catch (x :: Type) (xs :: [Type]) (ys :: [Type]) where
  catch :: Variant xs -> Either (Variant ys) x

instance CatchF x xs ys => Catch x xs ys where
  catch = fmap runIdentity . catchF

-- | Occasionally, we might want to use our "nested 'Either'" analogue for
-- whatever reason. For that situation the functions here allow you to swap
-- between the two representations.
--
-- >>> :t toEithersF @IO @'[String, Int, Bool]
-- toEithersF @IO @'[String, Int, Bool]
--   :: VariantF IO '[String, Int, Bool]
--      -> Either (IO [Char]) (Either (IO Int) (IO Bool))
--
-- In order to maintain the round-tripping property (see below), the functional
-- dependency only goes from the variant to the nested either. This is because
-- the opposite doesn't always necessarily make sense.
--
-- If @Variant '[a, b]@ is converted to @Either a b@, it would seem sensible to
-- say the opposite is equally as mechanical. However, consider a nesting like
-- @Either a (Either b c)@: should this translate to @Variant '[a, b, c]@ or
-- @Variant '[a, Either b c]@? There's not a unique mapping in this direction,
-- so we can't add the functional dependency.
--
-- prop> fromEithersF (toEithersF x) == (x :: VariantF Maybe '[Int, String])
class EithersF (f :: Type -> Type) (xs :: [Type]) (o :: Type)
    | f xs -> o, o f -> xs where
  toEithersF   :: VariantF f xs -> o
  fromEithersF :: o -> VariantF f xs

instance EithersF f '[x] (f x) where
  toEithersF = variantF id \_ ->
    error $ "Impossible case - something isn't happy when performing the "
         <> "exhaustivity check as this case shouldn't need a pattern-match."

  fromEithersF = Here

instance (Functor f, EithersF f (y ': xs) zs)
    => EithersF f (x ': y ': xs) (Either (f x) zs) where
  toEithersF = variantF Left (Right . toEithersF)
  fromEithersF = either Here (There . fromEithersF)

-- | The @f@-less analogue of 'EithersF'. The same properties as described
-- above will hold, with the same issues around 'fromEithers' result inference.
--
-- >>> :t toEithers @'[String, Int, Bool]
-- toEithers @'[String, Int, Bool]
--   :: Variant '[String, Int, Bool] -> Either [Char] (Either Int Bool)
--
-- The round-tripping property is also conserved:
--
-- prop> fromEithers (toEithers x) == (x :: Variant '[Int, String, Bool])
class Eithers (xs :: [Type]) (o :: Type) | xs -> o where
  toEithers   :: Variant xs -> o
  fromEithers :: o -> Variant xs

instance Eithers '[x] x where
  toEithers = variant id \_ ->
    error $ "Impossible case - something isn't happy when performing the "
         <> "exhaustivity check as this case shouldn't need a pattern-match."

  fromEithers = Here . Identity

instance Eithers (y ': xs) zs => Eithers (x ': y ': xs) (Either x zs) where
  toEithers   = variant Left (Right . toEithers)
  fromEithers = either (Here . Identity) (There . fromEithers)

-- | A constraint-based fold requires a polymorphic function relying on a
-- shared constraint between all members of the variant. If that's a lot of
-- words, let's see a little example:
--
-- >>> foldF @Show (throwF ["hello"] :: VariantF [] '[(), String, Bool]) show
-- "[\"hello\"]"
--
-- If everything in our variant is 'Show'-friendly, we can fold it with the
-- 'show' function, and we just show whatever is in there!
class FoldF (c :: Type -> Constraint) (xs :: [Type]) where
  foldF :: VariantF f xs -> (forall x. c x => f x -> m) -> m

instance FoldF c '[] where
  foldF xs _ = absurd (preposterous xs)

instance (c x, FoldF c xs) => FoldF c (x ': xs) where
  foldF (Here  x ) f = f x
  foldF (There xs) f = foldF @c xs f

-- | Similarly, we can fold the wrapper-less version in the same way. As an
-- example, if all the types are the same, we can pull out whatever value is in
-- there using the fold interface.
--
-- >>> :set -XRankNTypes -XScopedTypeVariables
-- >>> :{
-- fold' :: forall x xs. Fold ((~) x) xs => Variant xs -> x
-- fold' xs = fold @((~) x) xs id
-- :}
--
-- If all the types in the list are the same, and we can turn values of that
-- type into some result and return it.
class FoldF c xs => Fold (c :: Type -> Constraint) (xs :: [Type]) where
  fold :: Variant xs -> (forall x. c x => x -> m) -> m

instance FoldF c xs => Fold c xs where
  fold xs f = foldF @c xs (f . runIdentity)

-- | A choice of zero types is an uninhabited type! This means we can convert
-- it to 'Void'...
preposterous :: VariantF f '[] -> Void
preposterous = \case

-- | ... and it also means we can convert back!
postposterous :: Void -> VariantF f '[]
postposterous = \case

-- | When working in some monadic context, using 'catch' becomes trickier. The
-- intuitive behaviour is that each 'catch' shrinks the variant in the left
-- side of my 'MonadError', but this is therefore type-changing: as we can only
-- 'throwError' and 'catchError' with a 'MonadError' type, this is impossible!
--
-- To get round this problem, we have to specialise to 'ExceptT', which allows
-- us to map over the error type and change it as we go. If the error we catch
-- is the one in the variant that we want to handle, we pluck it out and deal
-- with it. Otherwise, we "re-throw" the variant minus the one we've handled.
catchFM
  :: forall x e e' f m a
   . ( Monad m
     , CatchF x e e'
     )
  =>         ExceptT (VariantF f e ) m a
  -> (f x -> ExceptT (VariantF f e') m a)
  ->         ExceptT (VariantF f e') m a

catchFM xs recover = mapExceptT (>>= go) xs
  where
    go = \case
      Right success -> pure (Right success)
      Left  failure -> case catchF @x failure of
        Right hit  -> runExceptT (recover hit)
        Left  miss -> pure (Left miss)

-- | Just the same as 'catchFM', but specialised for our plain 'Variant' and
-- sounding much less like a radio station.
catchM
  :: forall x e e' m a
   . ( Monad m
     , Catch x e e'
     )
  =>       ExceptT (Variant e ) m a
  -> (x -> ExceptT (Variant e') m a)
  ->       ExceptT (Variant e') m a

catchM xs recover
  = catchFM xs (recover . runIdentity)

-- | Throw an error into a variant 'MonadError' context. Note that this /isn't/
-- type-changing, so this can work for any 'MonadError', rather than just
-- 'ExceptT'.
throwFM :: (MonadError (VariantF f e) m, e `CouldBe` x) => f x -> m a
throwFM = throwError . throwF

-- | Same as 'throwFM', but without the @f@ context. Given a value of some type
-- within a 'Variant' within a 'MonadError' context, "throw" the error.
throwM :: (MonadError (Variant e) m, e `CouldBe` x) => x -> m a
throwM = throwFM . Identity

-- | Worked example:
--
-- >>> :set -XBlockArguments -XLambdaCase
-- >>> import Control.Monad.IO.Class (liftIO)
--
-- Let's declare some error types for our example app:
--
-- >>> data NetworkError      = NetworkError
-- >>> data UserNotFoundError = UserNotFoundError
--
-- Now these are in place, we can write a bit of business logic. This is
-- extremely contrived, but hopefully illustrates the point: we have a function
-- in which a number of things might go wrong!
--
-- >>> :{
-- getUser
--   :: ( e `CouldBe` NetworkError
--      , e `CouldBe` UserNotFoundError
--      )
--   => String
--   -> ExceptT (Variant e) IO String
-- getUser = \case
--   "Alice" -> throwM NetworkError
--   "Tom"   -> pure "Hi, Tom!"
--   _       -> throwM UserNotFoundError
-- :}
--
-- When we come to render a user's profile, we can deal with a missing user. If
-- something went wrong with the network, though, we'll handle that further up
-- the call stack:
--
-- >>> :{
-- renderProfile
--   :: e `CouldBe` NetworkError -- No mention of @UserNotFoundError@!
--   => String
--   -> ExceptT (Variant e) IO ()
-- renderProfile username = do
--   name <- catchM @UserNotFoundError (getUser username) \_ -> do
--     liftIO (putStrLn "ERROR! USER NOT FOUND. Defaulting to 'guest'.")
--     pure "Hello, mysterious stranger!"
--   liftIO (putStrLn name)
-- :}
--
-- Notice that the @UserNotFoundError@ constraint has disappeared! By using
-- 'catchM', we have /dispatched/ this constraint!
