{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: Data.Constraint.If

This module defines the constraint disjunction typeclass '||', with method 'dispatch':

> dispatch :: ( c || d ) => ( c => r ) -> ( d => r ) -> r

An expression of the form @ dispatch \@c \@d yes no @ denotes a selection between the two
branches @yes@ and @no@:

  - if the constraint @c@ can be determined to hold at the point of solving @c || d@,
    then the @yes@ branch is selected, which has access to evidence for the @c@ constraint;
  - otherwise, the fallback branch @no@ is selected, which has access to evidence for @d@.

If you don't need additional constraints in the fallback branch, you can also use:

> ifSat :: IfSat c => ( c => r ) -> r -> r

This is the special case of 'dispatch' which taes @d@ to be the trivial constraint,
@d ~ ( () :: Constraint)@.

This module also provides the type family @'IsSat' :: Constraint -> Bool@, which, when reduced,
will check whether the constraint provided as an argument is satisfied.

To use this functionality, you must enable the corresponding 'IfSat.Plugin.plugin',
by adding @\{\-\# OPTIONS_GHC -fplugin=IfSat.Plugin \#\-\}@
to the header of your module.

== Example

We can select the more efficient 'nubOrd' function when an 'Ord' instance
is available:

> myNub :: forall (a :: Type). ( Eq a, IfSat (Ord a) ) => [a] -> [a]
> myNub = ifSat @(Ord a) nubOrd nub
>  -- 'nubOrd' when 'Ord a' is satisfied, 'nub' otherwise.

When a user calls @myNub@, e.g.:

> foo :: [(Int, Int)]
> foo = myNub [(1,2), (3,3), (1,2), (2,2), (1,2), (1,4)]

GHC will discharge the @IfSat (Ord (Int,Int))@ constraint by trying to solve
the @Ord (Int, Int)@ constraint. In this case, GHC can solve the constraint
using the two top-level instances (which we assume are in scope):

> instance Ord Int
> instance (Ord a, Ord b) => Ord (a,b)

As the @ Ord (Int,Int) @ can be solved, GHC thus choose the first branch
in 'ifSat', which in this case is 'nubOrd'.

== When does branch selection occur?

What is important to understand is that the branch selection happens
precisely when the @IfSat ct@ constraint is solved.


> { -# OPTIONS_GHC -fplugin=IfSat.Plugin #- }
> module M1 where
>
> showFun :: forall (a :: Type). IfSat ( Show ( a -> a ) ) => ( a -> a ) -> String
> showFun = ifSat @( Show (a -> a) ) show ( \ _ -> "<<function>>" )
>
> test1 :: ( Bool -> Bool ) -> String
> test1 fun = showFun fun
>
> ----------------------------------------
>
> { -# OPTIONS_GHC -fplugin=IfSat.Plugin #- }
> module M2 where
>
> import M1
>
> instance Show ( Bool -> Bool ) where
>   show f = show [ f False, f True ]
>
> test2 :: ( a -> a ) -> String
> test2 fun = showFun fun
>
> test3 :: ( Bool -> Bool ) -> String
> test3 fun = showFun fun

After loading @M2@, we get the following results:

>>> test1 not
"<<function>>"

In this example, to typecheck @test1@ we need to solve @IfSat (Show (Bool -> Bool))@
inside module @M1@.
As no instance for @Show (Bool -> Bool)@ is available in @M1@, we pick the second branch,
resulting in @"\<\<function\>\>"@.

>>> test2 not
"<<function>>"

In this example, we must solve @IfSat (Show (a -> a))@ within @M2@. There is no such instance in @M2@,
so we pick the second branch.  
It doesn't matter that we are calling @test2@ with a function of type
@Bool -> Bool@: we had to solve @IfSat (Show (a -> a))@ when type-checking
the type signature of @test2@.

>>> test3 not
"[True, False]"

In this last example, we must solve @IfSat (Show (Bool -> Bool))@, but as we're in @M2@,
such an instance is available, so we choose the first branch.

Note in particular that @test1@ and @test3@ have the exact same definition (same type signature,
same body), but produce a different result. This is because the satisfiability check happens in
different contexts.
-}

module Data.Constraint.If
  ( type (||)(dispatch), IfSat, ifSat, IsSat )
  where

-- base
import Data.Kind
  ( Constraint )

--------------------------------------------------------------------------------


type (||) :: Constraint -> Constraint -> Constraint
class c || d where
  -- | @dispatch \@c \@d a b@ returns @a@ if the constraint @c@ is satisfied,
  -- otherwise @b@.
  --
  -- > dispatch :: ( c || d ) => ( c => r ) -> ( d => r ) -> r
  --
  -- Requires the if-instance 'IfSat.Plugin.plugin':
  -- add @{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}@
  -- to the header of your module.
  --
  -- Note: the selection happens at the point in the code where the @c || d@
  -- constraint is solved.
  dispatch :: ( ( IsSat c ~ True, c ) => r )
           -> ( ( IsSat c ~ False, IsSat d ~ True, d ) => r )
           -> r

type IfSat :: Constraint -> Constraint
type IfSat ct = ( ct || () )
  -- IfSat must be a type synonym, not a class newtype such as
  --
  -- > class    ( ct || () ) => IfSat ct
  -- > instance ( ct || () ) => IfSat ct
  --
  -- Otherwise, mentions of 'IfSat' can cause GHC to rewrite
  -- too early.

-- | @ ifSat \@ct a b@ returns @a@ if the constraint @ct@ is satisfied,
-- and @b@ otherwise.
--
-- Requires the if-instance 'IfSat.Plugin.plugin':
-- add @{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}@
-- to the header of your module.
--
-- Note: the selection happens at the point in the code where the @IfSat ct@
-- constraint is solved.
ifSat :: forall ct r
      .  ( IfSat ct )
      => ( ( IsSat ct ~ True, ct ) => r )
      -> ( ( IsSat ct ~ False ) => r )
      -> r
ifSat f g = dispatch @ct @() f g

-- | @IsSat ct@ returns @True@ if @ct@ is satisfied, and @False@ otherwise.
--
-- The satisfiability check occurs at the moment of performing type-family reduction.
type IsSat :: Constraint -> Bool
type family IsSat ct where
