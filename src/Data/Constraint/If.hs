{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module: Data.Constraint.If

This module defines the typeclass 'IfSat', with method 'ifSat':

> ifSat :: forall (ct :: Constraint) (r :: Type). IfSat ct => ( ct => r ) -> r -> r

An expression of the form @ ifSat \@ct yes no @ denotes a selection between the two
branches @yes@ and @no@:

  - if the constraint @ct@ can be determined to hold at the point of solving @IfSat ct@,
    then the @yes@ branch is selected, which has access to the @ct@ constraint;
  - otherwise, the fallback branch @no@ is selected.

To use this, you will also need to enable the corresponding 'IfSat.Plugin.plugin',
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

In this example, to typecheck @test1@ we need to solve @IfSat (Show (Bool -> Bool))@.
As no instance for @Show (Bool -> Bool)@ is available in @M1@, we pick the second branch,
resulting in @"\<\<function\>\>"@.

>>> test2 not
"<<function>>"

In this example, we must solve @IfSat (Show (a -> a))@. There is no such instance in @M2@,
so we pick the second branch.

>>> test3 not
"[True, False]"

>>> showFun not
"[True, False]"

In these last two examples, we must solve @IfSat (Show (Bool -> Bool))@.
Such an instance is in scope in @M2@, so we choose the first branch.
-}

module Data.Constraint.If
  ( IfSat(..), IsSat )
  where

-- base
import Data.Kind
  ( Constraint )

--------------------------------------------------------------------------------

type IfSat :: Constraint -> Constraint
class IfSat ct where
  -- | @ IfSat \@ct a b@ returns @a@ if the constraint is satisfied,
  -- and @b@ otherwise.
  --
  -- Requires the if-instance 'IfSat.Plugin.plugin':
  -- add @{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}@
  -- to the header of your module.
  --
  -- Note: the selection happens at the point in the code where the @IfSat ct@
  -- constraint is solved.
  ifSat :: ( ( IsSat ct ~ True, ct ) => r )
       -> ( IsSat ct ~ False => r)
       -> r

-- | @IsSat ct@ returns @True@ if @ct@ is satified, and @False@ otherwise.
--
-- The satisfiability check occurs at the moment of type-family reduction.
type IsSat :: Constraint -> Bool
type family IsSat ct where
