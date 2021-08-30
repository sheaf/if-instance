{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-|
Module: Data.Constraint.If

This module defines the typeclass 'IfCt', with method 'ifCt':

> ifCt :: forall (ct :: Constraint) (r :: Type). IfCt ct => ( ct => r ) -> r -> r

An expression of the form @ ifCt \@ct yes no @ denotes a selection between the two
branches @yes@ and @no@:

  - if the constraint @ct@ can be determined to hold at the point of solving @IfCt ct@,
    then the @yes@ branch is selected, which has access to the @ct@ constraint;
  - otherwise, the fallback branch @no@ is selected.

To use this, you will also need to enable the corresponding 'IfCt.Plugin.plugin',
by adding @\{\-\# OPTIONS_GHC -fplugin=IfCt.Plugin \#\-\}@
to the header of your module.

== Example

We can select the more efficient 'nubOrd' function when an 'Ord' instance
is available:

> myNub :: forall (a :: Type). ( Eq a, IfCt (Ord a) ) => [a] -> [a]
> myNub = ifCt @(Ord a) nubOrd nub
>  -- 'nubOrd' when 'Ord a' is satisfied, 'nub' otherwise.

When a user calls @myNub@, e.g.:

> foo :: [(Int, Int)]
> foo = myNub [(1,2), (3,3), (1,2), (2,2), (1,2), (1,4)]

GHC will discharge the @IfCt (Ord (Int,Int))@ constraint by trying to solve
the @Ord (Int, Int)@ constraint. In this case, GHC can solve the constraint
using the two top-level instances (which we assume are in scope):

> instance Ord Int
> instance (Ord a, Ord b) => Ord (a,b)

As the @ Ord (Int,Int) @ can be solved, GHC thus choose the first branch
in 'ifCt', which in this case is 'nubOrd'.

== When does branch selection occur?

What is important to understand is that the branch selection happens
precisely when the @IfCt ct@ constraint is solved.


> { -# OPTIONS_GHC -fplugin=IfCt.Plugin #- }
> module M1 where
>
> showFun :: forall (a :: Type). IfCt ( Show ( a -> a ) ) => ( a -> a ) -> String
> showFun = ifCt @( Show (a -> a) ) show ( \ _ -> "<<function>>" )
>
> test1 :: ( Bool -> Bool ) -> String
> test1 fun = showFun fun
>
> ----------------------------------------
>
> { -# OPTIONS_GHC -fplugin=IfCt.Plugin #- }
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

In this example, to typecheck @test1@ we need to solve @IfCt (Show (Bool -> Bool))@.
As no instance for @Show (Bool -> Bool)@ is available in @M1@, we pick the second branch,
resulting in @"\<\<function\>\>"@.

>>> test2 not
"<<function>>"

In this example, we must solve @IfCt (Show (a -> a))@. There is no such instance in @M2@,
so we pick the second branch.

>>> test3 not
"[True, False]"

>>> showFun not
"[True, False]"

In these last two examples, we must solve @IfCt (Show (Bool -> Bool))@.
Such an instance is in scope in @M2@, so we choose the first branch.
-}

module Data.Constraint.If
  ( IfCt(..) )
  where

-- base
import Data.Kind
  ( Constraint )

--------------------------------------------------------------------------------

type IfCt :: Constraint -> Constraint
class IfCt ct where
  -- | @ ifCt \@ct a b@ returns @a@ if the constraint is satisfied,
  -- and @b@ otherwise.
  --
  -- Requires the if-instance 'IfCt.Plugin.plugin':
  -- add @{-# OPTIONS_GHC -fplugin=IfCt.Plugin #-}@
  -- to the header of your module.
  --
  -- Note: the selection happens at the point in the code where the @IfCt ct@
  -- constraint is solved.
  ifCt :: ( ct => r ) -> r -> r
