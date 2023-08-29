
# Version 0.5.1.0 (2023-08-30)

- Be more thorough when resetting GHC solver monad state. This should ensure
  transparent backtracking after giving up on the LHS of a disjunction
  constraint.

# Version 0.5.0.0 (2023-08-29)

- Add a fixity declaration for `(||)` (`infixr 2`, matching term-level disjunction).

- Reset the GHC solver monad state after failing to solve the LHS constraint in
  a disjunction.

- Require `ghc-tcplugin-api >= 0.11`.

# Version 0.4.0.0 (2023-08-09)

- Only consider a constraint solved when there are no residual constraints.

- Bump version bounds for `ghc-tcplugin-api`.

# Version 0.3.1.0 (2023-01-24)

- Bumping of version bounds, and support for GHC 9.4 and GHC 9.6.
  Now requires `ghc-tcplugin-api` 0.9 or above.

# Version 0.3.0.0 (2021-09-01)

- Add the `(||)` constraint disjunction mechanism, with

```haskell
dispatch :: ( c || d ) => ( c => r ) -> ( d => r ) -> r
```

This allows users to select between two different constraints.
This is more general than `ifCt`, as it allows extra constraints
in the fallback branch.

- Implement `IfCt`, `ifCt` in terms of `(||)` and `dispatch`.
  `IfCt` becomes a type synonym, which means that it no longer
  bundles `ifCt`.

# Version 0.2.1.1 (2021-08-31)

- Minor documentation improvements.

# Version 0.2.1.0 (2021-08-31)

- Require `ghc-tcplugin-api >= 0.5.1.0`.

# Version 0.2.0.0 (2021-08-31)

- Add a type family `IsSat :: Constraint -> Bool`
  that computes whether a type-family is satisfied in
  the current context.

- Rename `IfCt` to `IfSat`.

# Version 0.1.0.0 (2021-08-30)

Initial release.
