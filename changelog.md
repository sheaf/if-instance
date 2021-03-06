
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
