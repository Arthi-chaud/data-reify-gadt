# `Data.Reify.GADT`

[![Hackage Version](https://img.shields.io/hackage/v/data-reify-gadt)](https://hackage.haskell.org/package/data-reify-gadt)
[![Doc](https://img.shields.io/badge/Documentation-Haddock-purple)](https://hackage.haskell.org/package/data-reify-gadt-0.1.0.0/docs/Data-Reify-GADT.html)
[![CI](https://github.com/Arthi-chaud/data-reify-gadt/actions/workflows/CI.yml/badge.svg)](https://github.com/Arthi-chaud/data-reify-gadt/actions/workflows/CI.yml)


`data-reify-gadt` is a rewrite of [`data-reify`](https://hackage.haskell.org/package/data-reify) to make the library usable with GADTs.

With this implementation, it is possible to use `data-reify`'s technique to make a graph out of typed ASTs that use GADTs.

The logic is the same, only the type definition changes slightly.

Take a look at the `examples/` directory.
