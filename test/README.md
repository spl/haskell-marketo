# Marketo Test

## Install dependencies

```
$ cabal sandbox init
$ cabal sandbox add-source ..
$ cabal --require-sandbox install -j --only-dependencies --max-backjumps=-1 --reorder-goals
```

## Build and run

```
$ cabal run
```
