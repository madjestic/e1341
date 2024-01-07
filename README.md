# e1341
a rewrite of e1340 space-sim game in terms of MSFs

## Creating a new Material:
```bash
$ cabal run genMaterial matDir/matName
$ cabal run genMaterial mat/graph
> ./mat/graph/graph...
```
(that generates a ./mat/testMat01 material directory with a default conent (constant shader material)

## Fix material's UUIDs:
```bash
$ cabal run exe:genUUID -- -m mat/test/test
```
