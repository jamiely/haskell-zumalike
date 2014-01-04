Test
====

```bash
cabal build && cabal test

# or, for colored-output
cabal build && ./dist/build/test-haskell-zumalike/test-haskell-zumalike
```

Todo
====

* Pull balls from the queue to shoot them instead of generating new
  balls each time.
* Only match/disappear a set of blocks if they are touching. 
* When there is a gap between balls, that is they are not touching, and
  the balls at each side are the same color, then the farther set of
  balls should get pulled back until the gap is closed.
* Matches should not immediately disappear, giving time for multiple
  gaps to close.

