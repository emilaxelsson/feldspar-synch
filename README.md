# feldspar-synch

Synchronous streams for Feldspar

## Installation

This package depends on [raw-feldspar](http://hackage.haskell.org/package/raw-feldspar).

When installing dependencies in a sandbox, you can get a faster experience and a smaller sandbox by passing a constraint for `language-c-quote`:

    cabal install --only-dependencies --constraint="language-c-quote -full-haskell-antiquotes"

## Examples

[Synth.hs](../../tree/master/examples/Synth.hs) is a simple synthesizer defined using this library.

