# Checking links on a website

...is actually a very good problem to get to know a programming language!

## Go-solution

Concurrently executing link-check for a given html site.

go-dependencies:

* github.com/PuerkitoBio/goquery (to scrap links from html documents)
* github.com/fatih/color (to color my log output)

### Example Usage

    go run *.go -s=http://esrlabs.com -p=20

## Python-solution

straight forward concurrency, multi-threaded solution

### Example Usage

    ./check.py -s http://esrlabs.com -p 20

## Haskell-solution

using STM for all concurrency

### Example Usage

    $ cd haskell
    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal build linkcheck
    $ cabal run linkcheck

