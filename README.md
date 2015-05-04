## Go-solution

Concurrently executing link-check for a given html site.

go-dependencies:

* github.com/PuerkitoBio/goquery (to scrap links from html documents)
* github.com/fatih/color (to color my log output)

### Example Usage

    go run *.go -s=http://esrlabs.com -p=20
