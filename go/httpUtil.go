package main

import (
	"github.com/PuerkitoBio/goquery"
	"net/http"
	u "net/url"
	"strings"
)

func removeParameters(url u.URL) string {
	if url.Scheme == "" {
		return url.String()
	}
	return url.Scheme + "://" + url.Host + url.Path
}

func stripInPageLink(s string) (*u.URL, error) {
	noPageLinks := strings.Split(s, "#")[0]
	_s := strings.Split(noPageLinks, "?")[0]
	parsed, err := u.Parse(_s)
	if err != nil {
		return nil, err
	}
	return u.Parse(removeParameters(*parsed))
}

func GetAllLinks(url u.URL, errChan chan<- error) chan u.URL {
	linkChan := make(chan u.URL)
	go func() {
		res, err := http.Get(url.String())
		if err != nil {
			errChan <- err
			Error.Printf("error occured during http.Get for %s\n", url.String())
		} else {
			doc, err := goquery.NewDocumentFromResponse(res)
			if err != nil {
				errChan <- err
			} else {
				doc.Find("a").Each(func(_ int, sel *goquery.Selection) {
					href, _ := sel.Attr("href")
					parsed, err := stripInPageLink(href)
					if err != nil {
						errChan <- err
						return
					}
					str := parsed.String()
					if str != "" {
						ref := *url.ResolveReference(parsed) //get absolute URL
						if ref.Scheme == "http" || ref.Scheme == "https" {
							Trace.Printf(" ===========> appending (now %d items) %s\n", totalWorkItems, ref.String())
							linkChan <- ref
						}
					}
				})
			}
		}
		close(linkChan)
	}()
	return linkChan
}
