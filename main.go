package main

import (
	"bytes"
	"flag"
	"fmt"
	"github.com/PuerkitoBio/goquery"
	"log"
	"net"
	"net/http"
	u "net/url"
	"strings"
	"sync"
)

const WORKERS int = 20

type VisitedUrl struct {
	Url        u.URL
	Status     int
	LinkedUrls []u.URL
}

type Result struct {
	Url    string
	Status int
}

func stripInPageLink(s string) (*u.URL, error) {
	_s := strings.Split(s, "#")[0]
	parsed, err := u.Parse(_s)
	if err != nil {
		return nil, err
	}
	return u.Parse(removeParameters(*parsed))
}

func collectLinks(url u.URL) (chan VisitedUrl, chan error) {
	c := make(chan VisitedUrl)
	errChan := make(chan error)
	go func() {
		res, err := http.Get(url.String())
		if err != nil {
			errChan <- err
		}
		linkedUrls := make([]u.URL, 0)
		if res.StatusCode >= 200 && res.StatusCode < 300 {
			doc, err := goquery.NewDocumentFromResponse(res)
			if err != nil {
				errChan <- err
			}
			doc.Find("a").Each(func(_ int, sel *goquery.Selection) {
				href, _ := sel.Attr("href")
				parsed, err := stripInPageLink(href)
				if err != nil {
					errChan <- err
					return
				}
				if parsed.String() != "" {
					ref := *url.ResolveReference(parsed)
					fmt.Printf(" ===========> appending %s\n", ref.String())
					linkedUrls = append(linkedUrls, ref)
				}
			})
		}
		c <- VisitedUrl{url, res.StatusCode, linkedUrls}
		close(c)
	}()
	return c, errChan
}

type Search struct {
	toQuery    <-chan u.URL      // pull next url to test from this channel
	unfiltered chan<- u.URL      // send all found links back
	results    chan<- VisitedUrl // send all found links back
	quit       <-chan bool       // listen to when we should quit
	id         int
	wg         *sync.WaitGroup
	workerWg   *sync.WaitGroup
}

func searchPage(s Search) {
	defer s.workerWg.Done()
	for {
		select {
		case url, ok := <-s.toQuery:
			if !ok {
				return
			}
			fmt.Printf("[%d]..............searching %s\n", s.id, url.String())
			testedChan, errChan := collectLinks(url)
			select {
			case t := <-testedChan:
				for _, v := range t.LinkedUrls {
					s.wg.Add(1)
					s.unfiltered <- v
				}
				s.results <- t
				break
			case err := <-errChan:
				log.Print(err)
			}
			s.wg.Done()
		case <-s.quit:
			fmt.Println("QUITTING")
			return
		}
	}
}

func main() {
	urlString := flag.String("s", "http://esrlabs.com", "the URL of the site to check links")
	flag.Parse()
	url, err := u.Parse(*urlString)
	if err != nil {
		log.Fatal("invalid url:")
	}
	ip, err := net.LookupIP(url.Host)
	if err != nil {
		log.Fatal("could not lookup ip of \"", url, "\"")
	}
	fmt.Printf("host:%s, ip: %s\n", url.Host, ip)
	domainCheck := func(url2check u.URL) bool {
		domain := strings.TrimPrefix(url.Host, "www.")
		if strings.HasSuffix(url2check.Host, domain) {
			return true
		}
		// if cheap test doesn't work, lookup IP
		_ip, err := net.LookupIP(url2check.Host)
		if err != nil {
			return false
		}
		return bytes.Equal(_ip[0], ip[0])
	}
	toQuery := make(chan u.URL, 10000)
	unfiltered := make(chan u.URL, 10000)
	results := make(chan VisitedUrl)
	filteredResults := make(chan Result)
	quit := make(chan bool)
	var wg sync.WaitGroup
	var workerWg sync.WaitGroup

	wg.Add(1)
	toQuery <- *url

	workerWg.Add(WORKERS)
	for i := 0; i < WORKERS; i++ {
		s := Search{toQuery, unfiltered, results, quit, i, &wg, &workerWg}
		go searchPage(s)
	}
	// setup filtering
	go filterNonRelevant(unfiltered, toQuery, &wg, domainCheck)
	go filterSeenResults(results, filteredResults)

	go func() {
		for v := range results {
			fmt.Printf("Checked: %s (Status %d)\n", v.Url.String(), v.Status)
		}
	}()

	fmt.Printf("waiting for queue...\n")
	wg.Wait()
	fmt.Printf("queue done!!\n")
	for i := 0; i < WORKERS; i++ {
		quit <- true
	}
	fmt.Printf("waiting for crunchers...\n")
	workerWg.Wait()
	fmt.Printf("crunchers done!!\n")
	close(results)
}

func filterSeenResults(results <-chan VisitedUrl, filteredResults chan<- Result) {
	var seen = make(map[Result]bool)
	for v := range results {
		r := Result{v.Url.String(), v.Status}
		if !seen[r] {
			seen[r] = true
			filteredResults <- r
		}
	}
}
func removeParameters(url u.URL) string {
	if url.Scheme == "" {
		return url.String()
	}
	return url.Scheme + "://" + url.Host + url.Path
}

func filterNonRelevant(in <-chan u.URL, out chan<- u.URL, wg *sync.WaitGroup, domainCheck func(u.URL) bool) {
	var checked = make(map[string]bool)
	for v := range in {
		link := removeParameters(v)
		if !checked[link] {
			checked[link] = true
			if domainCheck(v) {
				out <- v
				fmt.Printf(".........................added %s [host:%s]\n", v.String(), v.Host)
			}
		} else {
			wg.Done() //already counted for, but we don't use it
		}
	}
}
