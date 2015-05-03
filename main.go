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
	"sync/atomic"
)

var totalWorkItems int32 = 0

type VisitedUrl struct {
	Url        u.URL
	Status     string
	LinkedUrls map[u.URL]bool
}

type Result struct {
	Url    string
	Status string
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

func contentCanHaveLinks(r http.Response) bool {
	return strings.Contains(r.Header["Content-Type"][0], "text/html")
}

func collectLinks(url u.URL, checkDomain func(u.URL) bool) (chan VisitedUrl, chan error) {
	c := make(chan VisitedUrl)
	errChan := make(chan error)
	go func() {
		defer close(c)
		defer close(errChan)
		fmt.Printf("~~~~~~ trying %s\n", url.String())
		res, err := http.Head(url.String())
		linkedUrls := make(map[u.URL]bool)
		if err != nil {
			c <- VisitedUrl{url, fmt.Sprintf("Error:%d", err), linkedUrls}
			log.Printf("error occured during http.Head for %s:%s\n", url.String(), err)
			return
		}

		if res.StatusCode < 200 || res.StatusCode >= 300 {
			c <- VisitedUrl{url, fmt.Sprintf("Status:%d", res.StatusCode), linkedUrls}
			return
		}
		if checkDomain(url) && contentCanHaveLinks(*res) {
			fmt.Printf("~~~~~~ crawling %s\n", url.String())
			res, err := http.Get(url.String())
			if err != nil {
				errChan <- err
				log.Printf("error occured during http.Get for %s\n", url.String())
				return
			}
			doc, err := goquery.NewDocumentFromResponse(res)
			if err != nil {
				errChan <- err
				return
			}
			doc.Find("a").Each(func(_ int, sel *goquery.Selection) {
				href, _ := sel.Attr("href")
				parsed, err := stripInPageLink(href)
				if err != nil {
					errChan <- err
					return
				}
				str := parsed.String()
				if str != "" {
					ref := *url.ResolveReference(parsed)
					if !linkedUrls[ref] &&
						(ref.Scheme == "http" || ref.Scheme == "https") {
						linkedUrls[ref] = true
						atomic.AddInt32(&totalWorkItems, 1)
						fmt.Printf(" ===========> appending (now %d items) %s\n", totalWorkItems, ref.String())
					}
				}
			})
		}
		c <- VisitedUrl{url, fmt.Sprintf("Status:%d", res.StatusCode), linkedUrls}
	}()
	return c, errChan
}

type Search struct {
	toQuery     <-chan u.URL      // pull next url to test from this channel
	unfiltered  chan<- u.URL      // send all found links back
	results     chan<- VisitedUrl // send all found links back
	quit        <-chan bool       // listen to when we should quit
	domainCheck func(u.URL) bool
	id          int
	wg          *sync.WaitGroup
	workerWg    *sync.WaitGroup
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
			testedChan, errChan := collectLinks(url, s.domainCheck)
			select {
			case t := <-testedChan:
				for v := range t.LinkedUrls {
					s.wg.Add(1)
					s.unfiltered <- v
					atomic.AddInt32(&totalWorkItems, -1)
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
	parallel := flag.Int("p", 5, "number of parallel executions")
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

	workerWg.Add(*parallel)
	for i := 0; i < *parallel; i++ {
		s := Search{toQuery, unfiltered, results, quit, domainCheck, i, &wg, &workerWg}
		go searchPage(s)
	}
	// setup filtering
	go filterNonRelevant(unfiltered, toQuery, &wg)
	go filterSeenResults(results, filteredResults)

	go func() {
		for v := range results {
			fmt.Printf("Checked: %s (%s)\n", v.Url.String(), v.Status)
		}
	}()

	fmt.Printf("waiting for queue...\n")
	wg.Wait()
	fmt.Printf("queue done!!\n")
	for i := 0; i < *parallel; i++ {
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

func filterNonRelevant(in <-chan u.URL, out chan<- u.URL, wg *sync.WaitGroup) {
	var checked = make(map[string]bool)
	for v := range in {
		link := removeParameters(v)
		if !checked[link] {
			checked[link] = true
			out <- v
			fmt.Printf(".........................added %s [host:%s]\n", v.String(), v.Host)
		} else {
			wg.Done() //already counted for, but we don't use it
		}
	}
}
