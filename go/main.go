package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"
	"net"
	"net/http"
	u "net/url"
	"strings"
	"sync"
	"sync/atomic"
	"time"
)

const requestTimeout = 6 * time.Second

var totalWorkItems int32 = 0

type VisitedURL struct {
	query       Query
	Status      string
	LinkedUrls  map[u.URL]bool
	HadProblems bool
}

type Result struct {
	Url    string
	Status string
}

func contentCanHaveLinks(r http.Response) bool {
	return strings.Contains(r.Header["Content-Type"][0], "text/html")
}

func collectLinks(query Query, checkDomain func(u.URL) bool) (chan VisitedURL, chan error) {
	c := make(chan VisitedURL)
	errChan := make(chan error)
	dialTimeout := func(network, addr string) (net.Conn, error) {
		return net.DialTimeout(network, addr, requestTimeout)
	}
	transport := http.Transport{Dial: dialTimeout}
	client := http.Client{Transport: &transport}

	go func() {
		defer close(c)
		defer close(errChan)
		Trace.Printf("~~~~~~ trying %s\n", query.url.String())
		res, err := client.Head(query.url.String())
		linkedUrls := make(map[u.URL]bool)
		if err != nil {
			c <- VisitedURL{query, fmt.Sprintf("Error:%s", err), linkedUrls, true}
			Error.Printf("error occured during http.Head for %s:%s\n", query.url.String(), err)
			return
		}
		if res.StatusCode < 200 || res.StatusCode >= 300 {
			c <- VisitedURL{query, fmt.Sprintf("Status:%d", res.StatusCode), linkedUrls, true}
			Trace.Printf("HTTP status not OK %s (%d)\n", query.url.String(), res.StatusCode)
			return
		}
		if checkDomain(query.url) && contentCanHaveLinks(*res) {
			Trace.Printf("~~~~~~ crawling %s\n", query.url.String())
			linkChan := GetAllLinks(query.url, errChan)
			for e := range linkChan {
				if !linkedUrls[e] {
					linkedUrls[e] = true
					atomic.AddInt32(&totalWorkItems, 1)
					Trace.Printf(" ===========> appending (now %d items) %s\n", totalWorkItems, e.String())
				}
			}
		}
		c <- VisitedURL{query, fmt.Sprintf("Status:%d", res.StatusCode), linkedUrls, false}
	}()
	return c, errChan
}

type Query struct {
	url    u.URL
	origin string
}

type Search struct {
	toQuery     <-chan Query      // pull next url to test from this channel
	unfiltered  chan<- Query      // send all found links back
	results     chan<- VisitedURL // add processed page to results
	quit        <-chan bool       // listen to when we should quit
	domainCheck func(u.URL) bool  // check if the url should be queried further
	id          int               // id of worker go routine
	wg          *sync.WaitGroup
	workerWg    *sync.WaitGroup
}

func searchPage(s Search) {
	defer s.workerWg.Done()
	for {
		select {
		case query, ok := <-s.toQuery:
			if !ok {
				return
			}
			Trace.Printf("[%d]..............searching %s\n", s.id, query.url.String())
			testedChan, errChan := collectLinks(query, s.domainCheck)
			select {
			case t := <-testedChan:
				for v := range t.LinkedUrls {
					s.wg.Add(1)
					s.unfiltered <- Query{v, query.url.String()}
					atomic.AddInt32(&totalWorkItems, -1)
				}
				s.results <- t
				break
			case err := <-errChan:
				Error.Print(err.Error())
			}
			s.wg.Done()
		case <-s.quit:
			fmt.Print("Q")
			return
		}
	}
}

func main() {
	SetLogLevel(INFO)
	urlString := flag.String("s", "http://esrlabs.com", "the URL of the site to check links")
	parallel := flag.Int("p", 15, "number of parallel executions")
	flag.Parse()
	url, err := u.Parse(*urlString)
	if err != nil {
		log.Fatal("invalid url:")
	}
	ip, err := net.LookupIP(url.Host)
	if err != nil {
		log.Fatal("could not lookup ip of \"", url, "\"")
	}
	Info.Printf("host:%s, ip: %s\n", url.Host, ip)

	domainCheck := createDomainCheck(*url, ip)
	toQuery := make(chan Query, 10000) //TODO use go-routine to add to query queue
	unfiltered := make(chan Query, 10000)
	results := make(chan VisitedURL)
	quit := make(chan bool)
	var wg sync.WaitGroup
	var workerWg sync.WaitGroup

	wg.Add(1)
	toQuery <- Query{*url, ""}

	workerWg.Add(*parallel)
	for i := 0; i < *parallel; i++ {
		s := Search{toQuery, unfiltered, results, quit, domainCheck, i, &wg, &workerWg}
		go searchPage(s)
	}
	// setup filtering
	go filterNonRelevant(unfiltered, toQuery, &wg)

	checked, errors := 0, 0
	go func() {
		for v := range results {
			checked = checked + 1
			logFn := Info.Printf
			if v.HadProblems {
				logFn = Warning.Printf
				errors = errors + 1
			}
			logFn("Checked[%d]: %s (%s) (origin:%s)\n", checked, v.query.url.String(), v.Status, v.query.origin)
		}
	}()

	Info.Printf("waiting for queue...\n")
	wg.Wait()
	fmt.Printf("queue done!! %d pages checked, %d had problems (%d%%)\n", checked, errors, errors*100/checked)
	for i := 0; i < *parallel; i++ {
		quit <- true
	}
	Info.Printf("waiting for crunchers...\n")
	workerWg.Wait()
	fmt.Println("")
	Info.Printf("crunchers done!!\n")
	close(results)
}

func createDomainCheck(url u.URL, ip []net.IP) func(u.URL) bool {
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
	return domainCheck
}

func filterNonRelevant(in <-chan Query, out chan<- Query, wg *sync.WaitGroup) {
	var checked = make(map[string]bool)
	for v := range in {
		link := removeParameters(v.url)
		if !checked[link] {
			checked[link] = true
			out <- v
			Trace.Printf(".........................added %s [host:%s]\n", v.url.String(), v.url.Host)
		} else {
			wg.Done() //already counted for, but we don't use it
		}
	}
}
