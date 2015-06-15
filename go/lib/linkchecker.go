package lib

import (
	"bytes"
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
	Query       QueryT
	Status      string
	LinkedUrls  map[u.URL]bool
	HadProblems bool
}

type QueryT struct {
	Url    u.URL
	Origin string
}

type Search struct {
	toQuery     <-chan QueryT     // pull next url to test from this channel
	unfiltered  chan<- QueryT     // send all found links back
	results     chan<- VisitedURL // add processed page to results
	quit        <-chan bool       // listen to when we should quit
	domainCheck func(u.URL) bool  // check if the url should be queried further
	wg          *sync.WaitGroup
	workerWg    *sync.WaitGroup
}

type Result struct {
	Url    string
	Status string
}

func contentCanHaveLinks(r http.Response) bool {
	return strings.Contains(r.Header["Content-Type"][0], "text/html")
}

func collectLinks(query QueryT, checkDomain func(u.URL) bool) (chan VisitedURL, chan error) {
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
		Trace.Printf("~~~~~~ trying %s\n", query.Url.String())
		res, err := client.Head(query.Url.String())
		linkedUrls := make(map[u.URL]bool)
		if err != nil {
			c <- VisitedURL{query, fmt.Sprintf("Error:%s", err), linkedUrls, true}
			Error.Printf("error occured during http.Head for %s:%s\n", query.Url.String(), err)
			return
		}
		if res.StatusCode < 200 || res.StatusCode >= 300 {
			c <- VisitedURL{query, fmt.Sprintf("Status:%d", res.StatusCode), linkedUrls, true}
			Trace.Printf("HTTP status not OK %s (%d)\n", query.Url.String(), res.StatusCode)
			return
		}
		if checkDomain(query.Url) && contentCanHaveLinks(*res) {
			Trace.Printf("~~~~~~ crawling %s\n", query.Url.String())
			linkChan := GetAllLinks(query.Url, errChan)
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

func searchPage(s Search, id int) {
	defer s.workerWg.Done()
	for {
		select {
		case query, ok := <-s.toQuery:
			if !ok {
				return
			}
			Trace.Printf("[%d]..............searching %s\n", id, query.Url.String())
			testedChan, errChan := collectLinks(query, s.domainCheck)
			select {
			case t := <-testedChan:
				for v := range t.LinkedUrls {
					s.wg.Add(1)
					s.unfiltered <- QueryT{v, query.Url.String()}
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

func CheckLinks(urlString string, parallel int, results chan VisitedURL) {
	fmt.Println("CheckLinks for " + urlString)
	url, err := u.Parse(urlString)
	if err != nil {
		log.Fatal("invalid url:")
	}
	ip, err := net.LookupIP(url.Host)
	if err != nil {
		log.Fatal("could not lookup ip of \"", url, "\"")
	}
	Info.Printf("host:%s, ip: %s\n", url.Host, ip)

	domainCheck := createDomainCheck(*url, ip)
	toQuery := make(chan QueryT, 10000) //TODO use go-routine to add to query queue
	unfiltered := make(chan QueryT, 10000)
	// results := make(chan VisitedURL)
	quit := make(chan bool)
	var wg sync.WaitGroup
	var workerWg sync.WaitGroup

	wg.Add(1)
	toQuery <- QueryT{*url, ""}

	workerWg.Add(parallel)
	s := Search{toQuery, unfiltered, results, quit, domainCheck, &wg, &workerWg}
	for i := 0; i < parallel; i++ {
		go searchPage(s, i)
	}
	// setup filtering
	go filterNonRelevant(unfiltered, toQuery, &wg)

	Info.Printf("waiting for queue...\n")
	wg.Wait()
	for i := 0; i < parallel; i++ {
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

func filterNonRelevant(in <-chan QueryT, out chan<- QueryT, wg *sync.WaitGroup) {
	var checked = make(map[string]bool)
	for v := range in {
		link := removeParameters(v.Url)
		if !checked[link] {
			checked[link] = true
			out <- v
			Trace.Printf(".........................added %s [host:%s]\n", v.Url.String(), v.Url.Host)
		} else {
			wg.Done() //already counted for, but we don't use it
		}
	}
}
