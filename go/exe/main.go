package main

import (
	"flag"
	"fmt"
	"github.com/marcmo/linkchecker/go/lib"
)

func main() {
	lib.SetLogLevel(lib.INFO)

	urlString := flag.String("s", "http://esrlabs.com", "the URL of the site to check links")
	parallel := flag.Int("p", 15, "number of parallel executions")
	flag.Parse()

	results := make(chan lib.VisitedURL)
	checked, errors := 0, 0
	go func() {
		for v := range results {
			checked = checked + 1
			logFn := lib.Info.Printf
			if v.HadProblems {
				logFn = lib.Warning.Printf
				errors = errors + 1
			}
			logFn("Checked[%d]: %s (%s) (origin:%s)\n", checked, v.Query.Url.String(), v.Status, v.Query.Origin)
		}
	}()

	lib.CheckLinks(*urlString, *parallel, results)
	fmt.Printf("queue done!! %d pages checked, %d had problems (%d%%)\n", checked, errors, errors*100/checked)
}
