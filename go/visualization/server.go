package main

import (
	"flag"
	"fmt"
	"github.com/gorilla/websocket"
	"github.com/marcmo/linkchecker/go/lib"
	"log"
	"net/http"
	"os"
	"path"
	"strings"
	"text/template"
)

var (
	addr = flag.String("addr", ":8999", "http service address")
)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

type Link struct {
	Page        string
	Referee     string
	HadProblems bool
}

func mkLink(orig string, referee string, problem bool) Link {
	return Link{Page: orig, Referee: referee, HadProblems: problem}
}

func websocketHandler(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println(err)
		return
	}
	_, p, err := conn.ReadMessage()
	if err != nil {
		return
	}
	s := string(p[:])
	fmt.Println("received this string:" + s)
	results := make(chan lib.VisitedURL)
	go func() {
		for v := range results {
			lib.Trace.Printf("sending link %s\n", v.Query.Url.String())
			var outLinks []string
			for u := range v.LinkedUrls {
				trimmed := strings.TrimSpace(u.String())
				outLinks = append(outLinks, trimmed)
			}
			x := mkLink(v.Query.Url.String(), v.Query.Origin, v.HadProblems)
			err = conn.WriteJSON(x)
			if err != nil {
				panic(err)
			}
		}
	}()
	go lib.CheckLinks(s, 20, results)
}

func serveTemplate(w http.ResponseWriter, r *http.Request) {
	lp := path.Join("templates", "layout.html")
	fp := path.Join("templates", r.URL.Path)

	info, err := os.Stat(fp)
	if err != nil {
		if os.IsNotExist(err) { // template doesn't exist
			http.NotFound(w, r) //404
			return
		}
	}

	// Return a 404 if the request is for a directory
	if info.IsDir() {
		http.NotFound(w, r)
		return
	}

	tmpl, err := template.ParseFiles(lp, fp)
	if err != nil {
		log.Println(err.Error())
		http.Error(w, http.StatusText(500), 500)
		return
	}

	if err := tmpl.ExecuteTemplate(w, "layout", r); err != nil {
		log.Println(err.Error())
		http.Error(w, http.StatusText(500), 500)
	}
}

func main() {
	lib.SetLogLevel(lib.TRACE)
	fs := http.FileServer(http.Dir("static"))
	http.Handle("/static/", http.StripPrefix("/static/", fs))
	http.HandleFunc("/", serveTemplate)
	http.HandleFunc("/ws", websocketHandler)

	http.ListenAndServe(":3000", nil)
}
