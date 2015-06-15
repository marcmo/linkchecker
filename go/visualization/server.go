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
	Origin   string
	Outgoing []string
}

func mkLink(orig string, dests []string) Link {
	return Link{Origin: orig, Outgoing: dests}
}

func websocketHandler(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println(err)
		return
	}
	for {
		_, p, err := conn.ReadMessage()
		if err != nil {
			return
		}
		s := string(p[:])
		fmt.Println("received this string:" + s)
		results := make(chan lib.VisitedURL)
		go func() {
			for v := range results {
				outLinks := make([]string, len(v.LinkedUrls))
				for u := range v.LinkedUrls {
					outLinks = append(outLinks, u.String())
				}
				link := mkLink(v.VQuery.Url.String(), outLinks)
				err = conn.WriteJSON(link)
				if err != nil {
					panic(err)
				}
			}
		}()
		go lib.CheckLinks(s, 20, results)
		link := mkLink("e", []string{"b", "c"})
		err = conn.WriteJSON(link)
		if err != nil {
			return
		}
	}
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
	lib.SetLogLevel(lib.INFO)
	fs := http.FileServer(http.Dir("static"))
	http.Handle("/static/", http.StripPrefix("/static/", fs))
	http.HandleFunc("/", serveTemplate)
	http.HandleFunc("/ws", websocketHandler)

	http.ListenAndServe(":3000", nil)
}
