package main

import (
	"flag"
	"fmt"
	"github.com/gorilla/websocket"
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

func print_binary(s []byte) {
	fmt.Printf("Received b:")
	for n := 0; n < len(s); n++ {
		fmt.Printf("%d,", s[n])
	}
	fmt.Printf("\n")
}

func echoHandler(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println(err)
		return
	}
	for {
		messageType, p, err := conn.ReadMessage()
		if err != nil {
			return
		}
		print_binary(p)
		err = conn.WriteMessage(messageType, p)
		if err != nil {
			return
		}
	}
}
func serveTemplate(w http.ResponseWriter, r *http.Request) {
	lp := path.Join("templates", "layout.html")
	fp := path.Join("templates", r.URL.Path)

	// Return a 404 if the template doesn't exist
	info, err := os.Stat(fp)
	if err != nil {
		if os.IsNotExist(err) {
			http.NotFound(w, r)
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
		// Log the detailed error
		log.Println(err.Error())
		// Return a generic "Internal Server Error" message
		http.Error(w, http.StatusText(500), 500)
		return
	}

	if err := tmpl.ExecuteTemplate(w, "layout", r); err != nil {
		log.Println(err.Error())
		http.Error(w, http.StatusText(500), 500)
	}
}

func main() {
	fs := http.FileServer(http.Dir("static"))
	http.Handle("/static/", http.StripPrefix("/static/", fs))

	http.HandleFunc("/", serveTemplate)
	http.HandleFunc("/ws", echoHandler)

	http.ListenAndServe(":3000", nil)
}
