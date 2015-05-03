package main

import (
	"io"
	"io/ioutil"
	"log"
	"os"
)

var (
	Trace   *log.Logger
	Info    *log.Logger
	Warning *log.Logger
	Error   *log.Logger
)

type Level int

const (
	TRACE Level = 1 << iota
	INFO  Level = 2
	WARN  Level = 4
	ERROR Level = 8
)

func SetLogLevel(level Level) {
	switch level {
	case TRACE:
		InitLogging(os.Stdout, os.Stdout, os.Stdout, os.Stderr)
	case INFO:
		InitLogging(ioutil.Discard, os.Stdout, os.Stdout, os.Stderr)
	default:
	}
}

func InitLogging(
	traceHandle io.Writer,
	infoHandle io.Writer,
	warningHandle io.Writer,
	errorHandle io.Writer) {

	Trace = log.New(traceHandle,
		"TRACE: ",
		log.Ldate|log.Ltime|log.Lshortfile)

	Info = log.New(infoHandle, "INFO: ", log.Ltime)

	Warning = log.New(warningHandle,
		"WARNING: ",
		log.Ldate|log.Ltime|log.Lshortfile)

	Error = log.New(errorHandle,
		"ERROR: ",
		log.Ldate|log.Ltime|log.Lshortfile)
}
