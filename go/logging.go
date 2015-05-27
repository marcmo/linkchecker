package main

import (
	"github.com/fatih/color"
	"io"
	"io/ioutil"
	"log"
	"os"
	// "strings"
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
	case WARN:
		InitLogging(ioutil.Discard, ioutil.Discard, os.Stdout, os.Stderr)
	case ERROR:
		InitLogging(ioutil.Discard, ioutil.Discard, ioutil.Discard, os.Stderr)
	default:
	}
}

type ColoredWriter struct {
	writer io.Writer
	color  color.Color
}

func (cw ColoredWriter) Write(p []byte) (n int, err error) {
	s := string(p[:])
	return cw.writer.Write([]byte(cw.color.SprintfFunc()(s)))
}

func InitLogging(
	traceHandle io.Writer,
	infoHandle io.Writer,
	warningHandle io.Writer,
	errorHandle io.Writer) {

	infoWriter := ColoredWriter{infoHandle, *color.New(color.FgGreen)}
	warnWriter := ColoredWriter{warningHandle, *color.New(color.FgYellow)}
	errorWriter := ColoredWriter{errorHandle, *color.New(color.FgRed)}

	Trace = log.New(traceHandle, "TRACE: ", log.Ldate|log.Ltime|log.Lshortfile)
	Info = log.New(infoWriter, "INFO: ", log.Ltime)
	Warning = log.New(warnWriter, "WARNING: ", log.Ltime)
	Error = log.New(errorWriter, "ERROR: ", log.Ldate|log.Ltime|log.Lshortfile)
}
