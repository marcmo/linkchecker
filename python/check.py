#!/usr/bin/python

import sys, getopt
import urlparse
import logging
import httputil
import Queue
from sets import Set
from threading import Thread

logging.basicConfig(format='%(levelname)s:[%(asctime)s] %(message)s', datefmt='%I:%M:%S %p', level=logging.DEBUG)
logging.addLevelName(logging.WARNING, "\033[1;31m%s\033[1;0m" % logging.getLevelName(logging.WARNING))
logging.addLevelName(logging.ERROR, "\033[1;41m%s\033[1;0m" % logging.getLevelName(logging.ERROR))

# TODO not ready for concurrent access
entered = Set()
checked = {}

def cleanLink(link):
    noParams = link.split('?')[0]
    cleaned = noParams.split('#')[0]
    if link != cleaned:
        logging.info("cleaned from %s => %s", link, cleaned)
    return cleaned

def checkUrl(queue, domain):
    while True:
        (url, origin) = queue.get()
        if url == 'stop':
            logging.info("STOPPING")
            break
        logging.info("[%d links to go] >>>>>>>>> checking: %s", queue.qsize(), url)
        [status, isText, realurl] = httputil.ping(url)
        checked[url] = (status, origin)
        if status >= 200 and status < 300:
            logging.info("%s is responding (origin:%s)[status %d]", url, origin, status)
            if httputil.checkDomain(url, domain) and isText:
                links = httputil.getLinks(url)
                for link in links:
                    cleaned = cleanLink(link)
                    absolute = urlparse.urljoin(url, cleaned)
                    if absolute not in entered:
                        logging.info("adding link to check:%s", absolute)
                        queue.put((absolute, url))
                        entered.add(absolute)
        else:
            if status != 0:
                logging.error("[%s is responding (status %s)]", url, str(status))
        queue.task_done()

def usage():
    print 'check.py -s <starturl> -p <parallel threads>'

def main(argv):
    starturl = ''
    try:
        opts, args = getopt.getopt(argv, "hs:p:", ["starturl=","threads="])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    starturl="http://httpbin.org/"
    threadCount=1
    for opt, arg in opts:
        if opt == '-h':
            usage()
            sys.exit()
        elif opt in ("-s", "--starturl"):
            starturl = arg
        elif opt in ("-p", "--parallel"):
            threadCount = int(arg)
    domain = urlparse.urlparse(starturl).netloc
    print 'starturl is ', starturl, ', domain:', domain
    q = Queue.Queue()
    q.put((starturl, ''))
    entered.add(starturl)
    workers = []
    for _ in range(threadCount):
        worker = Thread(target=checkUrl, args=(q,domain))
        workers.append(worker)
        worker.start()
    logging.info("waiting for workers...")
    q.join()
    logging.info("FINISHED!")
    for _ in range(threadCount):
        q.put(('stop',''))
    print 'JOINING all worker threads'
    for w in workers:
        w.join()
    for key, (s, o) in checked.iteritems():
        print "%s --> %s [status:%s]" % (o, key, str(s))

if __name__ == "__main__":
    main(sys.argv[1:])
    print "done with main! ========"
    sys.exit()
