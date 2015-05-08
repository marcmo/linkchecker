#!/usr/bin/python

import sys, getopt
import urlparse
import logging
import httputil
import Queue
from sets import Set
from threading import Thread, Lock

logging.basicConfig(format='%(levelname)s:[%(asctime)s] %(message)s', datefmt='%I:%M:%S %p', level=logging.INFO)
logging.addLevelName(logging.WARNING, "\033[1;31m%s\033[1;0m" % logging.getLevelName(logging.WARNING))
logging.addLevelName(logging.ERROR, "\033[1;41m%s\033[1;0m" % logging.getLevelName(logging.ERROR))

lock = Lock()

def cleanLink(link):
    noParams = link.split('?')[0]
    cleaned = noParams.split('#')[0]
    if link != cleaned:
        logging.info("cleaned from %s => %s", link, cleaned)
    return cleaned

def checkUrl(queue, domain, results, entered):
    while True:
        (url, origin) = queue.get()
        if url == 'stop':
            logging.info("STOPPING")
            break
        logging.info("[%d links to go] >>>>>>>>> checking: %s", queue.qsize(), url)
        [status, isText, realurl] = httputil.ping(url)
        results.put((url, status, origin))
        if status >= 200 and status < 300:
            logging.info("%s is responding (origin:%s)[status %d]", url, origin, status)
            if httputil.checkDomain(url, domain) and isText:
                links = httputil.getLinks(url)
                for link in links:
                    cleaned = cleanLink(link)
                    absolute = urlparse.urljoin(url, cleaned)
                    if safeCheckAndSet(entered, absolute):
                        queue.put((absolute, url))
        else:
            if status != 0:
                logging.error("[%s is responding (status %s)]", url, str(status))
        queue.task_done()

# enable concurrent access to the elements we saw
def safeCheckAndSet(entered, elem):
    with lock:
        if elem not in entered:
            entered.add(elem)
            return True
        return False

def usage():
    print 'check.py -s <starturl> -p <parallel threads>'

def main(argv):
    starturl="http://httpbin.org/"
    threadCount=1

    try:
        opts, args = getopt.getopt(argv, "hs:p:", ["starturl=","threads="])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
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
    results = Queue.Queue()
    q = Queue.Queue()
    q.put((starturl, ''))
    entered = Set()
    entered.add(starturl)
    workers = []
    for _ in range(threadCount):
        worker = Thread(target=checkUrl, args=(q, domain, results, entered))
        workers.append(worker)
        worker.start()
    logging.info("waiting for workers...")
    q.join()
    logging.info("FINISHED!")
    # wait until all worker threads return
    for _ in range(threadCount):
        q.put(('stop',''))
    print 'JOINING all worker threads'
    for w in workers:
        w.join()
    resCount = 1
    while not results.empty():
        (key, s, o) = results.get()
        print "%d:%s --> %s [status:%s]" % (resCount, o, key, str(s))
        resCount += 1

if __name__ == "__main__":
    main(sys.argv[1:])
    print "done with main! ========"
    sys.exit()
