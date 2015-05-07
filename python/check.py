#!/usr/bin/python

import sys, getopt
from urlparse import urlparse
import logging
import httputil

logging.basicConfig(format='%(levelname)s:%(asctime)s %(message)s', datefmt='%I:%M:%S %p', level=logging.DEBUG)
logging.addLevelName(logging.WARNING, "\033[1;31m%s\033[1;0m" % logging.getLevelName(logging.WARNING))
logging.addLevelName(logging.ERROR, "\033[1;41m%s\033[1;0m" % logging.getLevelName(logging.ERROR))

def checkUrlRecursiv(starturl, domain):
    links2check = {starturl: ''}
    checked = {}
    while len(links2check) > 0:
        (url, origin) = links2check.popitem()
        logging.info("[%d links to go] >>>>>>>>> checking: %s", len(links2check), url)
        [status, isText, realurl] = httputil.ping(url)
        checked[url] = (status, origin)
        if status >= 200 and status < 300:
            logging.info("%s is responding (origin:%s)[status %d]", url, origin, status)
            if httputil.checkDomain(url, domain) and isText:
                links = httputil.getLinks(url)
                for link in links:
                    if link not in checked.keys():
                        links2check[link] = url
        else:
            if status != 0:
                logging.error("[%s is responding (status %s)]", url, str(status))
    for key, (s, o) in checked.iteritems():
        print("origin:%s --> %s (status:%s)", o, key, str(s))

def usage():
    print 'check.py -s <starturl>'

def main(argv):
    starturl = ''

    try:
        opts, args = getopt.getopt(argv, "hs:", ["starturl="])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            usage()
            sys.exit()
        elif opt in ("-s", "--starturl"):
            starturl = arg
            domain = urlparse(starturl).netloc
            print 'starturl is ', starturl, ', domain:', domain
            checkUrlRecursiv(starturl, domain)

if __name__ == "__main__":
    main(sys.argv[1:])
