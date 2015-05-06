#!/usr/bin/python

import sys, getopt, re
import urllib2, httplib
import logging
from lxml import html
from urlparse import urlparse

logging.basicConfig(format='%(levelname)s:%(asctime)s %(message)s', datefmt='%I:%M:%S %p', level=logging.DEBUG)
logging.addLevelName(logging.WARNING, "\033[1;31m%s\033[1;0m" % logging.getLevelName(logging.WARNING))
logging.addLevelName(logging.ERROR, "\033[1;41m%s\033[1;0m" % logging.getLevelName(logging.ERROR))

class HeadRequest(urllib2.Request):
    def get_method(self):
        return "HEAD"

def ping(url):
    try:
        response = urllib2.urlopen(HeadRequest(url), timeout=6)
        contentType = response.info()["Content-Type"]
        isText = True if re.match(r'text', contentType) else False
        return [response.getcode(), isText, response.geturl()]
    except urllib2.HTTPError, e:
        return [e.code, False, None]
    except urllib2.URLError, e:
        logging.error(url + ' resulted in URLError = ' + str(e.reason))
        return [str(e.reason), False, None]
    except httplib.HTTPException, e:
        logging.error(url + ' resulted in HTTPException')
        return [e, False, None]
    except Exception:
        logging.error(url + ' resulted in Exception')
        return [0, False, None]

def getLinks(url):
    result = set()
    page = html.fromstring(urllib2.urlopen(url).read())
    for link in page.xpath("//a"):
        href = link.get("href")
        if href and href.startswith("http"):
            result.add(href)
    return result

def checkDomain(url, domain):
    d = urlparse(url).netloc
    prefix = "www."
    d = d[len(prefix):] if d.startswith(prefix) else d
    return d.endswith(domain)

def checkUrlRecursiv(starturl, domain):
    links2check = {starturl: ''}
    checked = {}
    while len(links2check) > 0:
        (url, origin) = links2check.popitem()
        logging.info("[%d links to go] >>>>>>>>> checking: %s", len(links2check), url)
        [status, isText, realurl] = ping(url)
        checked[url] = (status, origin)
        if status >= 200 and status < 300:
            logging.info("%s is responding (origin:%s)[status %d]", url, origin, status)
            if checkDomain(url, domain) and isText:
                links = getLinks(url)
                for link in links:
                    if link not in checked.keys():
                        links2check[link] = url
        else:
            if status != 0:
                logging.error("[%s is responding (status %s)]", url, str(status))
    for key, (s, o) in checked.iteritems():
        logging.info("origin:%s --> %s (status:%s)", o, key, str(s))

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
