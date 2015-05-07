import urllib2
import httplib
import logging
import re
from urlparse import urlparse
from lxml import html

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

