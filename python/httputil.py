import logging
import requests
import re
from urlparse import urlparse
from lxml import html

def ping(url):
    try:
        response = requests.request('HEAD', url, timeout=6.0)
        contentType = response.headers['Content-Type']
        isText = True if re.match(r'text', contentType) else False
        return [response.status_code, isText, response.url]
    except requests.exceptions.HTTPError as e:
        logging.error(url + ' resulted in HTTPError')
        return [str(e), False, None]
    except requests.exceptions.Timeout:
        logging.error(url + ' resulted in timeout')
        return ['timeout', False, None]
    except requests.exceptions.TooManyRedirects:
        logging.error(url + ' too many redirects!')
        return [300, False, None]
    except requests.exceptions.ConnectionError, e:
        logging.error(url + ' ConnectionError!')
        return [str(e), False, None]
    except requests.exceptions.RequestException as e:
        logging.error(url + ' catastrophic error!! ' + str(e))
        return [str(e), False, None]

def getLinks(url):
    result = set()
    try:
        page = html.fromstring(requests.get(url).text)
        for link in page.xpath("//a"):
            href = link.get("href")
            if href and href.startswith("http"):
                result.add(href)
    except ValueError as e:
        logging.error('ValueError when trying to parse ' + url + ': ' + str(e))
    return result

def checkDomain(url, domain):
    d = urlparse(url).netloc
    prefix = "www."
    d = d[len(prefix):] if d.startswith(prefix) else d
    return d.endswith(domain)

