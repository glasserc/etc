import os.path
import urllib
from urlparse import urlsplit, urljoin
import re
import time
import BeautifulSoup
from BeautifulSoup import BeautifulStoneSoup
from BeautifulSoup import BeautifulSoup

START_URL = 'http://grayscale.scene.pl/en_index.php'
DEST_DIR = 'dest/'
MATCH_FILES = 'msx/.+\\.mp3$'
MATCH_SPIDER = 'startline=\d+$'

WHITE = None
GRAY = False
BLACK = True
visited = {}
urls = [START_URL]
files = {}
while urls:
    url = urls.pop(0)
    visited[url] = BLACK
    print 'fetching', url
    stream = urllib.urlopen(url)

    soup = BeautifulSoup(stream.read())

    for link in soup.findAll('a', href=re.compile(MATCH_FILES)):
        new_url = urljoin(url, link['href'])
        if new_url not in files:
            print 'found file', new_url
            files[new_url] = True
    if MATCH_SPIDER:
        links = soup.findAll('a', href=re.compile(MATCH_SPIDER))
        for link in links:
            new_url = urljoin(url, link['href'])
            if visited.get(new_url, WHITE) == WHITE:
                print 'found link', new_url
                visited[new_url] = GRAY
                urls.append(new_url)

    time.sleep(2)

for url in files:
    print 'retreiving', url
    local_name = os.path.join(DEST_DIR, url[url.rindex('/')+1:])
    urllib.urlretrieve(url, local_name)
    time.sleep(2)
