1. When the queried url have trailing characters after the search string (search string is what's denoted with {} in config.py), the query can potentially match against more than one single engine declared in config.py.
2. When the queried url matches more than one search engines, qutesearch will use the first one on the list.

For this reason, in config.py it's important to: 

USE THIS:
c.url.searchengines["gl"] =  "https://www.google.com/search?q={}"
c.url.searchengines["gli"] = "https://www.google.com/search?tbm=isch&q={}"

INSTEAD OF THIS:
c.url.searchengines["gl"] =  "https://www.google.com/search?q={}"
c.url.searchengines["gli"] = "https://www.google.com/search?q={}&tbm=isch"

Note: inversing the order of appearance of these two string could make quteserach to properly disambiguate the engines?

TODO: Properly manage ambiguous occurrences programatically.
