import sys

from warcio.archiveiterator import ArchiveIterator

with open(sys.argv[1], "rb") as stream:
    for record in ArchiveIterator(stream):
        print(record.length)
        print(record.rec_headers)
        print(record.rec_type)
