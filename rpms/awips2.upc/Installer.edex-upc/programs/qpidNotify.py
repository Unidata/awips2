#!python
# simple wrapper to the qpidNotify Python module
from awips.qpidingest import *
import sys

def run():
    try:
        if len(sys.argv) == 3:
            file = sys.argv[1]
            header = sys.argv[2]
            conn.sendmessage(file, header)
    except: NameError

if __name__ == '__main__':
    conn = IngestViaQPID()
    run()
