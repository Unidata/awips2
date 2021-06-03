#!/awips2/python/bin/python
#from ufpy import qpidingest
from sys import argv
from os import path
from awips.qpidingest import *


#read in command line argument as path
inPath = argv[1]
header = path.basename(inPath)


#make connection to QPID
#conn = qpidingest.IngestViaQPID(host='cpsbn1',port=5672,ssl=True)
conn = IngestViaQPID()

#send message to QPID
print "sending %s with a header of %s"%(inPath,header)
conn.sendmessage(inPath,header)

#close QPID connection
conn.close()

