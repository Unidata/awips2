#!/awips2/python/bin/python

import re
import sys

# we expect the filename as a command-line argument.
hdf5file = sys.argv[1]

matches = re.search(r'modelsounding-([0-9]+-[0-9]+-[0-9]+)-([0-9]+).h5', hdf5file, re.M|re.I)
if matches:
   # extract the date
   # extract the hour
   date = matches.group(1)
   hour = matches.group(2)

   reftimeDirName = date + "_" + hour + ":00:00.0"
   print reftimeDirName
else:
   print "ERROR: unrecognized file - " + hdf5file + "!"
   sys.exit(-1)
