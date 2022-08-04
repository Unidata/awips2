#!/awips2/python/bin/python

import re
import sys

# we expect the filename, model name, and forecast hour as arguments
hdf5file = sys.argv[1]
model = sys.argv[2]
forecastHour = sys.argv[3]

matches = re.search(r'modelsounding-([0-9]+-[0-9]+-[0-9]+-[0-9]+).h5', hdf5file, re.M|re.I)
if matches:
   # extract the reftime
   reftime = matches.group(1)

   newFileName = "modelsounding-" + model + "-" + reftime + "-FH-" + str(forecastHour) + ".h5"
   print newFileName
else:
   print "ERROR: unrecognized file - " + hdf5file + "!"
   sys.exit(-1)
