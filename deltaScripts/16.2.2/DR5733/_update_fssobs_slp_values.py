#!/usr/bin/env python2

# #5733
# Convert sea level pressure from Pa to hPa in an hdf5 file
# Do nothing if all values are already in hPa
# Author: tom.gurney@raytheon.com

import sys
import h5py

def main():
    if len(sys.argv) != 2:
        print "usage: {} filename.h5".format(sys.argv[0])
        sys.exit(1)

    didstuff = False
    try:
        with h5py.File(sys.argv[1], 'r+') as f:
            if 'seaLevelPress' in f:
                for i, data in enumerate(f['seaLevelPress']):
                    if data > 10000:
                        f['seaLevelPress'][i] = data / 100.0
                        didstuff = True
    except Exception as e:
        print "ERROR: " + str(sys.exc_info()[0]) + ": " + str(e)
        sys.exit(1)

    if didstuff:
        print "INFO: {}: updated".format(sys.argv[1])
    else:
        print "INFO: {}: no update needed".format(sys.argv[1])


if __name__ == "__main__":
    main()
