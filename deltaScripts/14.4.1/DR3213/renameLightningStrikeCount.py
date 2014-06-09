#!/usr/bin/env python

import h5py
import os
import sys

# multiplicity was incorrectly interpreted as 'stike count' when
# it was the number of strokes (AKA pulses) in the strike (AKA flash)

LIGHTNING_H5_PATH = '/awips2/edex/data/hdf5/binlightning'
OLD_NAME = 'strikeCount'
NEW_NAME = 'pulseCount'

for file in os.listdir(LIGHTNING_H5_PATH):
    if file.endswith('h5'):
        h5file = None
        try:
            fileName = os.path.join(LIGHTNING_H5_PATH, file)
            h5file = h5py.File(fileName, 'r+')
            for g in h5file.values():
                if NEW_NAME not in g and OLD_NAME in g:
                    g[NEW_NAME] = g[OLD_NAME]
        except Exception, e:
            print "Error renaming strikeCount in file", fileName, ":", e
        finally:
            if h5file:
                h5file.close()
