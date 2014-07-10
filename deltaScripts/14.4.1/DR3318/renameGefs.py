#!/usr/bin/env python
# Convert gefs' directories  to the new GEFS' directories

import h5py
import os
import sys

PSQL='/awips2/psql/bin/psql'

GRID_DIR= os.sep + 'awips2' + os.sep + 'edex' + os.sep + 'data' + os.sep + 'hdf5' + os.sep + 'grid'
OLD_DIR= os.path.join(GRID_DIR, 'gefs')
NEW_DIR= os.path.join(GRID_DIR, 'GEFS')

OLD_VALUE=':gefs:'
NEW_VALUE=':GEFS:'

def convertH5(dir):
    for file in os.listdir(dir):
        oldFilename = os.path.join(dir, file)
        if os.path.isdir(oldFilename):
            print 'INFO Converting %s' % (oldFilename)
            convertH5(oldFilename)
        elif file.startswith('gefs') and file.endswith('h5'):
            newFile = file.replace('gefs', 'GEFS', 1)
            filename = os.path.join(os.path.split(oldFilename)[0], newFile)
            try:
                os.rename(oldFilename, filename)
            except Exception, e:
                print 'WARNING: unable to rename %s to %s %s: ' % (oldFilename, filename, e)
                continue
            h5file = None
            try:
                h5file = h5py.File(filename, 'r+')
                for g in h5file.keys():
                    if str.find(g, OLD_VALUE) > 0 :
                        new = str.replace(g, OLD_VALUE, NEW_VALUE, 1)
                        h5file[new] = h5file[g]
            except Exception, e:
                print "WARNING: in file %s: %s" % (filename, e)
            finally:
                if h5file:
                    h5file.close()
 
def moveDir(old, new):
    if not os.path.isdir(old) :
        print 'INFO: No %s directory to move.' % (old)
        return
    
    if os.path.exists(new):
        print 'ERROR: Unable to create directory %s' % (new)
        print 'Fatal: %s already exists.' % (new)
        exit(1)
    try:
        os.rename(old, new)
    except Exception, e:
        print 'ERROR: Unable to create directory %s.' % (new)
        print 'Fatal: %s' % (e)
        exit(1)

 
print 'INFO: Updates for GEFS.'

print 'INFO: updating directory'

moveDir(OLD_DIR, NEW_DIR)
 
if os.path.isdir(NEW_DIR) :
    print 'INFO: Converting h5 files'
    convertH5(NEW_DIR)
else:
    print "WARNING: %s directory not found" % (NEW_DIR)

print 'INFO: Update database'

cmd = '%s -U awips -d metadata -c "update grid_info set datasetid=%s where datasetid=%s"' % (PSQL, "'GEFS'", "'gefs'")
if os.system(cmd) :
    print 'ERROR Unable to update database'
    exit(1)
    
print 'INFO: Updated GEFS successfully.'

