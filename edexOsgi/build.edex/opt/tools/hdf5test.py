#!/awips2/python/bin/python

#initially created by Everett Kladstrup

import sys
import h5py
import os
import glob
import numpy
import datetime

#this script logs keys in .h5 files that are the same value for all
#entries including across .h5 files in the same directory

#script is currently broken for some .h5 files where there are nested keys.
#pass directories containing .h5 files that should be compared
#all .h5 files in a directory are compared and if all the entries for
#a specific key are the same across all .h5 files it is logged
#can probably do try/except for file.keys(), when there are no longer any
#keys get file.value

#example usage ./hd5test.py /awips2/edex/data/hdf5/goessounding/

def arrEquality(arr1, arr2):
    rval = True
    if len(arr1) != len(arr2):
        rval=False
    else:
        for i in range(0,len(arr1)):
            if arr1[i] != arr2[i]:
                #print(str(arr1[i]) + " != " + str(arr2[i]))
                rval=False
    return rval

def walkfiles(files):
    ndarray_type = type(numpy.empty([0]))
    keys = files[0].keys()
    skipkey = False
    for key in keys:
        try:
            #make sure all values for this key are the same
            #and that they are the same across files
            prev_val = files[0][key].value[0]
            for file in files:
                for val in file[key].value:
                    #if an array use arrEquality function
                    if type(val) == ndarray_type:
                        if ( arrEquality(val, prev_val) == False ):
                            #print("array not equal; key: " + str(key))
                            #print("size " + str(len(file[key].value)))
                            skipkey = True
                    #if different data skip
                    elif val != prev_val:
                        skipkey = True
                    #stop looking through values if incompatible key found
                    if skipkey == True:
                        break
                #stop looking through files in incompatible key is found
                if skipkey == True:
                    break
            #if incompatible key dont print and reset for next key
            if skipkey == True:
                skipkey = False
            else:
                #log that this key is probably safe to remove
                logf.write("all values are the same for key: ")
                logf.write(str(key))
                logf.write("\n")
                logf.write("value seems to be \"")
                logf.write(str(files[0][key].value[0]))
                logf.write("\"\n")
        except:
            #log an error for this key
            logf.write("error processing key " + str(key) + "\n")

logf = open('log_hdf5test_all.txt', 'a')
#comment out this line to only print edges of arrays
numpy.set_printoptions(threshold=numpy.nan)

#for all arguments ( directories )
for x in range(1,len(sys.argv)):
    dir = sys.argv[x]
    logf.write("starting in dir " + dir + "\n")
    logf.write(str(datetime.datetime.now()) + "\n")
    allfiles = []
    for file in glob.glob( os.path.join(dir, "*.h5")):
        h5f = h5py.File(file, 'r')
        allfiles.append(h5f)
    walkfiles(allfiles)
logf.close()
