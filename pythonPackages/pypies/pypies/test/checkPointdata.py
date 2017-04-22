
import numpy, h5py
import os, sys, time
import string, traceback


resultMap = {}

def analyzeFile(filename):
    f = None    
    try:
        f = h5py.File(filename, 'r')
        for dsName in f.keys():
            ds = f[dsName]        
            dtype = ds.dtype.type
            shape = ds.shape
            if dtype == numpy.object_ or dtype == numpy.string_:
                # strings
                value = ds.value
                longestString = 0                    
                if len(shape) == 1:
                    for x in range(shape[0]):
                        size = len(value[x])
                        if size > longestString:
                            longestString = size    
                    if dtype == numpy.string_:
                        addResults(dtype, dsName, longestString, numpy.dtype(ds.dtype).itemsize)
                    else:
                        addResults(dtype, dsName, longestString)           
                if len(shape) == 2:
                    maxSecondDim = 0
                    for x in range(shape[0]):
                        for y in range(shape[1]):
                            size = len(value[x][y])
                            if size > longestString:
                                longestString = size
                            if size and y > maxSecondDim:
                                maxSecondDim = y                    
                    if dtype == numpy.string_:
                        addResults(dtype, dsName, longestString, numpy.dtype(ds.dtype).itemsize, maxSecondDim, shape[1])
                    else:
                        addResults(dtype, dsName, longestString, None, maxSecondDim, shape[1])
            elif len(shape) == 2:
                value = ds.value
                # non-string multidimensional data
                maxSecondDim = 0
                foundRealValue = 0
                for x in range(shape[0]):
                    for y in range(shape[1]):
                        val = value[x][y]
                        if val != -9999:
                            foundRealValue = 1
                            if y > maxSecondDim:                            
                                maxSecondDim = y
                addResults(dtype, dsName, foundRealValue, None, maxSecondDim, shape[1])
    finally:
        if f:
            f.close()

def addResults(dtype, dsName, longestFound, maxLength=None, usedSecondDim=None, nRows=None):
    if resultMap.has_key(dsName):
        entry = resultMap[dsName]
        if longestFound > entry['longestFound']:
            entry['longestFound'] = longestFound
        if usedSecondDim and usedSecondDim > entry['usedSecondDim']:
            entry['usedSecondDim'] = usedSecondDim
        resultMap[dsName] = entry
    else:
        resultMap[dsName] = {'type': dtype, 'longestFound':longestFound, 'maxLength':maxLength, 'usedSecondDim':usedSecondDim, 'nRows':nRows}

def checkFiles(directory):
    files = os.listdir(directory)
    files.sort()
    files.reverse()
    for f in files:
        print "Checking ", f
        if os.path.isdir(f):
            checkFiles(os.path.join(directory, f))
        else:
            try:
                analyzeFile(os.path.join(directory, f))
            except Exception, e:
                t, v, tb = sys.exc_info()    
                print "Error encountered, skipping file " + f + " in analysis:\n", string.join(traceback.format_exception(t, v, tb))

def processResults(directory):
    t = time.strftime('%Y%m%d')
    match = '/awips2/edex/data/hdf5/'
    index = directory.find(match)
    if index == 0:
        datatype = directory[len(match):]   
        filename = '/tmp/' + datatype + 'PointdataSizeAnalysis-' + t + '.txt'
    else:
        filename = '/tmp/pointdataSizeAnalysis-' + t + '.txt'        
    outFile = open(filename, 'w')
    outFile.write("Run against " + directory + ' on ' + t + '\n')    
    
    for key in resultMap:
        msg = "Dataset " + key + '\n'
        entry = resultMap[key]
        if entry['type'] == numpy.string_:
            msg += "   fixed length string most used was " + str(entry['longestFound']) + "/" + str(entry['maxLength']) + ' characters\n' 
        elif entry['type'] == numpy.object_:
            msg += "   vlen string most used was " + str(entry['longestFound']) + '\n'
        if entry['usedSecondDim']:
            # add 1 because it's indexed by 0
            msg += '   ' + str(entry['type']) + ' two dimensional data at most used ' + str(entry['usedSecondDim'] + 1) + \
                        '/' + str(entry['nRows']) + ' possible rows\n'
        msg += '\n'        
        outFile.write(msg)
    outFile.close()
        

def main():    
    dir = sys.argv[1]
    checkFiles(dir)
    processResults(dir)    
    
      

if __name__ == '__main__':
    main()