
import os, subprocess, re, stat, sys, time
import h5py

matchRe = re.compile('.*?(-{1,2}\\d{6,}).h5')

def findEachGroup(group, datasetList):
    if type(group) is h5py.highlevel.Group:
        for g in group.keys():
            findEachGroup(group[g], datasetList)
    elif type(group) is h5py.highlevel.Dataset:
        datasetList.append(group.name)
        
        
def processFile(filename, match):    
    startIndex = filename.find(match.groups()[0])
    endIndex = filename.find('.h5')
    reducedFilename = filename[0:startIndex] + filename[endIndex:]
    if not os.path.exists(reducedFilename):
        # this is the first one, just rename it
        try:
            os.rename(filename, reducedFilename)
        except OSError, e:
            print e            
    else:
        # open the file, find the datasets            
        datasetList = []
        hfile = None        
        try:
            hfile = h5py.File(filename, 'r')
            findEachGroup(hfile['/'], datasetList)
        finally:
            if hfile:                
                hfile.close()        
    
        fileSuccess = True
        # for each dataset in the file, run h5copy it into the output file
        for dataset in datasetList:
            if not copy(filename, dataset, reducedFilename):        
                fileSuccess = False
                
        # remove original file
        if True: #if fileSuccess:
            os.remove(filename)
    
def fileWalk(pth):
    if os.path.isdir(pth):
        innerFiles = os.listdir(pth)
        for f in innerFiles:
            fileWalk(pth + '/' + f)
    else:
        match = matchRe.match(pth)
        if match:
            processFile(pth, match)
        
def copy(filename, dataset, reducedFilename):        
    # note that this copies links as if they were real datasets, increasing the size of the output file
    cmd = ['h5copy', '-p', '-i', filename, '-o', reducedFilename, '-s', dataset, '-d', dataset]
    ret = subprocess.call(cmd)
    success = (ret == 0)
        
    if success:
        os.chmod(reducedFilename, stat.S_IWUSR | stat.S_IWGRP | stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)            
        #print "Successfully copied filename:", filename, "dataset:", dataset
        return True
    else:
        print "Failed to copy filename:", filename, "dataset:", dataset
        return False 
    
def main():
    if len(sys.argv) < 2:
        print "Please provide full path to input directory"
    else:
        inputDir = sys.argv[1]
        t0 = time.time()
        fileWalk(inputDir)
        t1 = time.time()
        print "Total copy time for directory", inputDir, (t1-t0), "seconds"
    

if __name__ == '__main__':
    main()