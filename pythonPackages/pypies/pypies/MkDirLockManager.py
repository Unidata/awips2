##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##


#
# Manages locking files for reading and writing
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/04/10                      njensen       Initial creation
# 
#

import time, os, logging
from pypies import logger

MAX_TIME_TO_WAIT = 120 # seconds
ORPHAN_TIMEOUT = 150 # seconds
MAX_SLEEP_TIME = 0.05
MIN_SLEEP_TIME = 0.01

readLockAppend = "_read"
writeLockAppend = "_write"

def dirCheck(filename):
    d = os.path.dirname(filename)
    if not os.path.exists(d):
        try:        
            os.makedirs(d)
        except OSError, e:
            if e.errno != 17: # could error with dir already exists based on a race condition
                raise e
                
def getLock(filename, mode):
    dirCheck(filename)    
    gotLock, fpath = _getLockInternal(filename, mode)            
    if gotLock:
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Got lock on ' + str(filename))
    else:
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug(str(os.getpid()) + " failed to get lock")
    return gotLock, fpath


def _getLockInternal(filename, mode):
    gotLock = False
    lockPath = None
    startTime = time.time()
    if logger.isEnabledFor(logging.DEBUG):
        logger.debug(str(os.getpid()) +" Attempting to get lock on mode " + mode + " " + filename)
    
    readLockDir = filename + readLockAppend
    writeLockDir = filename + writeLockAppend
    
    count = 0        
    
    timeElapsed = time.time() - startTime
    while not gotLock and timeElapsed < MAX_TIME_TO_WAIT: 
        count += 1
        if count % 100 == 0:
            # check for orphans every 100 tries
            _checkForOrphans(filename)
               
        if mode == 'r':
            if os.path.exists(writeLockDir):
                time.sleep(_getSleepTime(timeElapsed))                
                timeElapsed = time.time() - startTime
            else:
                try:                    
                    os.mkdir(readLockDir)                    
                except OSError, e:
                    # error 17 is dir already exists, which we can ignore
                    if e.errno != 17:
                        raise e
                    
                try:
                    # Need to check to make sure write lock wasn't created 
                    # between last check and our dir creation
                    if os.path.exists(writeLockDir):
                        os.rmdir(readLockDir)
                        continue
                except OSError, e:
                    continue #Ignore error
                
                try:
                    f = open(readLockDir + '/' + str(os.getpid()) + '.pid', 'w')
                    gotLock = True
                    lockPath = f.name
                except IOError, e:
                    if e.errno != 2:
                        # 2 indicates directory is gone, could have had another
                        # process remove the read dir before the file was created
                        raise e                
        else: # mode is 'w' or 'a'
            if os.path.exists(writeLockDir):
                time.sleep(_getSleepTime(timeElapsed))
                timeElapsed = time.time() - startTime
            else:
                # make the write lock to signal reads to start queuing up
                try:
                    os.mkdir(writeLockDir)
                except OSError, e:
                    if e.errno == 17:
                        continue # different process grabbed the write lock before this one
                    else:
                        raise e
                    
                # reset start time since we got the write lock. now just need to wait
                # for any ongoing reads to finish
                startTime = time.time()
                timeElapsed = time.time() - startTime
                while os.path.exists(readLockDir) and timeElapsed < MAX_TIME_TO_WAIT:
                    count += 1
                    if count % 100 == 0:
                        _checkForOrphans(filename)                
                    time.sleep(_getSleepTime(timeElapsed))
                    timeElapsed = time.time() - startTime
                if not os.path.exists(readLockDir):
                    gotLock = True
                    lockPath = writeLockDir
                elif timeElapsed >= MAX_TIME_TO_WAIT:
                    # the read lock never got released, so we don't have the write lock
                    # release the write lock to try and lessen impact
                    releaseLock(writeLockDir)
                    raise RuntimeError("Unable to get write lock, read locks not releasing: " + readLockDir)
                else :
                    # read lock was created after we checked in while loop but 
                    # read process checked write dir before we created it, release and continue
                    releaseLock(writeLockDir)
    return gotLock, lockPath

def _getSleepTime(timeWaiting):
    sleepTime = MAX_SLEEP_TIME
    if timeWaiting > 0.0:
        y = 1.0 / timeWaiting
        sleepTime = y / 100.0
        if sleepTime < MIN_SLEEP_TIME:
            sleepTime = MIN_SLEEP_TIME
        elif sleepTime > MAX_SLEEP_TIME:
            sleepTime = MAX_SLEEP_TIME
    return sleepTime


def releaseLock(lockPath):
    if lockPath.endswith('.pid'):
        # it was a read
        os.remove(lockPath)
        dirpath = lockPath[0:lockPath.rfind('/')]
        
        try:
            if len(os.listdir(dirpath)) == 0:
                os.rmdir(dirpath)
        except OSError, e:
            if e.errno != 2 and e.errno != 39:
                # error 2 is no directory exists, implying a different read release
                # raced and removed it first
                # error 39 is directory is not empty, implying a different read
                # added a pid file after we checked the dir's number of files            
                logger.warn('Unable to remove read lock ' + dirpath + ': ' + str(e))
    else:
        # it was a write
        os.rmdir(lockPath)
            
    if logger.isEnabledFor(logging.DEBUG):
        logger.debug('Released lock on ' + str(lockPath))

def _checkForOrphans(filename):
    if logger.isEnabledFor(logging.DEBUG):
        logger.debug('Checking for orphan locks on ' + filename)
    
    readLockDir = filename + readLockAppend
    writeLockDir = filename + writeLockAppend
    
    orphanRemoved = False
    
    nowTime = time.time()
    # check for read lock orphans
    if os.path.exists(readLockDir):
        for f in os.listdir(readLockDir):
            fullpath = readLockDir + '/' + f
            try:
                statinfo = os.stat(fullpath)
                if nowTime - ORPHAN_TIMEOUT > statinfo.st_mtime:
                    logger.warn("Orphan lock " + fullpath + " found and will be removed.")                
                    os.remove(fullpath)
                    orphanRemoved = True
            except OSError, e:
                if e.errno != 2:
                    # 2 indicates file was removed by other process looking for orphans
                    logger.error("Error removing orphaned lock: " + str(e))
        try:
            if len(os.listdir(readLockDir)) == 0:
                logger.warn("Orphan lock " + readLockDir + " found and will be removed.")            
                os.rmdir(readLockDir)
        except OSError, e:
            if e.errno != 2 and e.errno != 39:
                # error 2 is no directory exists, implying a different read release
                # raced and removed it first
                # error 39 is directory is not empty, implying a different read
                # added a pid file after we checked the dir's number of files            
                logger.error('Unable to remove orphaned read lock ' + readLockDir + ': ' + str(e))
    
    # check for write lock orphans
    if os.path.exists(writeLockDir):
        try:
            statinfo = os.stat(writeLockDir)
            if nowTime - ORPHAN_TIMEOUT > statinfo.st_mtime:
                logger.warn("Orphan lock " + writeLockDir + " found and will be removed.")
                os.rmdir(writeLockDir)
                orphanRemoved = True
        except OSError, e:
            # 2 indicates no such directory, assuming another process removed it
            if e.errno != 2:
                logger.error('Unable to remove orphaned lock: ' + str(e))
    
    return orphanRemoved
    