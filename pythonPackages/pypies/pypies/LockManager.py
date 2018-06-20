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
# Manages locking files for writing
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/01/10                      njensen       Initial Creation.
#    09/23/10                      njensen       Rewrote locking to use fcntl
# 
#

import fcntl, time, os, logging
from pypies import logger
from pypies import timeMap

MAX_TIME_TO_WAIT = 120 # seconds

def dirCheck(filename):
    d = os.path.dirname(filename)
    if not os.path.exists(d):
        try:        
            os.makedirs(d)
        except:
            pass # could error with dir already exists based on a race condition
    if not os.path.exists(filename):
        # for some unknown reason, can't get locks when i open it with create flag,
        # so we quickly create it and then close it so we can reopen it with RDWR
        # for the lock (according to chammack this is due to underlying linux calls)
        fd = os.open(filename, os.O_CREAT)
        os.close(fd)

def getLock(filename, mode):
    t0 = time.time()

    dirCheck(filename)    
    gotLock = False
    startTime = time.time()
    nowTime = time.time()    
    if mode == 'w' or mode == 'a':
        lockOp = fcntl.LOCK_EX
        fmode = os.O_RDWR
    else:
        lockOp = fcntl.LOCK_SH
        fmode = os.O_RDONLY
    fd = os.open(filename, fmode)
    lockOp = lockOp | fcntl.LOCK_NB
    
    if logger.isEnabledFor(logging.DEBUG):
        logger.debug(str(os.getpid()) +" Attempting to get lock on " + str(fd) + " mode " + mode + " " + filename)
    while not gotLock and (nowTime - startTime) < MAX_TIME_TO_WAIT:        
        try:
            fcntl.lockf(fd, lockOp)
            gotLock = True
        except:
            time.sleep(.1)
            nowTime = time.time()
            
    if gotLock:
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug('Got lock on ' + str(fd))
    else:
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug(str(os.getpid()) + " failed to get lock")
        os.close(fd)

    t1=time.time()
    if timeMap.has_key('getLock'):
        timeMap['getLock']+=t1-t0
    else:
        timeMap['getLock']=t1-t0

    return gotLock, fd

def releaseLock(fd):
    t0=time.time()
    fcntl.lockf(fd, fcntl.LOCK_UN)
    os.close(fd)
    if logger.isEnabledFor(logging.DEBUG):
        logger.debug('Released lock on ' + str(fd))
    t1=time.time()
    if timeMap.has_key('releaseLock'):
        timeMap['releaseLock']+=t1-t0
    else:
        timeMap['releaseLock']=t1-t0
    
    