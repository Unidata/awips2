/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.common.datastorage.locking;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * File lock manager provides a locking mechanism that is cluster-safe.
 * 
 * The first level is a local synchronization method-- a thread-safe method that
 * ensures that resources are locked within the same JVM.
 * 
 * The second level utilizes the filesystem locking mechanism, when available.
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date         Ticket#     Engineer    Description
 *     ------------ ----------  ----------- --------------------------
 *     Feb 19, 2007             chammack    Initial Creation.
 *     Jan 26, 2009             chammack    Refactored to use flock
 * 
 * </pre>
 * 
 * @author cnh
 * @version 1
 */
public class ClusteredLockManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ClusteredLockManager.class);

    protected static ClusteredLockManager instance;

    protected Map<File, LockMetadata> localSynchronizationMap;

    protected boolean shutdownInProgress;

    private static final String PLUGIN = "com.raytheon.uf.common.datastorage";

    private static final String CATEGORY = "LOCKMANAGER";

    private class LockMetadata {
        public FileLock lock;

        public RandomAccessFile raf;

        public File file;
    }

    /**
     * Private constructor
     * 
     * @throws EdexException
     */
    private ClusteredLockManager() throws LockException {
        this.localSynchronizationMap = new ConcurrentHashMap<File, LockMetadata>();
        this.shutdownInProgress = false;
        Runtime.getRuntime().addShutdownHook(new Thread() {

            /*
             * (non-Javadoc)
             * 
             * @see java.lang.Thread#run()
             */
            @Override
            public void run() {
                shutdownInProgress = true;

                boolean foundWritingProcess = false;
                long startTime = System.currentTimeMillis();
                final long MAX_TIME_TO_WAIT = 120 * 1000; // two minutes

                while (localSynchronizationMap.size() > 0
                        && (System.currentTimeMillis() - startTime < MAX_TIME_TO_WAIT)) {

                    foundWritingProcess = false;

                    for (Map.Entry<File, LockMetadata> lock : localSynchronizationMap
                            .entrySet()) {
                        if (!lock.getValue().lock.isShared()) {
                            // indicates a write lock
                            foundWritingProcess = true;
                        }
                    }

                    if (!foundWritingProcess)
                        break;

                    statusHandler.handle(Priority.SIGNIFICANT,
                                    "Shutting down: Waiting for hdf5 writers to finish...");

                    try {
                        Thread.sleep(1000L);
                    } catch (InterruptedException e) {
                        // ignore
                    }
                }
            }

        });
    }

    /**
     * Get the singleton instance of the lock manager
     * 
     * @return an instance of the lock manager
     * @throws EdexException
     *             if communication errors occurs
     */
    public static synchronized ClusteredLockManager getInstance()
            throws LockException {
        if (instance == null) {
            instance = new ClusteredLockManager();
        }

        return instance;
    }

    /**
     * Acquire a lock on a given lockname
     * 
     * @param lockName
     *            the name of the lock
     * @param writeLock
     *            if the lock is for a write, this should be true
     * @return true if the lock was granted
     * @throws LockException
     *             if locking errors occur (indicates communication problems)
     */
    public boolean getLock(File lockName, boolean writeLock)
            throws LockException, FileNotFoundException {

        if (this.shutdownInProgress == true) {
            // put thread to sleep
            try {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Shutdown in progress... hdf5 writer thread stopping");

                Thread.sleep(Long.MAX_VALUE);
            } catch (InterruptedException e) {
                // ignore
            }
        }

        synchronized (this) {
            LockMetadata lm = localSynchronizationMap.get(lockName);
            if (lm == null) {
                lm = new LockMetadata();
                // try on the file system

                if (writeLock) {
                    File dir = lockName.getParentFile();
                    if (!dir.exists()) {
                        dir.mkdirs();
                    }

                    try {
                        lm.file = lockName;
                        lm.file.createNewFile();
                    } catch (IOException e1) {
                        throw new LockException("Error creating file", e1);
                    }
                } else {
                    if (!lockName.exists()) {
                        throw new FileNotFoundException("File does not exist: "
                                + lockName.getAbsolutePath());
                    }
                }

                try {
                    String accessMethod = "r";
                    if (writeLock) {
                        accessMethod = "rw";
                    }
                    lm.raf = new RandomAccessFile(lockName, accessMethod);
                } catch (FileNotFoundException e1) {
                    if (!writeLock)
                        throw e1;
                    throw new LockException(
                            "Error setting up read/write access to lock file",
                            e1);
                }

                try {
                    lm.lock = lm.raf.getChannel().tryLock(0L, Long.MAX_VALUE,
                            !writeLock);
                } catch (OverlappingFileLockException e1) {
                    try {
                        lm.raf.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    return false;
                } catch (IOException e1) {
                    try {
                        lm.raf.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    throw new LockException("Error trying file lock", e1);
                }

                if (lm.lock == null) {
                    try {
                        lm.raf.close();
                    } catch (IOException e) {
                        System.out.println("Error closing file!");
                        e.printStackTrace();
                    }
                    return false;
                }

                localSynchronizationMap.put(lockName, lm);
                return true;
            }

            return false;

        }

    }

    /**
     * Release the specified lock
     * 
     * @param lockName
     *            the name of the lock
     * @throws LockException
     *             if a locking error occurs
     */
    public void releaseLock(File lockName) throws LockException {
        synchronized (this) {
            LockMetadata lm = localSynchronizationMap.remove(lockName);
            if (lm == null) {
                System.out.println("Warning! Lock wasn't present to release!");
                return;
            }

            try {
                lm.lock.release();
            } catch (IOException e) {
                throw new LockException("Error releasing lock", e);
            } finally {
                try {
                    lm.raf.close();
                } catch (IOException e) {
                    System.out.println("Error closing file");
                    e.printStackTrace();
                }

            }

        }

        return;

    }

}
