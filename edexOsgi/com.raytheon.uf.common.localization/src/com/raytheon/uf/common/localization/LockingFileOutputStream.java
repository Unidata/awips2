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
package com.raytheon.uf.common.localization;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import com.raytheon.uf.common.localization.FileLocker.Type;

/**
 * Locking file outputstream
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 23, 2011            mschenke    Initial creation
 * Jun 05, 2014 3248       njensen     Fix constructors so lock is obtained
 *                                      before super constructor called
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LockingFileOutputStream extends FileOutputStream {

    private File file;
    
    private final Object locker;
    
    /**
     * Create a new LockingFileOuputStream, creates an exclusive lock on the
     * file
     * 
     * @param file
     * @throws FileNotFoundException
     */
    public LockingFileOutputStream(File file) throws FileNotFoundException {
        this(file, false);
    }

    /**
     * Create a new LockingFileOuputStream, creates an exclusive lock on the
     * file
     * 
     * @param file
     * @param isAppending
     * @throws FileNotFoundException
     */
    public LockingFileOutputStream(File file, boolean isAppending)
            throws FileNotFoundException {
        this(file, isAppending, new Object());
    }
    
    /**
     * Intentionally private constructor that takes a locker object to provide a
     * unique lock tied to this stream instance. This constructor enforces that
     * the FileLocker lock will be obtained before the super constructor is
     * called. Otherwise, if isAppending is false, the super constructor will
     * set the file length to zero, wiping out the contents, and we absolutely
     * must have the write lock before that.
     * 
     * @param file
     * @param isAppending
     * @param locker
     * @throws FileNotFoundException
     */
    private LockingFileOutputStream(File file, boolean isAppending, Object locker) throws FileNotFoundException {
        this(file, isAppending, locker, FileLocker.lock(locker, file, Type.WRITE));
    }
    
    /**
     * Intentionally private constructor that should be called after the file
     * lock is obtained.
     * 
     * @param file
     * @param isAppending
     * @param locker
     * @param gotLock
     * @throws FileNotFoundException
     */
    private LockingFileOutputStream(File file, boolean isAppending, Object locker, boolean gotLock) throws FileNotFoundException {
        super(file, isAppending);
        this.file = file;
        this.locker = locker;
    }

    @Override
    public void close() throws IOException {
        close(true);
    }

    /**
     * Closes the output stream without unlocking the file. It is the
     * responsibility of the caller to call {@link #unlock()} when they are done
     * with the lock.
     */
    public void closeWithoutUnlocking() throws IOException {
        close(false);
    }

    /**
     * Closes the stream, flag designates if lock will be released or not. By
     * default {@link #close()} will unlock the file
     * 
     * @param unlock
     * @throws IOException
     */
    private void close(boolean unlock) throws IOException {
        try {
            super.close();
        } finally {
            if (unlock) {
                unlock();
            }
        }
    }

    /**
     * Unlocks the file lock associated with the stream.
     */
    public void unlock() {
        FileLocker.unlock(locker, file);
    }
}
