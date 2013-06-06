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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Class to be used to lock a File for reading or writing. The class works so
 * that Files are locked on the executing Thread. It is very important to make
 * sure that when locking files, the locker does not execute operations that
 * would lock the same file in a different thread after locking the file or
 * deadlock could occur
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 23, 2011            mschenke    Initial creation
 * Apr 12, 2013 1903       rjpeter     Fix allocateLock freezing out other lock requests.
 * May 30, 2013 2056       rjpeter     Allow ACQUIRING state to be released.
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class FileLocker {

    private static class LockWaiter {
    }

    private static enum LockState {
        ACQUIRING, IN_USE, RELEASED
    }

    private static class LockedFile {
        final Type lockType;

        final Thread lockingThread;

        final List<Object> lockers = new ArrayList<Object>();

        long lockTime = System.currentTimeMillis();

        File lockFile;

        LockState lockState = LockState.ACQUIRING;

        LockedFile(Thread thread, Type type, Object locker) {
            lockType = type;
            lockingThread = thread;
            lockers.add(locker);
        }
    }

    public static enum Type {
        READ, WRITE
    }

    private static final int MAX_WAIT = 30 * 1000;

    /** Map of waiters on threads */
    private final Map<File, Deque<LockWaiter>> waiters = new HashMap<File, Deque<LockWaiter>>();

    /** Map of locks we have on files */
    private final Map<File, LockedFile> locks = new HashMap<File, LockedFile>();

    /** Singleton instance of FileLocker class */
    private static FileLocker instance = new FileLocker();

    private FileLocker() {

    }

    /**
     * Attempt to lock the LocalizationFile for reading or writing.
     * 
     * @param locker
     *            Object acquiring the lock
     * @param file
     *            The LocalizationFile to lock
     * @param type
     *            The lock type READ or WRITE
     * @return true if locked file, false otherwise
     */
    public static boolean lock(Object locker, LocalizationFile file, Type type) {
        if (file != null) {
            try {
                return instance.lockInternal(locker,
                        file.getFile(type == Type.READ), type, true);
            } catch (LocalizationException e) {
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        "Error locking LocaliationFile: " + file, e);
            }
        }
        return false;
    }

    /**
     * Unlock the LocalizationFiles locked by the object
     * 
     * @param locker
     *            The object that was used to lock the file
     * @param files
     *            files to unlock
     */
    public static void unlock(Object locker, LocalizationFile... files) {
        for (LocalizationFile file : files) {
            if (file != null) {
                try {
                    instance.unlockInternal(locker, file.getFile(false));
                } catch (Throwable t) {
                    UFStatus.getHandler().handle(Priority.PROBLEM,
                            "Error unlocking LocaliationFile: " + file, t);
                }
            }
        }
    }

    /**
     * Attempt to lock the File for reading or writing.
     * 
     * @param locker
     *            Object acquiring the lock
     * @param file
     *            The File to lock
     * @param type
     *            The lock type READ or WRITE, if performing both, choose WRITE
     * @return true if locked file, false otherwise
     */
    public static boolean lock(Object locker, File file, Type type) {
        return instance.lockInternal(locker, file, type, true);
    }

    /**
     * Unlock the Files locked by the object
     * 
     * @param locker
     *            The object that was used to lock the file
     * @param files
     *            files to unlock
     */
    public static void unlock(Object locker, File... files) {
        for (File file : files) {
            instance.unlockInternal(locker, file);
        }
    }

    private boolean lockInternal(Object locker, File file, Type type,
            boolean block) {
        if (file.isDirectory()) {
            return false;
        }
        // Get executing thread
        Thread myThread = Thread.currentThread();
        LockedFile lock = null;
        // synchronize on the locks map while operating on map

        boolean grabbedLock = false;
        synchronized (locks) {
            lock = locks.get(file);
            if (lock == null) {
                lock = new LockedFile(myThread, type, locker);
                locks.put(file, lock);
                grabbedLock = true;
            }
        }

        if (grabbedLock) {
            // We were able to grab the lock file ourselves
            return allocateLock(file, lock);
        } else {
            synchronized (lock) {
                // if the lock file has been obtained and either the thread is
                // the same as the one that obtained the lock or the original
                // lock and this request are both read locks
                if ((lock.lockState == LockState.IN_USE)
                        && ((lock.lockingThread == myThread) || ((type == Type.READ) && (type == lock.lockType)))) {
                    // TODO: This is not safe as another thread could have a
                    // read lock and we may clobber the read
                    lock.lockers.add(locker);
                    lock.lockTime = System.currentTimeMillis();
                    return true;
                }
            }
        }

        // Should not reach this point when block = false
        if (block) {
            // Wait for lock to be released
            LockWaiter waiter = new LockWaiter();
            Deque<LockWaiter> lws = null;
            // become a waiter on the file, ensures when file is unlocked by
            // other Thread, they don't delete the lock file
            synchronized (waiters) {
                lws = waiters.get(file);
                if (lws == null) {
                    lws = new ArrayDeque<LockWaiter>();
                    waiters.put(file, lws);
                }
                lws.add(waiter);
            }

            boolean waiterRemoved = false;

            try {
                while (true) {
                    // Sleep
                    try {
                        Thread.sleep(10);
                    } catch (InterruptedException e) {
                        // Ignore
                    }

                    grabbedLock = false;
                    synchronized (locks) {
                        lock = locks.get(file);
                        if (lock == null) {
                            // File ready for grabbing
                            synchronized (lws) {
                                if (lws.peek() == waiter) {
                                    lws.poll();
                                    waiterRemoved = true;
                                    lock = new LockedFile(myThread, type,
                                            locker);
                                    locks.put(file, lock);
                                    grabbedLock = true;
                                }
                            }
                        }
                    }

                    if (grabbedLock) {
                        // We were able to grab the lock file ourselves
                        return allocateLock(file, lock);
                    } else if (lock != null) {
                        synchronized (lock) {
                            if ((type == Type.READ) && (type == lock.lockType)
                                    && LockState.IN_USE.equals(lock.lockState)) {
                                // A different waiter grabbed it for
                                // reading, we can read it also
                                lock.lockers.add(locker);
                                lock.lockTime = System.currentTimeMillis();
                                return true;
                            } else {
                                long curTime = System.currentTimeMillis();
                                if ((curTime - lock.lockTime) > MAX_WAIT) {
                                    System.err
                                            .println("Releasing lock: "
                                                    + "Lock has been allocated for  "
                                                    + ((curTime - lock.lockTime) / 1000)
                                                    + "s on file "
                                                    + file.getPath());
                                    locks.remove(file);
                                }
                            }
                        }
                    }
                }
            } finally {
                if (!waiterRemoved) {
                    synchronized (lws) {
                        lws.remove(waiter);
                    }
                }
            }
        }

        return false;
    }

    private void unlockInternal(Object locker, File file) {
        try {
            boolean fileUnlocked = false;
            LockedFile lock = null;

            // Get the Lock
            synchronized (locks) {
                lock = locks.get(file);
                // Return early if we have never locked
                if ((lock == null) || (locker == null)) {
                    return;
                }
            }

            synchronized (lock) {
                if ((lock.lockState == LockState.IN_USE)
                        || lock.lockingThread.equals(Thread.currentThread())) {
                    lock.lockers.remove(locker);

                    if (lock.lockers.isEmpty()) {
                        // No more lockers, remove lock and release
                        lock.lockState = LockState.RELEASED;
                        unallocateLock(lock);
                        fileUnlocked = true;
                    }
                }
            }

            if (fileUnlocked) {
                synchronized (locks) {
                    locks.remove(file);
                }

                // Check for waiters on the file
                synchronized (waiters) {
                    Deque<LockWaiter> lws = waiters.get(file);
                    if (lws == null) {
                        waiters.remove(file);
                    } else {
                        synchronized (lws) {
                            if (lws.isEmpty()) {
                                waiters.remove(file);
                            }
                        }
                    }
                }
            }
        } catch (Throwable t) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    "Error unlocking file: " + file, t);
        }
    }

    /**
     * Function to actually allocate the lock, this needs to be cross process
     * aware and wait until other processes are finished with the file
     * 
     * @param file
     * @param lock
     * @return
     * @throws FileNotFoundException
     * @throws IOException
     */
    private boolean allocateLock(File file, LockedFile lock) {
        // Get the lock directory, make sure it is not already taken
        File parentDir = file.getParentFile();

        if (!parentDir.exists()) {
            parentDir.mkdirs();
        }

        // If we can't write to the parent directory of the file we are
        // locking, can't do any locking
        if (parentDir.canWrite() == false) {
            UFStatus.getHandler()
                    .handle(Priority.PROBLEM,
                            "Cannot write to directory: "
                                    + parentDir.getAbsolutePath());
            return false;
        }

        boolean gotLock = false;
        File lockFile = new File(parentDir, "." + file.getName() + "_LOCK");

        try {
            // start with a moderate wait
            long waitInterval = 100;
            long curTime = System.currentTimeMillis();
            long maxWaitTime = curTime + MAX_WAIT;
            long fileTime;
            while ((curTime = System.currentTimeMillis()) < maxWaitTime) {
                gotLock = lockFile.createNewFile()
                        || (((fileTime = lockFile.lastModified()) > 0) && ((curTime - fileTime) > MAX_WAIT));

                if (gotLock) {
                    break;
                } else {
                    try {
                        Thread.sleep(waitInterval);
                    } catch (InterruptedException e) {
                        // Ignore
                    }

                    // every wait reduces the next wait down to minimum wait of
                    // 10 milliseconds
                    if (waitInterval > 10) {
                        waitInterval -= 10;
                    }
                }
            }
        } catch (IOException e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    "Error obtaining file lock: " + file, e);
        } finally {
            synchronized (lock) {
                long millis = System.currentTimeMillis();
                lock.lockFile = lockFile;
                lock.lockTime = millis;
                lock.lockFile.setLastModified(millis);
                lock.lockState = LockState.IN_USE;
            }
        }

        if (!gotLock) {
            System.err.println("Failed to obtain lock for: " + file
                    + ", returning anyway");
            Thread.dumpStack();
        }

        return gotLock;
    }

    private void unallocateLock(LockedFile lock) throws IOException {
        if (lock.lockFile != null) {
            lock.lockFile.delete();
            lock.lockFile = null;
        }
    }

    /**
     * Test program for file locking
     * 
     * @param args
     */
    public static void main(String[] args) {
        long t0 = System.currentTimeMillis();
        // Parse the command line args
        Map<String, String> argumentMap = new HashMap<String, String>();
        for (int i = 0; i < args.length; ++i) {
            String arg = args[i];
            if (arg.startsWith("-")) {
                // we have a key
                if ((args.length > (i + 1))
                        && (args[i + 1].startsWith("-") == false)) {
                    argumentMap.put(arg.substring(1), args[i + 1]);
                    ++i;
                } else {
                    argumentMap.put(arg.substring(1), "");
                }
            }
        }

        if (argumentMap.get("help") != null) {

            return;
        }
        String fileToLock = argumentMap.get("f");
        String threads = argumentMap.get("tc");
        final boolean verbose = argumentMap.get("v") != null;
        final boolean verboseCount = (argumentMap.get("vc") != null) || verbose;
        final boolean verboseErrors = (argumentMap.get("ve") != null)
                || verbose;
        String outputAtCount = argumentMap.get("c");
        String iterVal = argumentMap.get("i");

        if ((fileToLock == null) || (argumentMap.get("help") != null)) {
            System.out.println("Required argument -f <pathToFile>"
                    + " specifies the file to use in the program");
            System.out.println("-help prints out this help message");
            System.out.println("-tc <threadCount> specifies the number "
                    + "of threads to create that will be reading/"
                    + "writing from the file, default value is 1");
            System.out.println("-i <iterations> specifies the number of "
                    + "iterations the program will execute, default"
                    + " value will run indefinitely");
            System.out.println("-v will turn on all verbose output");
            System.out.println("-c <count> specifies after count "
                    + "iterations the program will output"
                    + " results when verbose flag is on, default is 100");
            System.out.println("-vc will turn on verbose output "
                    + "for showing results after count iterations");
            System.out.println("-ve will turn on verbose output for "
                    + "when errors occur");
            return;
        }

        int threadCount = 1;
        if (threads != null) {
            try {
                threadCount = Integer.parseInt(threads);
            } catch (Exception e) {
                System.err.println("Error parsing -threads (" + threads
                        + ") defaulting to 1");
            }
        }

        int outputCountAt = 100;
        if (outputAtCount != null) {
            try {
                outputCountAt = Integer.parseInt(outputAtCount);
            } catch (Exception e) {
                System.err.println("Error parsing output count value -c ("
                        + outputAtCount + ") defaulting to 100");
            }
        }

        Integer iterations = null;
        if (iterVal != null) {
            try {
                iterations = Integer.parseInt(iterVal);
            } catch (Exception e) {
                System.err
                        .println("Error parsing number of iterations flag -i ("
                                + iterVal + ") defaulting to indefinite");
            }
        }

        final int[] errors = new int[1];
        final File toLock = new File(fileToLock);
        List<Runnable> runnables = new ArrayList<Runnable>(threadCount);
        final Object lockObj = new Object();

        final int[] i = { 0 };
        for (int j = 1; j <= threadCount; ++j) {
            // Get arguments
            String threadArgs = argumentMap.get("" + j);
            if (threadArgs == null) {
                if (verbose) {
                    System.out
                            .println("No thread arguments specified for thread "
                                    + j
                                    + ", defaulting to '-"
                                    + j
                                    + " rw' (read/write)");
                    System.out.flush();
                }
                threadArgs = "rw";
            } else if ((threadArgs.contains("r") == false)
                    && (threadArgs.contains("w") == false)) {
                System.err.println("Error parsing thread " + j + " arguments ("
                        + threadArgs + "), defaulting to rw");
                System.err.flush();
                threadArgs = "rw";
            }

            final int threadId = j;
            final boolean read = threadArgs.contains("r");
            final boolean write = threadArgs.contains("w");

            if (verbose) {
                System.out.println("Thread " + j + " read=" + read + " write="
                        + write);
                System.out.flush();
            }

            runnables.add(new Runnable() {
                @Override
                public void run() {
                    try {
                        if (FileLocker.lock(lockObj, toLock, write ? Type.WRITE
                                : Type.READ)) {
                            boolean canRead = true;
                            if (toLock.exists() == false) {
                                if (verbose) {
                                    System.out.println(threadId
                                            + ": Lock file does not exist");
                                    System.out.flush();
                                }
                                canRead = false;
                            } else if (toLock.length() == 0) {
                                if (verboseErrors) {
                                    System.err
                                            .println(i[0]
                                                    + " Error: Lock file has incorrect file length");
                                    System.err.flush();
                                }
                                errors[0] += 1;
                            }

                            if (read && canRead) {
                                FileInputStream fin = null;
                                try {
                                    byte[] bytes = new byte[10];
                                    fin = new FileInputStream(toLock);
                                    fin.read(bytes);
                                    if (verbose) {
                                        System.out.println(threadId
                                                + " read from lock file: "
                                                + new String(bytes));
                                        System.out.flush();
                                    }
                                } catch (Throwable t) {
                                    if (verboseErrors) {
                                        t.printStackTrace();
                                    }
                                    errors[0] += 1;
                                } finally {
                                    if (fin != null) {
                                        fin.close();
                                    }
                                }
                            }

                            if (write) {
                                toLock.delete();
                                FileOutputStream fou = null;
                                try {
                                    byte[] bytes = ("" + threadId).getBytes();
                                    fou = new FileOutputStream(toLock);
                                    fou.write(bytes);
                                    if (verbose) {
                                        System.out.println(threadId
                                                + " wrote to lock file: "
                                                + new String(bytes));
                                        System.out.flush();
                                    }
                                } catch (Throwable t) {
                                    if (verboseErrors) {
                                        t.printStackTrace();
                                    }
                                    errors[0] += 1;
                                } finally {
                                    if (fou != null) {
                                        fou.close();
                                    }
                                }
                            }
                        } else {
                            if (verboseErrors) {
                                System.err
                                        .println(threadId
                                                + ": Did not accquire lock as expected!");
                                System.err.flush();
                            }
                        }
                    } catch (Throwable t) {
                        if (verboseErrors) {
                            errors[0] += 1;
                            t.printStackTrace();
                        }
                    } finally {
                        FileLocker.unlock(lockObj, toLock);
                    }
                }
            });
        }

        int totalErrors = 0;
        int count = 0;
        while ((iterations == null) || (i[0] < iterations)) {
            List<Thread> threadList = new ArrayList<Thread>(threadCount);
            for (Runnable r : runnables) {
                threadList.add(new Thread(r));
            }

            // Start the threads
            for (Thread t : threadList) {
                t.start();
            }

            // Wait for them to complete
            for (Thread t : threadList) {
                try {
                    t.join();
                } catch (InterruptedException e) {
                    // Ignore
                }
            }

            i[0] += 1;
            ++count;

            // Output at -c val if we are verbose or indefinitely looping
            if ((count == outputCountAt)
                    && (verboseCount || (iterations == null))) {
                PrintStream out = errors[0] > 0 ? System.err : System.out;
                out.println("Completed " + count + " iterations with "
                        + errors[0] + " errors");
                out.flush();
                totalErrors += errors[0];
                count = 0;
                errors[0] = 0;
            }
        }

        if ((i[0] % outputCountAt != 0) || !verboseCount) {
            totalErrors += errors[0];
            PrintStream out = totalErrors > 0 ? System.err : System.out;
            out.println("Completed " + i[0] + " iterations with " + totalErrors
                    + " errors");
            out.flush();
        }
        System.out.println("Time to execute = "
                + (System.currentTimeMillis() - t0) / 1000.0 + "s");
    }

}