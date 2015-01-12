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
package com.raytheon.collaboration.dataserver.storage;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import com.raytheon.collaboration.dataserver.RestException;
import com.raytheon.uf.common.util.concurrent.KeyLock;
import com.raytheon.uf.common.util.concurrent.KeyLocker;

/**
 * Collaboration event object data storage that uses the file system. Uses a
 * read/write lock policy based on file path. The paths are locked from the base
 * to the target node and unlocked in reverse order.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb  6, 2014  2756      bclement     Initial creation
 * Feb 28, 2014  2756      bclement     moved to storage package, made buffer size public
 * Mar 11, 2014  2827      bclement     read methods take servlet response object
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FileManager {

    public static final int BUFFER_SIZE = 1024 * 256; // 256K

    private static final ThreadLocal<byte[]> localBuffer = new ThreadLocal<byte[]>() {

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.ThreadLocal#initialValue()
         */
        @Override
        protected byte[] initialValue() {
            return new byte[BUFFER_SIZE];
        }

    };

    private static final KeyLocker<File> locker = new KeyLocker<File>();

    private final File base;

    /**
     * @param base
     *            base storage directory
     */
    public FileManager(File base) {
        this.base = base;
    }

    /**
     * simple container to associate a file lock with it's access mode
     */
    private static class UsedLock {
        public final KeyLock<File> lock;

        public final boolean readOnly;

        public UsedLock(KeyLock<File> lock, boolean readOnly) {
            this.lock = lock;
            this.readOnly = readOnly;
        }
    }

    /**
     * Write inputstream to file system
     * 
     * @param in
     * @param file
     * @throws IOException
     * @throws RestException
     */
    public void writeFile(InputStream in, File file) throws IOException,
            RestException {
        OutputStream out = null;
        List<UsedLock> locks = null;
        KeyLock<File> targetLock = null;
        try {
            File parent = file.getParentFile();
            // lock all files starting at child of base going to parent
            locks = getCreateLocks(parent);
            // lock target file for modification
            targetLock = locker.getLock(file);
            targetLock.lock();
            createDirs(locks);
            out = new FileOutputStream(file);
            copy(in, out);
        } finally {
            // unlock in reverse order
            if (targetLock != null) {
                targetLock.unlock();
            }
            unlock(locks);
            in.close();
            if (out != null) {
                out.flush();
                out.close();
            }
        }
    }

    /**
     * Each item in locks represents a node in a directory path. Attempts to
     * create each node in order.
     * 
     * @param locks
     * @throws IOException
     */
    private void createDirs(List<UsedLock> locks) throws IOException {
        for (UsedLock lock : locks) {
            File f = lock.lock.getKey();
            if (!f.exists()) {
                if (!f.mkdir()) {
                    throw new IOException("Unable to create file: "
                            + f.getAbsolutePath());
                }
            }
        }
    }

    /**
     * Unlock each file lock in reverse order
     * 
     * @param locks
     */
    private void unlock(List<UsedLock> locks) {
        if (locks != null) {
            ListIterator<UsedLock> iter = locks.listIterator(locks.size());
            while (iter.hasPrevious()) {
                UsedLock ul = iter.previous();
                if (ul.readOnly) {
                    ul.lock.readUnlock();
                } else {
                    ul.lock.unlock();
                }
            }
        }
    }

    /**
     * Get a list of write file locks for each directory from the base to the
     * target directory
     * 
     * @param targetDirectory
     * @return
     * @throws RestException
     */
    private List<UsedLock> getCreateLocks(File targetDirectory)
            throws RestException {
        List<KeyLock<File>> locks = new ArrayList<KeyLock<File>>();
        // walk backwards getting locks for each node
        while (!base.equals(targetDirectory)) {
            locks.add(locker.getLock(targetDirectory));
            targetDirectory = targetDirectory.getParentFile();
        }
        List<UsedLock> rval = new ArrayList<UsedLock>(locks.size());
        // reverse iterate so we lock in the correct order
        ListIterator<KeyLock<File>> iter = locks.listIterator(locks.size());
        while (iter.hasPrevious()) {
            KeyLock<File> lock = iter.previous();
            File f = lock.getKey();
            if (!f.exists()) {
                lock.lock();
                rval.add(new UsedLock(lock, false));
            } else if (f.isDirectory()) {
                lock.readLock();
                rval.add(new UsedLock(lock, true));
            } else {
                // file exists but is not a directory
                throw new RestException(HttpServletResponse.SC_CONFLICT,
                        "Resource already exists: " + f.getAbsolutePath());
            }
        }
        return rval;
    }

    /**
     * Output file to servlet response
     * 
     * @param file
     * @param resp
     * @throws IOException
     * @throws RestException
     */
    public void readFile(File file, HttpServletResponse resp)
            throws IOException,
            RestException {
        InputStream in = null;
        List<UsedLock> locks = null;
        ServletOutputStream out = null;
        try {
            locks = getReadLocks(file);
            if ( !file.isFile()){
                throw new RestException(HttpServletResponse.SC_NOT_FOUND,
                        "No Such File: " + file.getAbsoluteFile());
            }
            in = new FileInputStream(file);
            /*
             * We have to wait until we are sure we can read the file before we
             * get the outputstream. This is because the act of getting the
             * output stream commits to using it with a 200 response. If we
             * throw an error, we won't be able to use the response object to
             * send an error and we will send a 200 with an empty body
             */
            out = resp.getOutputStream();
            copy(in, out);
        } finally {
            unlock(locks);
            if (in != null) {
                in.close();
            }
            if (out != null) {
                out.close();
            }
        }
    }

    /**
     * Get a list of read only file locks for each file from the base to the
     * target file
     * 
     * @param file
     * @return
     */
    private List<UsedLock> getReadLocks(File file) {
        List<KeyLock<File>> locks = new ArrayList<KeyLock<File>>();
        // walk backwards getting locks for each node
        while (!base.equals(file)) {
            locks.add(locker.getLock(file));
            file = file.getParentFile();
        }
        List<UsedLock> rval = new ArrayList<UsedLock>(locks.size());
        // reverse iterate so we lock in the correct order
        ListIterator<KeyLock<File>> iter = locks.listIterator(locks.size());
        while (iter.hasPrevious()) {
            KeyLock<File> lock = iter.previous();
            lock.readLock();
            rval.add(new UsedLock(lock, true));
        }
        return rval;
    }

    /**
     * Output contents of directory to response in XML format
     * 
     * @param directory
     * @param resp
     * @throws IOException
     * @throws RestException
     */
    public void readDirectoryAsXml(File directory, HttpServletResponse resp)
            throws IOException, RestException {
        List<UsedLock> locks = null;
        Writer w = null;
        try {
            locks = getReadLocks(directory);
            if (!directory.isDirectory()) {
                // someone else modified it while waiting for lock
                throw new RestException(HttpServletResponse.SC_NOT_FOUND,
                        "No Such Directory: " + directory.getAbsolutePath());
            }
            w = resp.getWriter();
            w.write("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
            w.write("<Contents xmlns=\"urn:uf:viz:collaboration\">");
            for (File f : directory.listFiles()) {
                if (f.isDirectory()) {
                    writeTextTag(w, "Directory", f.getName());
                } else {
                    writeTextTag(w, "File", f.getName());
                }
            }
            w.write("</Contents>");
        } finally {
            unlock(locks);
            if (w != null) {
                w.flush();
                w.close();
            }
        }
    }

    /**
     * Write tag with text element to writer
     * 
     * @param w
     * @param tagName
     * @param text
     * @throws IOException
     */
    private void writeTextTag(Writer w, String tagName, String text)
            throws IOException {
        w.write("<");
        w.write(tagName);
        w.write(">");
        w.write(text);
        w.write("</");
        w.write(tagName);
        w.write(">");
    }

    /**
     * Output contents of directory to response in HTML format
     * 
     * @param directory
     * @param resp
     * @throws IOException
     * @throws RestException
     */
    public void readDirectoryAsHtml(File directory, HttpServletResponse resp)
            throws IOException, RestException {
        List<UsedLock> locks = null;
        Writer w = null;
        try {
            locks = getReadLocks(directory);
            if (!directory.isDirectory()) {
                // someone else modified it while waiting for lock
                throw new RestException(HttpServletResponse.SC_NOT_FOUND,
                        "No Such Directory: " + directory.getAbsolutePath());
            }
            w = resp.getWriter();
            w.write("<!DOCTYPE html>\n");
            w.write("<html><body>");
            for (File f : directory.listFiles()) {
                String name = f.getName();
                if (f.isDirectory() && !name.endsWith("/")) {
                    name = name + "/";
                }
                writeLinkTag(w, name);
            }
            w.write("</body></html>");
        } finally {
            unlock(locks);
            if (w != null) {
                w.flush();
                w.close();
            }
        }
    }

    /**
     * Write html link reference tag to writer
     * 
     * @param w
     * @param href
     * @throws IOException
     */
    private void writeLinkTag(Writer w, String href) throws IOException {
        w.write("<a href=\"");
        w.write(href);
        w.write("\">");
        w.write(href);
        w.write("</a></br>");
    }

    /**
     * Copy bytes from input to output. Flushes output after writing.
     * 
     * @param in
     * @param out
     * @throws IOException
     */
    public static void copy(InputStream in, OutputStream out)
            throws IOException {
        byte[] buff = localBuffer.get();
        int len;
        while ((len = in.read(buff)) != -1) {
            out.write(buff, 0, len);
        }
        out.flush();
    }

    /**
     * Delete target file and all children
     * 
     * @param file
     * @throws IOException
     * @throws RestException
     */
    public void delete(File file) throws IOException, RestException {
        List<UsedLock> parentLocks = null;
        KeyLock<File> targetLock = null;
        try {
            File parent = file.getParentFile();
            parentLocks = getReadLocks(parent);
            targetLock = locker.getLock(file);
            targetLock.lock();
            if (!file.exists()) {
                throw new RestException(HttpServletResponse.SC_NOT_FOUND,
                        "No Such Resource: " + file.getAbsolutePath());
            }
            // we don't have to lock children since we have a write lock on the
            // directory
            deleteRecursive(file);
        } finally {
            if (targetLock != null) {
                targetLock.unlock();
            }
            unlock(parentLocks);
        }
    }

    /**
     * Recursive method to delete file and children
     * 
     * @param file
     * @throws IOException
     */
    private void deleteRecursive(File file) throws IOException {
        if (file.isDirectory()) {
            for (File sub : file.listFiles()) {
                deleteRecursive(sub);
            }
        }
        if (!file.delete()) {
            throw new IOException("Unable to delete file: "
                    + file.getAbsolutePath());
        }
    }

}
