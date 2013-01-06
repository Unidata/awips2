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
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.localization.FileLocker.Type;
import com.raytheon.uf.common.localization.ILocalizationAdapter.ListResponse;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Represents a file in the localization system. <BR>
 * <BR>
 * A LocalizationFile cannot be constructed directly, it must be obtained using
 * the PathManager. <BR>
 * <BR>
 * A file is generally not realized until the getFile() method is called. At
 * that point, it becomes a real file on the local system. Prior to calling
 * getFile(), the LocalizationFile can be considered a pointer. Operations are
 * provided directly on this interface that allow the changes to be persisted
 * both to the local filesystem, and also to the localization store. <BR>
 * <BR>
 * <HR>
 * <B>Common Use Cases:</B> <BR>
 * <UL>
 * <LI>Reading a file - A user should call getFile() and interact with the file
 * as if it was a regular filesystem file.
 * <LI>Writing to a file - A user can write to a file by obtaining the
 * java.io.File object using the getFile() method and then writing to it as if
 * it was a regular file using conventional file I/O methods. To save the file
 * back to the localization store, call save().
 * <LI>Renaming a file - Calling rename() will rename both any local file (if it
 * exists) and the copy in the localization store.
 * <LI>Delete - Calling delete() will delete any local file (if it exists), and
 * delete the copy on the localization store.
 * </UL>
 * 
 * 
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Mar 27, 2008				njensen	    Initial creation
 * May 01, 2008             chammack    Added Localization synchronization information
 * May 15, 2008 #878        chammack    Refactor
 * Mar 24, 2010 #2866       randerso    Removed conditional around call to retrieve. 
 *                                      This was added as part of an effort to improve 
 *                                      localization performance but caused updated 
 *                                      files on the server not to be retrieved.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public final class LocalizationFile implements Comparable<LocalizationFile> {
    private static transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationFile.class);

    /**
     * Class {@link LocalizationFile} exposes to the
     * {@link ILocalizationAdapter} objects so they can modify the file if
     * anything changes. Don't want to expose ability to modify
     * {@link LocalizationFile} contents to everyone
     * 
     * @author mschenke
     * @version 1.0
     */
    public class ModifiableLocalizationFile {

        private ModifiableLocalizationFile() {
            // Private constructor
        }

        public LocalizationFile getLocalizationFile() {
            return LocalizationFile.this;
        }

        public void setTimeStamp(Date timeStamp) {
            getLocalizationFile().fileTimestamp = timeStamp;
        }

        public void setFileChecksum(String checksum) {
            getLocalizationFile().fileCheckSum = checksum;
        }

        public void setIsAvailableOnServer(boolean isAvailableOnServer) {
            getLocalizationFile().isAvailableOnServer = isAvailableOnServer;
        }

        public void setIsDirectory(boolean isDirectory) {
            getLocalizationFile().isDirectory = isDirectory;
        }

        public File getLocalFile() {
            return getLocalizationFile().file;
        }

        public String getFileName() {
            return getLocalizationFile().path;
        }

        public LocalizationContext getContext() {
            return getLocalizationFile().context;
        }
    }

    /** Local file pointer to localization file, will never be null */
    protected final File file;

    /** The file timestamp on the server, may be null if file doesn't exist yet */
    private Date fileTimestamp;

    /** Checksum of file on server, may be null if file doesn't exist yet */
    private String fileCheckSum;

    /** The context of the file, never null */
    private final LocalizationContext context;

    /** True if file points to a directory, false otherwise */
    private boolean isDirectory;

    /** Denotes whether the file exists on the server */
    private boolean isAvailableOnServer;

    /** The localization adapter for the file */
    protected final ILocalizationAdapter adapter;

    /** The localization path of the file */
    private final String path;

    /** Protection flag of file, if file cannot be overridden, it is protected */
    private LocalizationLevel protectedLevel;

    /** File changed observers */
    private final Set<ILocalizationFileObserver> observers = new HashSet<ILocalizationFileObserver>();

    /** Flag to set if file has been requested */
    protected boolean fileRequested = false;

    /**
     * Construct a null localization file, used to keep track of files that
     * cannot exist.
     */
    LocalizationFile() {
        file = null;
        path = null;
        adapter = null;
        context = null;
    }

    /**
     * Check if a file is null type
     * 
     * @return
     */
    boolean isNull() {
        return (adapter == null) && (path == null) && (context == null)
                && (file == null);
    }

    LocalizationFile(ILocalizationAdapter adapter, LocalizationContext context,
            File file, Date date, String path, String checkSum,
            boolean isDirectory, boolean existsOnServer,
            LocalizationLevel protectedLevel) {
        this.adapter = adapter;
        this.context = context;
        this.file = file;
        this.fileCheckSum = checkSum;
        this.fileTimestamp = date;
        this.isAvailableOnServer = existsOnServer;
        this.isDirectory = isDirectory;
        this.path = LocalizationUtil.getSplitUnique(path);
        this.protectedLevel = protectedLevel;
        LocalizationNotificationObserver.getInstance().addObservedFile(this);
    }

    /**
     * Update the localization file with new metadata
     * 
     * @param metadata
     */
    void update(ListResponse metadata) {
        if (metadata != null) {
            // Update new metadata
            this.isAvailableOnServer = metadata.existsOnServer;
            this.fileTimestamp = metadata.date;
            this.fileCheckSum = metadata.checkSum;
            this.isDirectory = metadata.isDirectory;
            this.protectedLevel = metadata.protectedLevel;
        }
    }

    /**
     * Returns a modifiable version of the localization file. Meant to be used
     * internally within localization only which is why package level
     * 
     * @return
     */
    ModifiableLocalizationFile getModifiableFile() {
        return new ModifiableLocalizationFile();
    }

    /**
     * This returns the time stamp of the file where it is stored, not the local
     * version of the file
     * 
     * @return the file time stamp, may be null if file doesn't exist yet
     */
    public Date getTimeStamp() {
        return fileTimestamp;
    }

    /**
     * This returns the check sum of the file where it is stored
     * 
     * @return the file check sum, may be null if file doesn't exist yet
     */
    public String getCheckSum() {
        return fileCheckSum;
    }

    /**
     * Return a local file pointer that can be used to interact with the data in
     * the file. This method is NOT recommended for use in reading/writing to
     * the file. The methods openInputStream and openOutputStream should be used
     * to safely read/write to the file
     * 
     * <BR>
     * Prior to calling this method, the file is not guaranteed to exist on the
     * local filesystem. Note that in some cases (e.g. when creating a file),
     * the File returned may not actually exist.
     * 
     * @param retrieveFile
     *            a flag that specifies whether the file should be downloaded if
     *            the local file pointer doesn't exist
     * @return the file
     */
    public File getFile(boolean retrieveFile) throws LocalizationException {
        if (retrieveFile) {
            fileRequested = true;
        }
        if (isAvailableOnServer && retrieveFile) {
            if (isDirectory) {
                file.mkdirs();
            }
            adapter.retrieve(this);
        }

        if ((isDirectory == false) && !file.exists()) {
            try {
                file.getParentFile().mkdirs();
            } catch (Throwable t) {
                // try to create the file's directory automatically, but if
                // it fails, don't report it as it is just something to do
                // to help the user of the file for easier creation of the
                // file
            }
        }

        return file;
    }

    /**
     * This method is not recommended for use, use openInputStream() for reading
     * the LocalizationFile and openOutputStream() for writing to the
     * LocalizationFile. ALWAYS close() the streams when done reading/writing as
     * those methods will auto lock the file. If must use this method, call
     * FileLocker.lock/unlock when using the file
     * 
     * @return File pointer
     */
    public File getFile() {
        try {
            return getFile(true);
        } catch (LocalizationException e) {
            return adapter.getPath(context, path);
        }
    }

    /**
     * Creates an InputStream for the LocalizationFile
     * 
     * @return the InputStream to be used for reading the file
     * @throws LocalizationException
     */
    public LocalizationFileInputStream openInputStream()
            throws LocalizationException {
        try {
            return new LocalizationFileInputStream(this);
        } catch (FileNotFoundException e) {
            throw new LocalizationException("Error opening input stream", e);
        }
    }

    /**
     * Reads the file into memory utilizing correct locking resources.
     * 
     * @return
     * @throws LocalizationException
     */
    public byte[] read() throws LocalizationException {
        byte[] rval = null;
        InputStream is = null;
        File f = getFile();
        try {
            is = openInputStream();

            // Get the size of the file
            long length = f.length();

            // TODO
            // You cannot create an array using a long type.
            // It needs to be an int type.
            // Before converting to an int type, check
            // to ensure that file is not larger than Integer.MAX_VALUE.
            // Create the byte array to hold the data
            rval = new byte[(int) length];

            // Read in the bytes
            int offset = 0;
            int numRead = 0;
            while ((offset < rval.length)
                    && ((numRead = is.read(rval, offset, rval.length - offset)) >= 0)) {
                offset += numRead;
            }

            // Ensure all the bytes have been read in
            if (offset < rval.length) {
                throw new LocalizationException(
                        "Could not completely read file " + f.getName());
            }
        } catch (IOException e) {
            throw new LocalizationException("Could not read file "
                    + f.getName(), e);
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException e) {
                    statusHandler
                            .handle(Priority.WARN,
                                    "Error occurred closing input stream for localization file",
                                    e);
                }
            }
        }
        return rval;
    }

    /**
     * Creates an OutputStream for the LocalizationFile
     * 
     * @return the OutputStream to be used for writing to the file
     * @throws LocalizationException
     */
    public LocalizationFileOutputStream openOutputStream()
            throws LocalizationException {
        return openOutputStream(false);
    }

    /**
     * Creates an OutputStream for the LocalizationFile
     * 
     * @param isAppending
     * @return the OutputStream to be used for writing to the file
     * @throws LocalizationException
     */
    public LocalizationFileOutputStream openOutputStream(boolean isAppending)
            throws LocalizationException {
        try {
            return new LocalizationFileOutputStream(this, isAppending);
        } catch (FileNotFoundException e) {
            throw new LocalizationException("Error opening input stream", e);
        }
    }

    /**
     * Writes the data to the underlying file. Also persists the file back to
     * the localization store.
     * 
     * @param bytes
     * @throws LocalizationException
     */
    public void write(byte[] bytes) throws LocalizationException {
        LocalizationFileOutputStream os = null;
        try {
            os = openOutputStream();
            os.write(bytes);
        } catch (IOException e) {
            throw new LocalizationException("Could not write to file "
                    + file.getName(), e);
        } finally {
            if (os != null) {
                try {
                    os.closeAndSave();
                } catch (IOException e) {
                    statusHandler.handle(Priority.INFO,
                            "Failed to close output stream for file", e);
                }
            }
        }
    }

    /**
     * 
     * Return the localization context that this file belongs to
     * 
     * @return the context
     */
    public LocalizationContext getContext() {
        return context;
    }

    /**
     * Returns true if the file is available on server.
     * 
     * @return true if the file is available on the server
     */
    public boolean isAvailableOnServer() {
        return isAvailableOnServer;
    }

    /**
     * Checks if the file points to a directory
     * 
     * @return true if the file is actually a directory
     */
    public boolean isDirectory() {
        return isDirectory;
    }

    /**
     * Check if file is protected
     * 
     * @return true if file is protected and cannot be overridden
     */
    public boolean isProtected() {
        return protectedLevel != null;
    }

    /**
     * Gets the level the file is protected at, null otherwise
     * 
     * @return
     */
    public LocalizationLevel getProtectedLevel() {
        return protectedLevel;
    }

    /**
     * Save the file back to the localization store
     * 
     * @throws LocalizationOpFailedException
     */
    public boolean save() throws LocalizationOpFailedException {
        try {
            FileLocker.lock(this, file, Type.WRITE);
            String checksum = "";
            try {
                checksum = Checksum.getMD5Checksum(file);
            } catch (Exception e) {
                // Ignore
            }
            // Check if file differs from server file
            if (!checksum.equals(fileCheckSum)) {
                boolean rval = adapter.save(getModifiableFile());
                if (rval) {
                    fileCheckSum = checksum;
                }
                return rval;
            }

            // Local file matches server file, success technically
            return true;
        } finally {
            FileLocker.unlock(this, file);
        }
    }

    /**
     * Return the file path (not including the context directories)
     * 
     * @return the file path
     */
    public String getName() {
        return path;
    }

    /**
     * Delete the localization file
     * 
     * @return true if file is deleted, false if file still exists
     * @throws LocalizationOpFailedException
     */
    public boolean delete() throws LocalizationOpFailedException {
        try {
            FileLocker.lock(this, file, Type.WRITE);
            if (exists()) {
                return adapter.delete(getModifiableFile());
            } else if (file.exists()) {
                // Local file does actually exist, delete manually
                return file.delete();
            }

            // File doesn't exist, it is deleted, so technically success?
            return true;
        } finally {
            FileLocker.unlock(this, file);
        }
    }

    /**
     * Check if the localization file exists
     * 
     * @return true if the file exists
     */
    public boolean exists() {
        return (isNull() == false) && adapter.exists(this);
    }

    /**
     * Add an observer on the LocalizationFile
     * 
     * @param observer
     */
    public void addFileUpdatedObserver(ILocalizationFileObserver observer) {
        synchronized (observers) {
            observers.add(observer);
        }
    }

    /**
     * Remove the observer as a listener on the file
     * 
     * @param observer
     */
    public void removeFileUpdatedObserver(ILocalizationFileObserver observer) {
        synchronized (observers) {
            observers.remove(observer);
        }
    }

    /**
     * Notify the observers for the LocalizationFile of the change
     * 
     * @param message
     *            update message
     * @param metadata
     *            updated metadata for the file based on the message
     */
    void notifyObservers(FileUpdatedMessage message) {
        List<ILocalizationFileObserver> toNotify = new ArrayList<ILocalizationFileObserver>();
        synchronized (observers) {
            toNotify.addAll(observers);
        }
        for (Object observer : toNotify) {
            try {
                ((ILocalizationFileObserver) observer).fileUpdated(message);
            } catch (Throwable t) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error notifying observer of file change", t);
            }
        }
    }

    /**
     * Returns the object version of this jaxb serialized file. Returns null if
     * the file does not exist or is empty.
     * 
     * @param <T>
     * @param resultClass
     * @param manager
     * @return
     * @throws LocalizationException
     */
    public <T> T jaxbUnmarshal(Class<T> resultClass, JAXBManager manager)
            throws LocalizationException {
        File f = getFile();
        if (f.exists() && (f.length() > 0)) {
            InputStream is = null;
            try {
                is = openInputStream();
                T object = resultClass.cast(manager
                        .jaxbUnmarshalFromInputStream(is));
                return object;
            } catch (Exception e) {
                throw new LocalizationException("Could not unmarshal file "
                        + file.getName(), e);
            } finally {
                if (is != null) {
                    try {
                        is.close();
                    } catch (IOException e) {
                        statusHandler.handle(Priority.WARN,
                                "Failed to close input stream for file", e);
                    }
                }
            }
        }

        return null;
    }

    @Override
    public String toString() {
        return context + IPathManager.SEPARATOR + path;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(LocalizationFile o) {
        return getName().compareTo(o.getName());
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        LocalizationFile other = (LocalizationFile) obj;
        if (context == null) {
            if (other.context != null) {
                return false;
            }
        } else if (!context.equals(other.context)) {
            return false;
        }
        if (path == null) {
            if (other.path != null) {
                return false;
            }
        } else if (!path.equals(other.path)) {
            return false;
        }
        return true;
    }

}
