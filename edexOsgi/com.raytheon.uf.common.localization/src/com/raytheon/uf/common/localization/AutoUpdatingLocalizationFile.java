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

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Wrapper class that auto updates the internal LocalizationFile with the
 * highest {@link LocalizationLevel} version. Has update listening mechanism to
 * notify when that changes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AutoUpdatingLocalizationFile {

    public static interface AutoUpdatingFileChangedListener {

        public void fileChanged(AutoUpdatingLocalizationFile file);

    }

    private static class AutoUpdatingFileObserver extends
            WeakReference<AutoUpdatingLocalizationFile> implements
            ILocalizationFileObserver {

        private static ReferenceQueue<AutoUpdatingLocalizationFile> queue = new ReferenceQueue<AutoUpdatingLocalizationFile>();

        private LocalizationFile registeredFile;

        /**
         * @param referent
         */
        public AutoUpdatingFileObserver(AutoUpdatingLocalizationFile referent) {
            super(referent, queue);
            this.registeredFile = referent.getFile();
            this.registeredFile.addFileUpdatedObserver(this);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.localization.ILocalizationFileObserver#fileUpdated
         * (com.raytheon.uf.common.localization.FileUpdatedMessage)
         */
        @Override
        public void fileUpdated(FileUpdatedMessage message) {
            AutoUpdatingLocalizationFile file = get();
            if (file != null) {
                switch (message.getChangeType()) {
                case ADDED:
                case DELETED:
                    if (registeredFile != null) {
                        registeredFile.removeFileUpdatedObserver(this);
                    }
                    LocalizationFile newFile = file.internalFile = file
                            .getHighestLevelFile(file.filePath, file.type);
                    if (newFile == null) {
                        // Always ensure we are still listening even when no
                        // file exists so if file is created, we get notified
                        IPathManager mgr = PathManagerFactory.getPathManager();
                        newFile = mgr.getLocalizationFile(mgr.getContext(
                                file.type, LocalizationLevel.BASE),
                                file.filePath);
                    }
                    registeredFile = newFile;
                    registeredFile.addFileUpdatedObserver(this);
                }
                file.notifyListeners();
            } else {
                cleanupReferences();
            }
        }

        private void cleanup() {
            if (this.registeredFile != null) {
                this.registeredFile.removeFileUpdatedObserver(this);
                this.registeredFile = null;
            }
        }

        public static void cleanupReferences() {
            AutoUpdatingFileObserver observer;
            while ((observer = (AutoUpdatingFileObserver) queue.poll()) != null) {
                observer.cleanup();
            }
        }
    }

    private final String filePath;

    private final LocalizationType type;

    private final Set<AutoUpdatingFileChangedListener> listeners = new LinkedHashSet<AutoUpdatingFileChangedListener>();

    private LocalizationFile internalFile;

    /**
     * Constructs an auto updating localization file from an existing file
     * 
     * @param file
     */
    public AutoUpdatingLocalizationFile(LocalizationFile file) {
        this.filePath = file.getName();
        this.type = file.getContext().getLocalizationType();
        this.internalFile = file;
        new AutoUpdatingFileObserver(this);
    }

    /**
     * Constructs and auto updating localization file from a file path and type
     * 
     * @param filePath
     * @param type
     * @throws LocalizationException
     */
    public AutoUpdatingLocalizationFile(String filePath, LocalizationType type)
            throws LocalizationException {
        this.filePath = filePath;
        this.type = type;
        LocalizationFile file = getHighestLevelFile(filePath, type);
        if (file == null) {
            throw new LocalizationException("File with path, " + filePath
                    + ", could not be found");
        }
        internalFile = file;
        new AutoUpdatingFileObserver(this);
    }

    private LocalizationFile getHighestLevelFile(String filePath,
            LocalizationType type) {
        Map<LocalizationLevel, LocalizationFile> hierarchy = PathManagerFactory
                .getPathManager().getTieredLocalizationFile(type, filePath);
        LocalizationFile file = null;
        for (LocalizationLevel level : LocalizationLevel.values()) {
            LocalizationFile levelFile = hierarchy.get(level);
            if (levelFile != null) {
                file = levelFile;
            }
        }
        return file;
    }

    /**
     * Gets the localization file path of the updating file
     * 
     * @return
     */
    public String getFilePath() {
        return filePath;
    }

    /**
     * Gets the localization type of the updating file
     * 
     * @return
     */
    public LocalizationType getType() {
        return type;
    }

    /**
     * Gets the {@link LocalizationFile} object from the updating file
     * 
     * @return
     */
    public LocalizationFile getFile() {
        return internalFile;
    }

    /**
     * Loads the localization file as an Object given the manager and expected
     * type
     * 
     * @param manager
     * @param type
     * @return
     * @throws SerializationException
     */
    public <T> T loadObject(JAXBManager manager, Class<T> type)
            throws SerializationException {
        if (internalFile == null) {
            return null;
        }
        try {
            return type.cast(manager.jaxbUnmarshalFromInputStream(internalFile
                    .openInputStream()));
        } catch (LocalizationException e) {
            throw new SerializationException(
                    "Error opening LocalizationFile input stream", e);
        }
    }

    /**
     * Adds a listener for file changed events
     * 
     * @param listener
     */
    public void addListener(AutoUpdatingFileChangedListener listener) {
        synchronized (listeners) {
            listeners.add(listener);
        }
    }

    /**
     * Removes a listener for file changed events
     * 
     * @param listener
     */
    public void removeListener(AutoUpdatingFileChangedListener listener) {
        synchronized (listeners) {
            listeners.remove(listener);
        }
    }

    /**
     * Notifies the listeners the file has changed
     */
    private void notifyListeners() {
        List<AutoUpdatingFileChangedListener> toNotify = new ArrayList<AutoUpdatingFileChangedListener>();
        synchronized (listeners) {
            toNotify.addAll(listeners);
        }
        for (AutoUpdatingFileChangedListener listener : toNotify) {
            listener.fileChanged(this);
        }
    }

}
