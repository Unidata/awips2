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
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.localization.FileLocker.Type;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationAdapter.ListResponse;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Common localization notification observer. Automatically cleans up
 * LocaliationFiles listening using WeakReferences
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2011            mschenke     Initial creation
 * Sep 17, 2012 #875       rferrel     Added check that file exists before 
 *                                      deleting it.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationNotificationObserver {

    private static class LocalizationTypeFileKey {
        private final LocalizationType type;

        private final String path;

        public LocalizationTypeFileKey(LocalizationType type, String path) {
            super();
            this.type = type;
            this.path = path;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((path == null) ? 0 : path.hashCode());
            result = prime * result + ((type == null) ? 0 : type.hashCode());
            return result;
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
            final LocalizationTypeFileKey other = (LocalizationTypeFileKey) obj;
            if (path == null) {
                if (other.path != null) {
                    return false;
                }
            } else if (!path.equals(other.path)) {
                return false;
            }
            if (type == null) {
                if (other.type != null) {
                    return false;
                }
            } else if (!type.equals(other.type)) {
                return false;
            }
            return true;
        }

        @Override
        public String toString() {
            return this.type + "/" + this.path;
        }
    }

    public static final String LOCALIZATION_TOPIC = "edex.alerts.utility";

    private static LocalizationNotificationObserver instance = null;

    private Set<ILocalizationFileObserver> globalObservers = new HashSet<ILocalizationFileObserver>();

    private final Map<LocalizationTypeFileKey, Set<LocalizationFile>> observedFiles;

    private PathManager pm;

    public static synchronized LocalizationNotificationObserver getInstance() {
        if (instance == null) {
            instance = new LocalizationNotificationObserver();
        }
        return instance;
    }

    /**
     * Adds an observed file to the notification manager, will auto get removed
     * when no references left to LocalizationFile
     * 
     * @param lf
     */
    void addObservedFile(LocalizationFile lf) {
        LocalizationTypeFileKey key = new LocalizationTypeFileKey(lf
                .getContext().getLocalizationType(), lf.getName());

        Set<LocalizationFile> lfList;
        synchronized (observedFiles) {
            lfList = observedFiles.get(key);
            if (lfList == null) {
                lfList = new HashSet<LocalizationFile>();
                observedFiles.put(key, lfList);
            }
        }
        synchronized (lfList) {
            if (lfList.add(lf) == false) {
                // Contract between IPathManager/LocalizationFile will force
                // this to never occur and will be developer mistake if so
                throw new RuntimeException(
                        "Internal Error: Attempted to register LocalizationFile which had already been registered");
            }
        }
    }

    /**
     * Adds a global file change observer. This observer will get notified of
     * all FileUpdatedMessages received
     * 
     * @param observer
     */
    public void addGlobalFileChangeObserver(ILocalizationFileObserver observer) {
        synchronized (globalObservers) {
            globalObservers.add(observer);
        }
    }

    /**
     * Remove a global file change observer
     * 
     * @param observer
     */
    public void removeGlobalFileChangeObserver(
            ILocalizationFileObserver observer) {
        synchronized (globalObservers) {
            globalObservers.remove(observer);
        }
    }

    private LocalizationNotificationObserver() {
        observedFiles = new ConcurrentHashMap<LocalizationTypeFileKey, Set<LocalizationFile>>();
        pm = (PathManager) PathManagerFactory.getPathManager();
    }

    public synchronized void fileUpdateMessageRecieved(FileUpdatedMessage fum) {
        LocalizationType type = fum.getContext().getLocalizationType();
        String filename = LocalizationUtil.getSplitUnique(fum.getFileName());

        // Check if file update is older than latest file data
        Collection<LocalizationFile> potentialFiles = getLocalizationFiles(
                type, filename);

        // Find exact match first:
        for (LocalizationFile file : potentialFiles) {
            if (file.getContext().equals(fum.getContext())) {
                // exact match found, skip old updates (in case we have changed
                // the file since this update)
                try {
                    FileLocker.lock(this, file.file, Type.WRITE);
                    Date fileTS = file.getTimeStamp();
                    if (fileTS != null && fileTS.getTime() > fum.getTimeStamp()) {
                        // Update is older than latest file data, skip update as
                        // a newer one should be coming
                        return;
                    } else {
                        // Proceed with update process
                        processUpdate(fum, file);
                        break;
                    }
                } finally {
                    FileLocker.unlock(this, file.file);
                }
            }
        }

        // If file deleted, delete from filesystem if non directory
        if (fum.getChangeType() == FileChangeType.DELETED) {
            File local = pm.adapter.getPath(fum.getContext(), filename);
            if (local != null && local.isDirectory() == false && local.exists()) {
                try {
                    FileLocker.lock(this, local, Type.WRITE);
                    local.delete();
                } finally {
                    FileLocker.unlock(this, local);
                }
            }
        }

        // Process other file, skipping context match that was processed above
        for (LocalizationFile file : potentialFiles) {
            if (file.getContext().equals(fum.getContext()) == false) {
                processUpdate(fum, file);
            }
        }

        // Split parts so we update sub directories
        String[] parts = LocalizationUtil.splitUnique(filename);
        for (int idx = parts.length - 1; idx > 0; --idx) {
            String subpath = "";
            for (int i = 0; i < idx; ++i) {
                subpath += parts[i];
                if (i < (idx - 1)) {
                    subpath += IPathManager.SEPARATOR;
                }
            }

            // Get the file references for the key to notify
            Collection<LocalizationFile> files = getLocalizationFiles(type,
                    subpath);
            for (LocalizationFile file : files) {
                processUpdate(fum, file);
            }
        }

        // Notify system wide listeners
        synchronized (globalObservers) {
            for (ILocalizationFileObserver obs : globalObservers) {
                obs.fileUpdated(fum);
            }
        }
    }

    private void processUpdate(FileUpdatedMessage fum, LocalizationFile file) {
        LocalizationContext context = fum.getContext();
        LocalizationLevel level = context.getLocalizationLevel();
        LocalizationType type = context.getLocalizationType();
        String contextName = context.getContextName();

        int compVal = file.getContext().getLocalizationLevel().compareTo(level);
        if (compVal <= 0) {
            boolean notify = false;
            if (compVal < 0) {
                // Different level, check our context name to
                // make sure it matches update message
                String ourContextName = pm.getContext(type, level)
                        .getContextName();
                if ((ourContextName == null && contextName == null)
                        || (ourContextName != null && ourContextName
                                .equals(contextName))) {
                    notify = true;
                }
            }

            if (fum.getContext().equals(file.getContext())) {
                // context perfectly matches, make sure we
                // notify
                notify = true;
                // This file should be MODIFIED based on
                // update...
                if (file.isDirectory() == false) {
                    // Update file with new metadata
                    file.update(getMetadata(file));

                    // If we are still not a directoy and we
                    // should request the file, request it
                    if (file.isDirectory() == false && file.fileRequested) {
                        switch (fum.getChangeType()) {
                        case UPDATED:
                        case ADDED: {
                            file.getFile();
                            break;
                        }
                        }
                    }
                }
            }

            if (notify) {
                // Notify file of change
                file.notifyObservers(fum);
            }
        }
    }

    private Collection<LocalizationFile> getLocalizationFiles(
            LocalizationType type, String fileName) {
        Set<LocalizationFile> copy = new HashSet<LocalizationFile>();
        Set<LocalizationFile> lfList;
        synchronized (observedFiles) {
            lfList = observedFiles.get(new LocalizationTypeFileKey(type,
                    fileName));
        }
        if (lfList != null) {
            synchronized (lfList) {
                copy = new HashSet<LocalizationFile>(lfList);
            }
        }
        return copy;
    }

    /**
     * @param lf
     * @return
     */
    private ListResponse getMetadata(LocalizationFile lf) {
        ListResponse rval = null;
        // Update our information first
        try {
            for (ListResponse lr : lf.adapter
                    .getLocalizationMetadata(
                            new LocalizationContext[] { lf.getContext() },
                            lf.getName())) {
                if (lr != null) {
                    rval = lr;
                    break;
                }
            }
        } catch (LocalizationOpFailedException e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    "Error updating file", e);
        }

        return rval;
    }

}
