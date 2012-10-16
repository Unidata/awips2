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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

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

    private static class LocalizationFileKey {
        private final LocalizationType type;

        private final String path;

        public LocalizationFileKey(LocalizationType type, String path) {
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
            final LocalizationFileKey other = (LocalizationFileKey) obj;
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

    private final Map<LocalizationFileKey, Set<LocalizationFileRef>> observedFiles;

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
        LocalizationFileKey key = new LocalizationFileKey(lf.getContext()
                .getLocalizationType(), lf.getName());

        LocalizationFileRef ref = new LocalizationFileRef(lf);
        Set<LocalizationFileRef> lfList;
        synchronized (observedFiles) {
            lfList = observedFiles.get(key);
            if (lfList == null) {
                lfList = new HashSet<LocalizationFileRef>();
                // add now so cleanUpRefs() cannot remove the set
                lfList.add(ref);
                observedFiles.put(key, lfList);
            }
        }
        synchronized (lfList) {
            lfList.add(ref);
        }
        cleanUpRefs();
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
        observedFiles = new ConcurrentHashMap<LocalizationFileKey, Set<LocalizationFileRef>>();
        pm = (PathManager) PathManagerFactory.getPathManager();
    }

    public void fileUpdateMessageRecieved(FileUpdatedMessage fum) {
        LocalizationType type = fum.getContext().getLocalizationType();
        LocalizationLevel level = fum.getContext().getLocalizationLevel();
        String contextName = fum.getContext().getContextName();
        String filename = LocalizationUtil.getSplitUnique(fum.getFileName());

        // Cleanup LocalizationFiles that have been GC'd
        cleanUpRefs();

        // If file deleted, delete from filesystem if non directory
        if (fum.getChangeType() == FileChangeType.DELETED) {
            File local = pm.adapter.getPath(fum.getContext(), filename);
            if (local != null && local.isDirectory() == false && local.exists()) {
                local.delete();
            }
        }

        // Response map, only request updated data once per file reference key
        Map<Object, ListResponse> responseMap = new HashMap<Object, ListResponse>();

        do {
            LocalizationFileKey key = new LocalizationFileKey(type, filename);
            // Get the file references for the key to notify
            Set<LocalizationFileRef> lfList;
            synchronized (observedFiles) {
                lfList = observedFiles.get(key);
            }
            if (lfList != null) {
                Set<LocalizationFileRef> copy;
                synchronized (lfList) {
                    copy = new HashSet<LocalizationFileRef>(lfList);
                }
                // Flags so we only delete or request once
                boolean requested = false;
                for (LocalizationFileRef ref : copy) {
                    LocalizationFile lf = ref.get();
                    if (lf != null) {
                        // If null, means it was garbage collected, will be
                        // caught next update
                        int compVal = lf.getContext().getLocalizationLevel()
                                .compareTo(level);
                        if (compVal <= 0) {
                            boolean notify = false;

                            if (compVal < 0) {
                                // Different level, check our context name to
                                // make sure it matches update message
                                String ourContextName = pm.getContext(type,
                                        level).getContextName();
                                if ((ourContextName == null && contextName == null)
                                        || (ourContextName != null && ourContextName
                                                .equals(contextName))) {
                                    notify = true;
                                }
                            }

                            // This file should be NOTIFEID based on update...
                            ListResponse resp = null;
                            if (fum.getContext().equals(lf.getContext())) {
                                // context perfectly matches, make sure we
                                // notify
                                notify = true;
                                // This file should be MODIFIED based on
                                // update...
                                if (lf.isDirectory() == false) {
                                    resp = responseMap.get(ref.getKey());
                                    if (resp == null) {
                                        // Make sure we only request metadata
                                        // once per file update
                                        resp = getMetadata(lf);
                                        responseMap.put(ref.getKey(), resp);
                                    }

                                    // Update file with new metadata
                                    lf.update(resp);

                                    // If we are still not a directoy and we
                                    // should request the file, request it
                                    if (lf.isDirectory() == false
                                            && lf.fileRequested && !requested) {
                                        switch (fum.getChangeType()) {
                                        case UPDATED:
                                        case ADDED: {
                                            requested = true;
                                            lf.getFile();
                                            break;
                                        }
                                        }
                                    }
                                }
                            }

                            if (notify) {
                                // Notify file of change
                                lf.notifyObservers(fum);
                            }
                        }
                    }
                }
            }

            int pos = filename.lastIndexOf(IPathManager.SEPARATOR);
            if (pos > 0) {
                filename = filename.substring(0, pos);
            } else {
                filename = "";
            }
        } while (!filename.isEmpty());

        // Notify system wide listeners
        synchronized (globalObservers) {
            for (ILocalizationFileObserver obs : globalObservers) {
                obs.fileUpdated(fum);
            }
        }
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

    /**
     * Clean up stale references
     */
    private void cleanUpRefs() {
        List<LocalizationFileKey> keysToRemove = new ArrayList<LocalizationFileKey>();
        for (LocalizationFileKey key : observedFiles.keySet()) {
            Set<LocalizationFileRef> refs = observedFiles.get(key);
            if (refs == null || refs.size() == 0) {
                keysToRemove.add(key);
            } else {
                synchronized (refs) {
                    List<LocalizationFileRef> toRemove = new ArrayList<LocalizationFileRef>();
                    for (LocalizationFileRef ref : refs) {
                        if (ref.get() == null) {
                            toRemove.add(ref);
                        }
                    }
                    refs.removeAll(toRemove);
                    if (refs.size() == 0) {
                        keysToRemove.add(key);
                    }
                }
            }
        }

        for (LocalizationFileKey key : keysToRemove) {
            observedFiles.remove(key);
        }
    }
}
