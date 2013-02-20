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
package com.raytheon.uf.viz.core.maps;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.rsc.capabilities.Capabilities;

/**
 * Class to store map style preferences
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2010            randerso     Initial creation
 * Jan 25, 2013 DR 15649   D. Friedman Clone capabilities in get/put.
 *                                     Stored preferences in a sub-directory
 *                                     and observe changes.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@XmlRootElement()
@XmlAccessorType(XmlAccessType.FIELD)
public class MapStylePreferenceStore implements ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapStylePreferenceStore.class);

    private static final String MAPSTYLE_FILENAME = "mapStyles/mapstylepreferences.xml";

    private static final String OLD_MAPSTYLE_FILENAME = "mapstylepreferences.xml";

    private static class MapStylePreferenceKey {
        private String perspective;

        private String mapName;

        private MapStylePreferenceKey() {
        }

        public MapStylePreferenceKey(String perspective, String mapName) {
            this();
            this.perspective = perspective;
            this.mapName = mapName;
        }

        /**
         * @return the perspective
         */
        public String getPerspective() {
            return perspective;
        }

        /**
         * @param perspective
         *            the perspective to set
         */
        public void setPerspective(String perspective) {
            this.perspective = perspective;
        }

        /**
         * @return the mapName
         */
        public String getMapName() {
            return mapName;
        }

        /**
         * @param mapName
         *            the mapName to set
         */
        public void setMapName(String mapName) {
            this.mapName = mapName;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((mapName == null) ? 0 : mapName.hashCode());
            result = prime * result
                    + ((perspective == null) ? 0 : perspective.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
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
            MapStylePreferenceKey other = (MapStylePreferenceKey) obj;
            if (mapName == null) {
                if (other.mapName != null) {
                    return false;
                }
            } else if (!mapName.equals(other.mapName)) {
                return false;
            }
            if (perspective == null) {
                if (other.perspective != null) {
                    return false;
                }
            } else if (!perspective.equals(other.perspective)) {
                return false;
            }
            return true;
        }
    }

    @XmlTransient
    private Map<MapStylePreferenceKey, Capabilities> combinedPreferences;

    private Map<MapStylePreferenceKey, Capabilities> preferences;

    @XmlTransient
    LocalizationFile siteLf, userLf;

    @XmlTransient
    boolean needToLoad = true;

    public static MapStylePreferenceStore load() {
        MapStylePreferenceStore store = new MapStylePreferenceStore();
        store.loadFiles();
        return store;
    }

    private synchronized void loadFiles() {
        if (needToLoad)
            needToLoad = false;
        else
            return;

        IPathManager pathMgr = PathManagerFactory.getPathManager();

        if (siteLf == null) {
            siteLf = pathMgr.getLocalizationFile(pathMgr
                    .getContext(LocalizationType.CAVE_STATIC,
                            LocalizationLevel.SITE), MAPSTYLE_FILENAME);

            userLf = pathMgr.getLocalizationFile(pathMgr
                    .getContext(LocalizationType.CAVE_STATIC,
                            LocalizationLevel.USER), MAPSTYLE_FILENAME);

            ILocalizationFileObserver obs = new ILocalizationFileObserver() {
                @Override
                public void fileUpdated(FileUpdatedMessage message) {
                    synchronized (MapStylePreferenceStore.this) {
                        needToLoad = true;
                    }
                }
            };

            siteLf.addFileUpdatedObserver(obs);
            userLf.addFileUpdatedObserver(obs);

            /* DR 15649 for OB 13.3.1: If the map style preferences are in the
             * old location, move it to the correct place.  This code can be
             * removed in the future.
             */
            if (! userLf.exists()) {
                LocalizationFile oldUserLf = pathMgr.getLocalizationFile(pathMgr
                        .getContext(LocalizationType.CAVE_STATIC,
                                LocalizationLevel.USER), OLD_MAPSTYLE_FILENAME);

                if (oldUserLf.exists()) {
                    try {
                        userLf.write(oldUserLf.read());
                        oldUserLf.delete();
                        statusHandler.handle(Priority.INFO, "Moved user map style preferences to new location");
                    } catch (LocalizationException e) {
                        statusHandler.handle(Priority.PROBLEM, "Unable to move map style preferences", e);
                        e.printStackTrace();
                    }
                }
            }
        }

        if (siteLf.exists()) {
            try {
                combinedPreferences = ((MapStylePreferenceStore) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(siteLf.getFile())).preferences;
            } catch (SerializationException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Exception while loading site map style preferences",
                                e);
            }
        } else {
            combinedPreferences = new HashMap<MapStylePreferenceKey, Capabilities>();
        }

        if (userLf.exists()) {
            try {
                preferences = ((MapStylePreferenceStore) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(userLf.getFile())).preferences;

                // merge user into site
                for (Entry<MapStylePreferenceKey, Capabilities> entry : preferences
                        .entrySet()) {
                    combinedPreferences.put(entry.getKey(),
                            entry.getValue());
                }

            } catch (SerializationException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Exception while loading user map style preferences",
                                e);
            }
        } else {
            preferences = new HashMap<MapStylePreferenceKey, Capabilities>();
        }
    }

    private MapStylePreferenceStore() {
    }

    public synchronized Capabilities get(String perspective, String mapName) {
        MapStylePreferenceKey key = new MapStylePreferenceKey(perspective,
                mapName);

        loadFiles();

        Capabilities value = combinedPreferences.get(key);
        if (value == null) {
            value = new Capabilities();
        } else
            value = value.clone();
        return value;
    }

    public synchronized Capabilities put(String perspective, String mapName,
            Capabilities value) {
        MapStylePreferenceKey key = new MapStylePreferenceKey(perspective,
                mapName);

        value = value.clone();

        Capabilities oldValue = combinedPreferences.put(key, value);
        preferences.put(key, value);

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationFile lf = pathMgr.getLocalizationFile(pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER),
                MAPSTYLE_FILENAME);

        File file = lf.getFile();
        try {
            SerializationUtil
                    .jaxbMarshalToXmlFile(this, file.getAbsolutePath());
            lf.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Exception while storing map style preferences", e);
        }
        return oldValue;
    }
}
