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

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
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

    private static final String MAPSTYLE_FILENAME = "mapstylepreferences.xml";

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

    public static MapStylePreferenceStore load() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationFile siteLf = pathMgr.getLocalizationFile(pathMgr
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.SITE), MAPSTYLE_FILENAME);

        LocalizationFile userLf = pathMgr.getLocalizationFile(pathMgr
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.USER), MAPSTYLE_FILENAME);

        MapStylePreferenceStore store = new MapStylePreferenceStore();
        if (siteLf.exists()) {
            try {
                store.combinedPreferences = ((MapStylePreferenceStore) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(siteLf.getFile())).preferences;
            } catch (SerializationException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Exception while loading site map style preferences",
                                e);
            }
        } else {
            store.combinedPreferences = new HashMap<MapStylePreferenceKey, Capabilities>();
        }

        if (userLf.exists()) {
            try {
                store.preferences = ((MapStylePreferenceStore) SerializationUtil
                        .jaxbUnmarshalFromXmlFile(userLf.getFile())).preferences;

                // merge user into site
                for (Entry<MapStylePreferenceKey, Capabilities> entry : store.preferences
                        .entrySet()) {
                    store.combinedPreferences.put(entry.getKey(),
                            entry.getValue());
                }

            } catch (SerializationException e) {
                statusHandler
                        .handle(Priority.PROBLEM,
                                "Exception while loading user map style preferences",
                                e);
            }
        } else {
            store.preferences = new HashMap<MapStylePreferenceKey, Capabilities>();
        }

        return store;
    }

    private MapStylePreferenceStore() {
    }

    public Capabilities get(String perspective, String mapName) {
        MapStylePreferenceKey key = new MapStylePreferenceKey(perspective,
                mapName);

        Capabilities value = combinedPreferences.get(key);
        if (value == null) {
            value = new Capabilities();
        }
        return value;
    }

    public Capabilities put(String perspective, String mapName,
            Capabilities value) {
        MapStylePreferenceKey key = new MapStylePreferenceKey(perspective,
                mapName);

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
