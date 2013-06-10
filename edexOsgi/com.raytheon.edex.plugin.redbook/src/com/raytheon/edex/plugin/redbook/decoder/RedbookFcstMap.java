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
package com.raytheon.edex.plugin.redbook.decoder;

import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Mapping of WMO header TTAAII values to forecast hour attributes.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	-----------	-----------	--------------------------
 * 20101022            6424 kshrestha	Add fcsttime
 * Apr 29, 2013        1958 bgonzale     Map is loaded once, and then
 *                                       not loaded again unless the mapping
 *                                       file changes.
 * 
 * </pre>
 * 
 * @author kshrestha
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class RedbookFcstMap implements ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookFcstMap.class);

    private static final String REDBOOK_FCST_MAP_XML = "redbook/redbookFcstMap.xml";

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class MapFcstHr {
        @XmlElement(required = false)
        public String name;

        @XmlElement
        public String fcstHR;

        @XmlAttribute(name = "prd")
        public Integer binPeriod;

        @XmlAttribute(name = "ofs")
        public Integer binOffset;
    }

    private static RedbookFcstMap instance;

    private HashMap<String, MapFcstHr> mapping;

    private RedbookFcstMap() {
    }

    private static RedbookFcstMap load(LocalizationFile xmlFile) {
        RedbookFcstMap loadedMap = null;
        try {
            loadedMap = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    RedbookFcstMap.class, xmlFile.getFile().getAbsolutePath());
        } catch (SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
        }
        return loadedMap;
    }

    /**
     * @param key
     * @return The Map Forecast Hour attributes associated with the key; null if
     *         none found.
     */
    public MapFcstHr get(String key) {
        return this.mapping.get(key);
    }

    /**
     * Get the instance of the Map.
     * 
     * @return the instance.
     */
    public static synchronized RedbookFcstMap getInstance() {
        if (instance == null) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext ctx = pathMgr.getContext(
                    LocalizationContext.LocalizationType.EDEX_STATIC,
                    LocalizationContext.LocalizationLevel.BASE);
            final LocalizationFile xmlFile = pathMgr.getLocalizationFile(ctx,
                    REDBOOK_FCST_MAP_XML);

            instance = load(xmlFile);
            xmlFile.addFileUpdatedObserver(new ILocalizationFileObserver() {
                @Override
                public void fileUpdated(FileUpdatedMessage message) {
                    RedbookFcstMap updatedMap = load(xmlFile);
                    instance.mapping.clear();
                    instance.mapping.putAll(updatedMap.mapping);
                }
            });
        }
        return instance;
    }
}
