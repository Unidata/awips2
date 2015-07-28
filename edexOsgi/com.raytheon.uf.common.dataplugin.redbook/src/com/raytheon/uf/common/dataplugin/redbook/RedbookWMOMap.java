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
package com.raytheon.uf.common.dataplugin.redbook;

import java.util.HashMap;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Returns the redbook WMO prefix to human readable name
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 30, 2008             chammack    Initial creation
 * Nov 04, 2013 2361        njensen     Use JAXB instead of SerializationUtil
 * Jun 25, 2015 4512        mapeters    Refactored from viz.redbook, made mapping
 *                                      private, added getValue() and addEntry()
 * Jul 21, 2015 4512        mapeters    Check for redbookMapping.xml in common_static.configured,
 *                                      then cave_static.base
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class RedbookWMOMap {

    private static final String FILE_PATH = "redbook" + IPathManager.SEPARATOR
            + "redbookMapping.xml";

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Info {
        public String name;

        @XmlElement(required = false)
        public String projection;
    }

    private HashMap<String, Info> mapping = new HashMap<>();

    public Info getValue(String key) {
        return mapping.get(key);
    }

    public void addEntry(String key, Info value) {
        mapping.put(key, value);
    }

    public static RedbookWMOMap load() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        /*
         * Check common_static.configured first, as it is now being saved there.
         * If not found, check cave_static.base, where it used to be stored (in
         * the future cave_static should no longer need to be checked).
         */
        LocalizationContext context = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.CONFIGURED);
        LocalizationFile locFile = pathMgr.getLocalizationFile(context,
                FILE_PATH);
        if (locFile == null || !locFile.exists()) {
            context = pathMgr.getContext(
                    LocalizationContext.LocalizationType.CAVE_STATIC,
                    LocalizationContext.LocalizationLevel.BASE);
            locFile = pathMgr.getLocalizationFile(context, FILE_PATH);
        }

        RedbookWMOMap map = JAXB.unmarshal(locFile.getFile(),
                RedbookWMOMap.class);

        // add ability for comma separated values in xml file
        RedbookWMOMap secondMap = new RedbookWMOMap();
        secondMap.mapping = new HashMap<String, Info>();
        secondMap.mapping.putAll(map.mapping);
        for (String key : map.mapping.keySet()) {
            if (key.contains(",")) {
                String[] keys = key.split(",");
                for (String theKey : keys) {
                    secondMap.mapping.put(theKey.trim(),
                            secondMap.mapping.get(key));
                }
                secondMap.mapping.remove(key);
            }
        }
        return secondMap;
    }
}
