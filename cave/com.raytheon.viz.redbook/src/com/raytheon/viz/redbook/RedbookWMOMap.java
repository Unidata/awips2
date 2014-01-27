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
package com.raytheon.viz.redbook;

import java.io.File;
import java.util.HashMap;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Returns the redbook WMO prefix to human readable name
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 30, 2008				chammack	Initial creation
 * Nov 04, 2013 2361        njensen     Use JAXB instead of SerializationUtil
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class RedbookWMOMap {

    @XmlAccessorType(XmlAccessType.FIELD)
    public static class Info {
        public String name;

        @XmlElement(required = false)
        public String projection;
    }

    public HashMap<String, Info> mapping;

    public static RedbookWMOMap load() throws Exception {
        File file = PathManagerFactory.getPathManager().getStaticFile(
                "redbook/redbookMapping.xml");
        try {
            RedbookWMOMap map = JAXB.unmarshal(file, RedbookWMOMap.class);

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
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }
}
