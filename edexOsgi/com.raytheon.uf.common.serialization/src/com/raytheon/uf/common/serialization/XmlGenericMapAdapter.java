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
package com.raytheon.uf.common.serialization;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * Handles the conversion between Java {@link Map} classes to Jaxb usable
 * {@link MapType} classes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012 955        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class XmlGenericMapAdapter<K, V> extends
        XmlAdapter<MapType<K, V>, Map<K, V>> {

    @Override
    public Map<K, V> unmarshal(MapType<K, V> serialized) throws Exception {
        HashMap<K, V> map = new HashMap<K, V>();

        for (MapEntryType<K, V> mapEntryType : serialized.getEntry()) {
            map.put(mapEntryType.getKey(), mapEntryType.getValue());
        }
        return map;
    }

    @Override
    public MapType<K, V> marshal(Map<K, V> unserialized) throws Exception {
        MapType<K, V> mapType = new MapType<K, V>();

        for (Map.Entry<K, V> entry : unserialized.entrySet()) {
            MapEntryType<K, V> mapEntryType = new MapEntryType<K, V>();
            mapEntryType.setKey(entry.getKey());
            mapEntryType.setValue(entry.getValue());
            mapType.getEntry().add(mapEntryType);
        }
        return mapType;
    }
}
