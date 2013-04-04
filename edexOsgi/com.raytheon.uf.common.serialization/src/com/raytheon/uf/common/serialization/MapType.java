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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Used by Jaxb to represent a {@link Map}.
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

public class MapType<K, V> {
    private List<MapEntryType<K, V>> entry = new ArrayList<MapEntryType<K, V>>();

    public MapType() {
    }

    public MapType(Map<K, V> map) {
        for (Map.Entry<K, V> mapEntry : map.entrySet()) {
            entry.add(new MapEntryType<K, V>(mapEntry));
        }
    }

    public List<MapEntryType<K, V>> getEntry() {
        return entry;
    }

    public void setEntry(List<MapEntryType<K, V>> entry) {
        this.entry = entry;
    }
}
