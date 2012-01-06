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
package com.raytheon.viz.ghg.monitor.config;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.viz.ghg.monitor.data.GhgDataFilter;
import com.raytheon.viz.ghg.monitor.data.GhgDataFilterEntry;

/**
 * An adapter class to convert a map<String, GhgDataFilter> into an array of
 * GhgDataFilterEntry objects, which JAXB knows how to serialize.
 * 
 * @author wdougherty
 * 
 */
public class GhgDataFilterAdapter extends
        XmlAdapter<GhgDataFilterEntry[], Map<String, GhgDataFilter>> {

    @Override
    public GhgDataFilterEntry[] marshal(Map<String, GhgDataFilter> map)
            throws Exception {

        if (map == null) {
            return new GhgDataFilterEntry[0];
        }

        GhgDataFilterEntry[] result = new GhgDataFilterEntry[map.size()];

        GhgDataFilter value;
        GhgDataFilterEntry entry;
        int idx = 0;
        for (String key : map.keySet()) {
            value = map.get(key);
            entry = new GhgDataFilterEntry();
            entry.setKey(key);
            entry.setValue(value);
            result[idx++] = entry;
        }
        return result;
    }

    @Override
    public Map<String, GhgDataFilter> unmarshal(GhgDataFilterEntry[] filters)
            throws Exception {
        Map<String, GhgDataFilter> map = new HashMap<String, GhgDataFilter>(
                filters.length);
        if (filters != null) {
            for (GhgDataFilterEntry filter : filters) {
                map.put(filter.getKey(), filter.getValue());
            }
        }
        return map;
    }
}

