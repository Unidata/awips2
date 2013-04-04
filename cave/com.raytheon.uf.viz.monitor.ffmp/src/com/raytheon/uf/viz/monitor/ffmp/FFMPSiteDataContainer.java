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
package com.raytheon.uf.viz.monitor.ffmp;

import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * FFMP data container that holds the FFMPSiteData for each site.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2013            njensen     Initial creation
 * Feb 28, 2013  1729      dhladky     Sped up, synch blocks were hanging it.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FFMPSiteDataContainer {

    private ConcurrentMap<String, FFMPSiteData> siteDataMap = new ConcurrentHashMap<String, FFMPSiteData>();

    public FFMPSiteData get(String siteKey) {

        FFMPSiteData data = siteDataMap.get(siteKey);

        if (data == null) {
            data = new FFMPSiteData();
            FFMPSiteData previous = siteDataMap.putIfAbsent(siteKey, data);
            if (previous != null) {
                return previous;
            }
        }

        return data;
    }

    public void clear() {
        Collection<FFMPSiteData> vals = siteDataMap.values();
        for (FFMPSiteData data : vals) {
            data.clear();
        }

        siteDataMap.clear();
    }

    public FFMPSiteData removeSite(String siteKey) {
   
        return siteDataMap.remove(siteKey);
    }

    public boolean containsSite(String siteKey) {
        return siteDataMap.containsKey(siteKey);
    }

}
