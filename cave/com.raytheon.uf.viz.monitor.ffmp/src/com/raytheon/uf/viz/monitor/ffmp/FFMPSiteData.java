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

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * A data container that holds the site's FFMPSourceData for each source.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2013            njensen     Initial creation
 * Feb 28, 2013  1729      dhladky     Sped up, synch blocks were hanging it.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FFMPSiteData {

    private ConcurrentMap<String, FFMPSourceData> sourceMap = new ConcurrentHashMap<String, FFMPSourceData>();

    /**
     * Gets the data of the specified source
     * 
     * @param source
     * @return
     */
    public FFMPSourceData getSourceData(String source) {

        FFMPSourceData sourceData = sourceMap.get(source);

        if (sourceData == null) {
            sourceData = new FFMPSourceData();
            FFMPSourceData previous = sourceMap.putIfAbsent(source, sourceData);
            if (previous != null) {
                return previous;
            }
        }

        return sourceData;
    }

    /**
     * Clears all the data for the site
     */
    public void clear() {
        for (FFMPSourceData source : sourceMap.values()) {
            source.clear();
        }
    }

    /**
     * Checks if there is a data object for the source
     * 
     * @param source
     * @return
     */
    public boolean containsSource(String source) {
        return sourceMap.containsKey(source);
    }

    /**
     * Gets the sources that have data objects
     * 
     * @return
     */
    public Set<String> getSources() {
        return sourceMap.keySet();
    }

}
