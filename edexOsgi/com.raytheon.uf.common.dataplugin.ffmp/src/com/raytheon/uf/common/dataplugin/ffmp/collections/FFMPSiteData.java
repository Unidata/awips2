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
package com.raytheon.uf.common.dataplugin.ffmp.collections;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.raytheon.uf.common.monitor.xml.SourceXML;

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
 * Oct 26, 2015  5056      dhladky     Moved to common area for data cache.
 * Aug 07, 2018  6720      njensen     Updated method signature to be
 *                                     getSourceData(SourceXML)
 * Aug 14, 2018 6720       njensen     Use simplified enums
 * 
 * </pre>
 * 
 * @author njensen
 */

public class FFMPSiteData {

    private ConcurrentMap<String, FFMPSourceData> sourceMap = new ConcurrentHashMap<>();

    /**
     * Gets the data of the specified source
     * 
     * @param source
     * @return
     */
    public FFMPSourceData getSourceData(SourceXML source) {
        String cacheKey = source.getSourceName();
        if (source.isGuidance()) {
            cacheKey = source.getSourceFamily();
        }
        FFMPSourceData sourceData = sourceMap.get(cacheKey);

        if (sourceData == null) {
            sourceData = new FFMPSourceData();
            FFMPSourceData previous = sourceMap.putIfAbsent(cacheKey,
                    sourceData);
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

    @Override
    public String toString() {
        return "FFMPSiteData [sourceMap=" + sourceMap + "]";
    }

}
