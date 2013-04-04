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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentNavigableMap;
import java.util.concurrent.ConcurrentSkipListMap;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;

/**
 * A data container that holds FFMPRecords, the previousUriQueryDate, and the
 * available and loaded URIs for the particular source associated with a site.
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

public class FFMPSourceData {

    private FFMPRecord ffmpData;

    /** earliest available date queried **/
    private Date previousUriQueryDate;

    private ConcurrentNavigableMap<Date, List<String>> availableUris = new ConcurrentSkipListMap<Date, List<String>>();

    /** map of huc to list of loaded URIs **/
    private ConcurrentMap<String, List<String>> loadedUris = new ConcurrentHashMap<String, List<String>>();

    /**
     * Clears the data
     */
    public void clear() {
        ffmpData = null;
        previousUriQueryDate = null;
        availableUris.clear();
        loadedUris.clear();
    }

    /**
     * Gets the FFMPRecord. Possibly null.
     * 
     * @return
     */
    public FFMPRecord getRecord() {
        return ffmpData;
    }

    /**
     * Sets the FFMPRecord.
     * 
     * @param record
     */
    public void setRecord(FFMPRecord record) {
        ffmpData = record;
    }

    /**
     * Gets the previous time that URIs were queried for for this site and
     * source.
     * 
     * @return
     */
    public Date getPreviousUriQueryDate() {
        return previousUriQueryDate;
    }

    /**
     * Sets the previous time that URIs were queried for for this site and
     * source.
     * 
     * @param previousUriQueryDate
     */
    public void setPreviousUriQueryDate(Date previousUriQueryDate) {
        this.previousUriQueryDate = previousUriQueryDate;
    }

    /**
     * Gets the URIs associated with a HUC that have been loaded.
     * 
     * @param huc
     * @return
     */
    public List<String> getLoadedUris(String huc) {

        List<String> loaded = loadedUris.get(huc);

        if (loaded == null) {
            loaded = new ArrayList<String>();
            List<String> previous = loadedUris.putIfAbsent(huc, loaded);
            if (previous != null) {
                return previous;
            }
        }

        return loaded;
    }

    /**
     * Tracks a URI associated with a HUC as loaded.
     * 
     * @param huc
     * @param uri
     */
    public void addLoadedUri(String huc, String uri) {

        List<String> uriList = loadedUris.get(huc);

        if (uriList == null) {
            uriList = new ArrayList<String>();
            List<String> previous = loadedUris.putIfAbsent(huc, uriList);
            if (previous != null) {
                uriList = previous;
            }
        }

        uriList.add(uri);
    }

    /**
     * Checks if this site and source has loaded any URIs yet.
     * 
     * @return
     */
    public boolean hasLoadedAnyUris() {
        return !loadedUris.isEmpty();
    }

    /**
     * Gets the set of HUCs that have loaded some URIs.
     * 
     * @return
     */
    public Set<String> getLoadedHucs() {
        return loadedUris.keySet();
    }

    /**
     * Gets the Available URIs based on time.
     * 
     * @return
     */
    public ConcurrentNavigableMap<Date, List<String>> getAvailableUris() {
        return availableUris;
    }

}
