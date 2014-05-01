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
import java.util.Collections;
import java.util.Date;
import java.util.List;
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
 * Jul 15, 2013  2184      dhladky     Removed all HUC's but ALL
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
    private List<String> loadedUris = new ArrayList<String>();

    /**
     * Clears the data
     */
    public void clear() {
        ffmpData = null;
        previousUriQueryDate = null;
        availableUris.clear();
        synchronized (loadedUris) {
            loadedUris.clear();
        }
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
     * Gets the URIs that have been loaded.
     * 
     * @return
     */
    public List<String> getLoadedUris() {
        return Collections.unmodifiableList(loadedUris);
    }

    /**
     * Tracks a URI as loaded.
     * 
     * @param uri
     */
    public void addLoadedUri(String uri) {
        synchronized (loadedUris) {
            loadedUris.add(uri);
        }
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
     * Gets the Available URIs based on time.
     * 
     * @return
     */
    public ConcurrentNavigableMap<Date, List<String>> getAvailableUris() {
        return availableUris;
    }

    /**
     * Removes a URI
     * 
     * @param uri
     */
    public void removeLoadedUri(String uri) {
        synchronized (loadedUris) {
            loadedUris.remove(uri);
        }
    }

}
