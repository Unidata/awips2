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
package com.raytheon.viz.aviation.monitor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * This class coordinates getting the Grid data for the sites being monitored by
 * AvnFPS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 6, 2013 1735       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class SiteGridManager {

    /** Singleton instance of class used for synchronization. */
    private static final SiteGridManager instance = new SiteGridManager();

    /** Time out for performing a reset in milliseconds */
    private static final long RESET_TIME = 5L * TimeUtil.MILLIS_PER_MINUTE;

    /** The list of sites that need grid information. */
    private final List<String> siteIDs = new ArrayList<String>();

    /** Python pickle of the grid information for sites */
    private final Map<String, String> dataMap = new HashMap<String, String>();

    /** The time value for values in dataMap. */
    private long timeSeconds = 0L;

    /** When true grid maps are being retrieved. */
    private boolean gettingData = false;

    /** Listeners to notified when the grid data has arrived. */
    private final List<IGridDataRetrieveListener> listeners = new ArrayList<IGridDataRetrieveListener>();

    /**
     * Timer to force retrieval of grid data. Currently no way to notify if the
     * grid data for a given time has been updated.
     */
    private Job resetJob;

    /**
     * Add sites to list of sites needing grid data.
     * 
     * @param siteIDs
     */
    public static void addSiteIDs(List<String> siteIDs) {
        synchronized (instance) {
            instance.siteIDs.addAll(siteIDs);
        }
    }

    /**
     * The list of sites needing grid information.
     * 
     * @return siteIDs
     */
    public static List<String> getSiteIDs() {
        synchronized (instance) {
            return new ArrayList<String>(instance.siteIDs);
        }
    }

    /**
     * Clear data Maps sites and time.
     */
    public static void clear() {
        synchronized (instance) {
            instance.siteIDs.clear();
            instance.dataMap.clear();
            instance.timeSeconds = 0L;
            instance.gettingData = false;
            instance.resetJob.cancel();
        }
    }

    /**
     * Used by python to set all grid data for siteIDs and notify listeners that
     * grid data has arrived.
     * 
     * @param dataMap
     */
    public static void setContainersMap(Map<String, String> dataMap) {
        synchronized (instance) {
            instance.dataMap.clear();
            instance.dataMap.putAll(dataMap);
            instance.gettingData = false;
        }
        instance.resetJob.cancel();
        instance.resetJob.schedule(RESET_TIME);
        notifyListeners();
    }

    /**
     * Used by python to determine if grid data must be retrieved. This allows
     * only one Job to retrieve the data.
     * 
     * @param timeSeconds
     *            - The time for the grid data.
     * @return true retrieve grid data otherwise false
     */
    public static boolean needData(long timeSeconds) {
        boolean state = false;
        synchronized (instance) {
            if (!instance.gettingData && instance.timeSeconds != timeSeconds) {
                state = true;
                instance.timeSeconds = timeSeconds;
                instance.dataMap.clear();
                instance.gettingData = true;
            }
        }
        return state;
    }

    /**
     * Add listener to be notified when grid data is retrieved.
     * 
     * @param listener
     */
    public static void addRetrieveDataListener(
            IGridDataRetrieveListener listener) {
        synchronized (instance) {
            instance.listeners.add(listener);
        }
    }

    /**
     * Remove the listener.
     * 
     * @param listener
     */
    public static void removeRetrieveDataListener(
            IGridDataRetrieveListener listener) {
        synchronized (instance) {
            instance.listeners.remove(listener);
        }
    }

    /**
     * Inform listeners that grid data has arrived.
     */
    private static void notifyListeners() {
        List<IGridDataRetrieveListener> listeners = null;
        synchronized (instance) {
            listeners = new ArrayList<IGridDataRetrieveListener>(
                    instance.listeners);
        }

        for (IGridDataRetrieveListener listener : listeners) {
            listener.gridDataRetrieved();
        }
    }

    /**
     * Get the python data for the desired site and time. If data not yet
     * retrieved null is returned.
     * 
     * @param siteID
     * @param timeSeconds
     * @return data
     */
    public static String getData(String siteID, long timeSeconds) {
        synchronized (instance) {
            return instance.dataMap.get(siteID);
        }
    }

    /**
     * Private constructor to allow only the one instance.
     */
    private SiteGridManager() {
        resetJob = new Job("SiteGridManger reset") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                SiteGridManager.this.reset();
                return Status.OK_STATUS;
            }
        };
        resetJob.setSystem(true);
    }

    /**
     * Perform reset so the next data request will query for the grid data.
     */
    private synchronized void reset() {
        dataMap.clear();
        timeSeconds = 0L;
        gettingData = false;
    }
}
