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

package com.raytheon.edex.plugin.gfe.cache.gridlocations;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Container object for holding GridLocation objects
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/08/09      1674       bphillip    Initial creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GridLocationCache {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridLocationCache.class);

    private static Map<String, GridLocationCache> instances = new HashMap<String, GridLocationCache>();

    /** The cached GridLocations */
    private Map<String, GridLocation> gridLocationMap;

    public static synchronized GridLocationCache getInstance(String siteId) {
        GridLocationCache rval = instances.get(siteId);
        if (rval == null) {
            rval = new GridLocationCache();
            instances.put(siteId, rval);
        }
        return rval;
    }

    /**
     * Constructs a new GridLocationCache
     */
    private GridLocationCache() {
        gridLocationMap = new HashMap<String, GridLocation>();
    }

    /**
     * Retrieves a gridLocation from the cache
     * 
     * @param modelName
     *            The model name of the gridLocation to retrieve
     * @return The GridLocation
     */
    public GridLocation getGridLocation(String modelName) {
        synchronized (gridLocationMap) {
            return gridLocationMap.get(modelName);
        }
    }

    /**
     * Retrieves a gridLocation from the cache
     * 
     * @param dbId
     *            The DatabaseID containing the model name of the GridLocation
     *            to retrieve
     * @return The GridLocation associated with the provided DatabaseID
     */
    public GridLocation getGridLocation(DatabaseID dbId) {
        return getGridLocation(dbId.getModelName());
    }

    /**
     * Adds a new GridLocation to the cache
     * 
     * @param modelName
     *            The model name of the gridLocation
     * @param gridLocation
     *            The GridLocation object
     */
    public void addGridLocation(String modelName, GridLocation gridLocation) {
        synchronized (gridLocationMap) {
            gridLocationMap.put(modelName, gridLocation);
        }
    }

    /**
     * Removes the grid location information for the specified site
     * 
     * @param siteID
     *            The site ID for which to remove the grid location data
     */
    public static void removeGridLocationsForSite(String siteID) {
        statusHandler.handle(Priority.EVENTA, "Purging " + siteID
                + " GridLocations from GridLocation cache...");
        if (UFStatus.getHandler().isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.handle(
                    Priority.DEBUG,
                    "\nGridLocationCache keys before purge:\n"
                            + instances.keySet());
        }

        instances.remove(siteID);

        if (UFStatus.getHandler().isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.handle(
                    Priority.DEBUG,
                    "\nGridLocationCache keys after purge:\n"
                            + instances.keySet());
        }
    }
}
