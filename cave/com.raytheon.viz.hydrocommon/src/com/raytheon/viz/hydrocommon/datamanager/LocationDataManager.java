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
package com.raytheon.viz.hydrocommon.datamanager;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Class for managing database query calls. LocationDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008 1697       askripsky   Initial Creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class LocationDataManager extends HydroDataManager {
    protected static LocationDataManager manager = null;

    private static final String SELECT_NAME_STATEMENT = "SELECT name FROM location";

    /**
     * Private constructor.
     */
    protected LocationDataManager() {
        super();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized LocationDataManager getInstance() {
        if (manager == null) {
            manager = new LocationDataManager();
        }

        return (LocationDataManager) manager;
    }

    /**
     * Returns the name corresponding to the location id.
     * 
     * @param lid
     * @return
     * @throws VizException
     */
    public String getLocationName(String lid) throws VizException {
        QueryResult result = runMappedQuery(SELECT_NAME_STATEMENT
                + " WHERE lid='" + lid + "'");

        return (result.getResultCount() > 0) ? (String) result.getRows()[0]
                .getColumn(result.getColumnNames().get("name")) : "";
    }
}
