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
package com.raytheon.edex.plugin.gfe.server.database;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;

/**
 * Database manager for handling instances of D2DSatDatabases
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class D2DSatDatabaseManager {

    /** Map of D2DSatDatabases based on site */
    private static Map<String, D2DSatDatabase> satDbMap = new HashMap<String, D2DSatDatabase>();

    /**
     * Initializes the D2DSatDatabase using the given siteID and configuration
     * 
     * @param siteID
     *            The siteID to initialize a new D2DSatDatabase for
     * @param config
     *            The configuration
     */
    public static void initializeD2DSatDatabase(String siteID,
            IFPServerConfig config) {
        List<String> dirNames = new ArrayList<String>();
        List<String> parmNames = new ArrayList<String>();
        Map<String, String> satDirs = config.satDirs();
        if (satDirs != null) {
            for (String dirName : satDirs.keySet()) {
                dirNames.add(dirName);
                parmNames.add(satDirs.get(dirName));
            }
        }
        D2DSatDatabase db = new D2DSatDatabase(config, dirNames, parmNames);
        satDbMap.put(siteID, db);
    }

    /**
     * Retrieves the D2DSatDatabase instance for the given site
     * 
     * @param siteID
     *            The site id for which to get the D2DSatDatabase
     * @return The D2DSatDatabase instance
     */
    public static D2DSatDatabase getSatDatabase(String siteID) {
        return satDbMap.get(siteID);
    }

    /**
     * Removes a site's D2DSatDatabase
     * 
     * @param siteID
     *            The site to remove the D2DSatDatabase for
     */
    public static void removeSatDatabase(String siteID) {
        satDbMap.remove(siteID);
    }

    /**
     * Gets the D2DSatDatabase id for the given site
     * 
     * @param siteID
     *            the site to get the D2DSatDatabase id for
     * @return The D2DSatDatabase database id
     */
    public static DatabaseID getSatDbId(String siteID) {
        return satDbMap.get(siteID).getDbId();
    }
}
