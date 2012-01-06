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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.plugin.gfe.exception.GfeConfigurationException;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 15, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class HLSTopoDatabaseManager {

    private static Map<String, HLSTopoDatabase> hlsTopoDbMap = new HashMap<String, HLSTopoDatabase>();

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private HLSTopoDatabaseManager() {
        throw new AssertionError();
    }

    /**
     * Initialized the HLS topo database for the given site ID.
     * 
     * @param siteID
     *            The site for which the HLS topo database needs to be
     *            initialized.
     * @throws GfeException
     */
    public static void initializeHLSTopoDatabase(String siteID)
            throws GfeException {

        for (String source : getHLSTopoSources()) {
            HLSTopoDatabase database = new HLSTopoDatabase(siteID, source);
            hlsTopoDbMap.put(siteID + " " + source, database);
        }
    }

    /**
     * Return the HLS topo database associated with the specified site ID and
     * data source.
     * 
     * @param siteID
     *            the site ID whose HLS topo database is to be retrieved.
     * @param source
     *            the database source ("NED", for example)
     * @return The HLSTopo database associated with siteID, or null if the HLS
     *         topo database for that site has not been initialized.
     */
    public static HLSTopoDatabase getHLSTopoDatabase(String siteID,
            String source) {
        return hlsTopoDbMap.get(siteID + " " + source);
    }

    /**
     * Remove the HLS topo database for siteID. After calling this method, calls
     * to getHLSTopoDatabase for the site will return null until the HLS topo
     * database for the site is re-initialized.
     * 
     * @param siteID
     *            The site whose HLS topo database should be removed from the
     *            manager.
     */
    public static void removeHLSTopoDatabase(String siteID, String source) {
        hlsTopoDbMap.remove(siteID + " " + source);
    }

    /**
     * Get a database ID for the HLS topo database for the specified siteID and
     * source.
     * 
     * @param siteID
     *            The site for which to obtain a HLS topo database ID.
     * @param source
     *            The data source for which to obtain a HLS topo database ID.
     * @return a DatabaseID for the HLS topo database
     * @throws GfeConfigurationException
     *             if the database ID cannot be created.
     */
    public static DatabaseID getHLSTopoDbId(String siteID, String source) {
        return new DatabaseID(siteID, DataType.GRID, "D2D", source);
    }

    /**
     * @return
     */
    public static List<String> getHLSTopoSources() {
        // TODO: read from properties file?
        List<String> sources = Arrays.asList(new String[] { "CRMTopo", "NED" });
        return sources;
    }

    /**
     * @param siteID
     * @return
     */
    public static List<DatabaseID> getHLSTopoDatabases(String siteID) {
        List<DatabaseID> dbs = new ArrayList<DatabaseID>();
        for (String source : getHLSTopoSources()) {
            dbs.add(getHLSTopoDbId(siteID, source));
        }
        return dbs;
    }
}
