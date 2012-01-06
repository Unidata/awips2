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
 * Climo database manager. Based heavily on TopoDatabaseManager.
 * 
 * @author wldougher
 * 
 */
public class ClimoDatabaseManager {

    private static Map<String, ClimoDatabase> climoDbMap = new HashMap<String, ClimoDatabase>();

    /**
     * Initialize the climo database for the given site ID.
     * 
     * @param siteID
     *            The site for which the climo database needs to be initialized.
     * @throws GfeException
     */
    public static void initializeClimoDatabase(String siteID)
            throws GfeException {

        for (String source : getClimoSources()) {
            ClimoDatabase climo = new ClimoDatabase(siteID, source);
            climoDbMap.put(siteID + " " + source, climo);
        }
    }

    /**
     * Return the climo database associated with the specified site ID and data
     * source.
     * 
     * @param siteID
     *            the site ID whose climo database is to be retrieved.
     * @param source
     *            the database source ("PRISM", for example)
     * @return The climo database associated with siteID, or null if the climo
     *         database for that site has not been initialized.
     */
    public static ClimoDatabase getClimoDatabase(String siteID, String source) {
        return climoDbMap.get(siteID + " " + source);
    }

    /**
     * Remove the climo database for siteID. After calling this method, calls to
     * getClimoDatabase for the site will return null until the climo database
     * for the site is re-initialized.
     * 
     * @param siteID
     *            The site whose climo database should be removed from the
     *            manager.
     */
    public static void removeClimoDatabase(String siteID, String source) {
        climoDbMap.remove(siteID + " " + source);
    }

    /**
     * Get a database ID for climo database for siteID and source.
     * 
     * @param siteID
     *            The site for which to obtain a climo database ID.
     * @param source
     *            The data source for which to obtain a climo database ID.
     * @return a DatabaseID for the climo database
     * @throws GfeConfigurationException
     *             if the database ID cannot be created.
     */
    public static DatabaseID getClimoDbId(String siteID, String source) {
        return new DatabaseID(siteID, DataType.GRID, "D2D", source + "Climo");
    }

    /**
     * @return
     */
    public static List<String> getClimoSources() {
        // TODO: read from properties file?
        List<String> sources = Arrays.asList(new String[] { "PRISM", "NCDC" });
        return sources;
    }

    /**
     * @param siteID
     * @return
     */
    public static List<DatabaseID> getClimoDatabases(String siteID) {
        List<DatabaseID> dbs = new ArrayList<DatabaseID>();
        for (String source : getClimoSources()) {
            dbs.add(getClimoDbId(siteID, source));
        }
        return dbs;
    }

}
