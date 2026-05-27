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
package com.raytheon.edex.plugin.shef.database;

import com.raytheon.edex.plugin.shef.database.PostShef.Location;
import com.raytheon.edex.plugin.shef.util.ShefUtil;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Utility class to query for Hydro data used by the SHEF decoder.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2018   6991     mduff       Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class HydroDataAccessor {
    private static final IUFStatusHandler log = UFStatus
            .getHandler(HydroDataAccessor.class);

    private CoreDao dao;

    public HydroDataAccessor() {
        dao = new CoreDao(DaoConfig.forDatabase(ShefConstants.IHFS));
    }

    /**
     * Determines if the lid,pe,ts already exists and whether or not the
     * riverstatus table needs to be updated.
     * 
     * @param lid
     *            Location Id to check
     * @param pe
     *            Physical Element to check
     * @param ts
     *            Type Source to check
     * @param traceId
     *            Data's traceId
     * @return true if data are in the RiverStatus table
     */
    public boolean isUpdateRiverStatus(String lid, String pe, String ts,
            String traceId) {
        boolean rval = false;
        Object[] results = null;

        String query = "select lid from riverstatus where lid = '" + lid
                + "' and pe = '" + pe + "' and " + "ts = '" + ts + "'";

        try {
            results = dao.executeSQLQuery(query);

            if ((results != null) && (results.length > 0)) {
                rval = true;
            }

        } catch (Exception e) {
            log.error("Query = [" + query + "]");
            log.error(traceId + " - PostgresSQL error searching riverstatus",
                    e);
        }
        return rval;
    }

    /**
     * For a given location and pe code and type-source prefix, this function
     * returns the type-source code with the lowest rank in IngestFilter.
     * Alternatively, if a specific ordinal number is passed, then the Nth
     * ranking ts is returned. If no (<= 0) ordinal number (i.e. 1st, 2nd) is
     * requested, then the highest rank (1st) is returned. The type-source
     * prefix is normally given as a one-character string, R for observed data
     * and F for forecast data.
     *
     * The function argument returns a status variable indicating whether the
     * request was satisfied.
     * 
     * @param lid
     *            the location id
     * @param pe
     *            the physical element
     * @param tsPrefix
     *            Type Source prefix
     * @param ordinal
     *            The ordinal
     * @param traceId
     *            The data's traceId
     * @return The best Type Source
     */
    public String getBestTs(String lid, String pe, String tsPrefix, int ordinal,
            String traceId) {
        int count = 0;
        String tsFound = null;
        String query = "SELECT ts_rank,ts FROM ingestfilter WHERE lid = '" + lid
                + "' AND pe = '" + pe + "' AND ts like '" + tsPrefix
                + "' AND ingest = 'T' ORDER BY ts_rank, ts";
        Object[] results = null;
        try {
            /*
             * get the ingest filter entries for this location. note that the
             * retrieval is ordered so that if multiple best ranks exist, there
             * is some predicatibility for the identified best one. also note
             * that this approach ignores the duration, extremum, and probabilty
             * code.
             */
            results = dao.executeSQLQuery(query);
            Object[] row = null;
            if ((results != null) && (results.length > 0)) {
                /*
                 * if no specific ordinal number was requested, return with the
                 * highest rank.
                 */
                if (ordinal <= 0) {
                    row = (Object[]) results[0];
                    tsFound = ShefUtil.getString(row[1], null);
                } else {
                    /*
                     * get a count of the number of matching ts entries. if the
                     * requested ordinal number is greater than the number
                     * available then return with a not found status.
                     */

                    count = results.length;

                    if (ordinal <= count) {
                        row = (Object[]) results[ordinal - 1];
                        tsFound = ShefUtil.getString(row[1], null);
                    }
                }
            }
        } catch (Exception e) {
            log.error("Query = [" + query + "]");
            log.error(
                    traceId + " - PostgresSQL error retrieving from ingestfilter",
                    e);
        }
        return tsFound;
    }

    /**
     * Checks if location data should be posted. 4 possible return values:
     * Location defined as location - 0 Location defined as geoarea - 1 Location
     * defined but don't post - 2 Location undefined - 3
     *
     * @param locId
     *            - Location Id to check
     * @param traceId
     *            Data's traceId
     * @return Location corresponding to 1 of 4 return values
     */
    public Location checkLocation(String locId, String traceId) {
        Location retVal = Location.LOC_UNDEFINED;
        String sql = null;
        try {
            sql = "select lid, post from location where lid = '" + locId + "'";

            // TODO fix multiple results returned error
            Object[] results = dao.executeSQLQuery(sql);
            if (results.length > 0) {
                Object[] resultRow = (Object[]) results[0];
                int post = ShefUtil.getInt(resultRow[1], 0);

                retVal = (post == 1) ? Location.LOC_LOCATION
                        : Location.LOC_NO_POST;

            } else {
                sql = "select area_id from GeoArea where area_id = '" + locId
                        + "'";
                results = dao.executeSQLQuery(sql);
                if (results.length > 0) {
                    retVal = Location.LOC_GEOAREA;
                }
            }
        } catch (Exception e) {
            log.error("Query = [" + sql + "]");
            log.error(traceId + " - Error checking location", e);
        }

        return retVal;
    }

    /**
     * Retrieves the number of records in the table based on the where clause
     *
     * @param table
     *            - table to search
     * @param where
     *            - where clause to narrow the search
     * @return - number of records in the table
     */
    public int recordCount(String table, String where) {
        int retVal = 0;
        StringBuilder sql = new StringBuilder("Select count(*) from ")
                .append(table);
        if (where != null) {
            sql.append(where);
        }
        try {
            Object[] results = dao.executeSQLQuery(sql.toString());
            retVal = ShefUtil.getInt(results[0], 0);
        } catch (Exception e) {
            log.error("Query = [" + sql.toString() + "]");
            log.error(
                    "An error occurred in recordCount:  " + table + " - " + sql,
                    e);
        }
        return retVal;
    }

}
