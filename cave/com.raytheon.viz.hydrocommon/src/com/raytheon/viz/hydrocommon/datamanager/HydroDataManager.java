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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.Rating;
import com.raytheon.viz.hydrocommon.data.RatingShift;

/**
 * Base Data Manager which other data managers should extend.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2008 1636       askripsky   Initial Creation
 * Sep 09, 2009 2259       mpduff      Added rating shift data
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public abstract class HydroDataManager {

    protected SimpleDateFormat dbFormat = HydroConstants.DATE_FORMAT;

    protected static Map<String, RatingShift> ratingShiftMap = new HashMap<String, RatingShift>();

    protected static Map<String, Rating> ratingMap = new HashMap<String, Rating>();

    /**
     * Private constructor.
     */
    protected HydroDataManager() {
        dbFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * Returns the raw data from the database.
     * 
     * @return The raw data
     * @throws VizException
     */
    protected ArrayList<Object[]> runQuery(String dataQuery) {
        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);
        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + dataQuery);
        }
        ArrayList<Object[]> data = null;

        try {
            data = (ArrayList<Object[]>) DirectDbQuery.executeQuery(dataQuery,
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return data;
    }

    /**
     * Returns the raw data from the database in a mapped result set.
     * 
     * @return The raw data
     * @throws VizException
     * @throws VizException
     */
    public QueryResult runMappedQuery(String dataQuery) throws VizException {
        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);
        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + dataQuery);
        }
        QueryResult data = null;

        data = DirectDbQuery.executeMappedQuery(dataQuery, HydroConstants.IHFS,
                QueryLanguage.SQL);

        return data;
    }

    /**
     * Executes statement with no return value on the database.
     * 
     * @throws VizException
     */
    public void runStatement(String dataQuery) throws VizException {

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);
        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + dataQuery);
        }

        DirectDbQuery.executeStatement(dataQuery, HydroConstants.IHFS,
                QueryLanguage.SQL);

    }

    /**
     * Runs functions defined in the DB
     * 
     * @param functionCall
     * @throws VizException
     */
    public ArrayList<Object[]> execFunction(String functionCall)
            throws VizException {
        return runQuery("SELECT " + functionCall);
    }
}
