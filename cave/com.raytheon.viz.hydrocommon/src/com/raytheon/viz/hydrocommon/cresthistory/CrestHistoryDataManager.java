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
package com.raytheon.viz.hydrocommon.cresthistory;

/**
 * Another Data manager
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date        Ticket#     Engineer Description
 * ------------   ----------  ----------- --------------------------
 * Dec 11, 2008   1628  dhladky  initial
 * Nov 04, 2010   5518	lbousaid	added all/above/bellow flag to
 * 									getRiverCrestData
 * Jan 09, 2015  16698  JingtaoD    Crest History failed validation dialog pops up when OK button clicked
 * 
 * </pre>
 *
 * @author dhladky
 * @version 1.0
 */

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.RiverDataPoint;
import com.raytheon.viz.hydrocommon.datamanager.RiverDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.DbUtils;

public class CrestHistoryDataManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrestHistoryDataManager.class);

    /** Singleton instance of this class */
    private static CrestHistoryDataManager crestDataManager = null;

    private static String insertmark = "X";

    /* Private Constructor */
    private CrestHistoryDataManager() {
    }

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static synchronized CrestHistoryDataManager getInstance() {
        if (crestDataManager == null) {
            crestDataManager = new CrestHistoryDataManager();
        }
        return crestDataManager;
    }

    /**
     * Gets the crest data
     * 
     * @param rdp
     * @return
     */
    public CrestHistoryData getRiverCrestData(String lid, boolean control,
            int allFlag) {

        RiverDataManager rdm = RiverDataManager.getInstance();
        RiverDataPoint rdp = rdm.getRiverDataPoint(lid);
        // get crest data
        return rdm.getRiverCrests(rdp, allFlag);
    }

    /**
     * Delete a Crest
     * 
     * @param cd
     */
    public void deleteCrest(CrestData cd, String lid) {
        String deleteCrest = "delete from crest where lid = '" + lid
                + "' and datcrst = '" + cd.getDateString() + "' and timcrst ='"
                + cd.getTimeString() + "'";
        try {
            AppsDefaults ad = AppsDefaults.getInstance();
            boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                    false);
            if (debug) {
                System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                        + ad.getToken(HydroConstants.PGPORT) + ":"
                        + ad.getToken(HydroConstants.DB_NAME));
                System.out.println("Query: " + deleteCrest);
            }

            DirectDbQuery.executeStatement(deleteCrest, HydroConstants.IHFS,
                    QueryLanguage.SQL);
        } catch (VizException ve) {
            statusHandler.error("Error deleting the crest: " + lid, ve);
        }
    }

    /**
     * Insert/update a crest.
     * 
     * @param cd
     *            The CrestData object holding data for database
     * @param lid
     *            The current lid
     * @param selectedCrest
     *            The crest data currently in the database
     * @param mode
     *            The mode, 1 = new, 2 = delete, 3 = update
     * @return Error Message, or null if no errors
     * @throws VizException
     */
    public String insertCrest(CrestData cd, String lid,
            CrestData selectedCrest, int mode) {
        String errMsg = null;

        DbUtils.escapeSpecialCharforData(cd);

        if (mode == 3) {

            // Did the primary key change?
            if ((selectedCrest != null)
                    && (cd.getDateString()
                            .equals(selectedCrest.getDateString()) && cd
                            .getTimeString().equals(
                                    selectedCrest.getTimeString()))) {
                // The PK is different, delete the record, then insert
                String query = "delete from crest where lid = '" + lid
                        + "' and datcrst = '" + selectedCrest.getDateString()
                        + "' and timcrst = '" + selectedCrest.getTimeString()
                        + "'";

                try {
                    AppsDefaults ad = AppsDefaults.getInstance();
                    boolean debug = ad.getBoolean(
                            HydroConstants.DEBUG_HYDRO_DB_TOKEN, false);
                    if (debug) {
                        System.out.println(ad.getToken(HydroConstants.PGHOST)
                                + ":" + ad.getToken(HydroConstants.PGPORT)
                                + ":" + ad.getToken(HydroConstants.DB_NAME));
                        System.out.println("Query: " + query);
                    }

                    DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                            QueryLanguage.SQL);
                } catch (VizException e) {
                    e.printStackTrace();
                }
            }
        }
        // Insert new record
        StringBuffer front = new StringBuffer();
        StringBuffer back = new StringBuffer();
        front.append("insert into crest (lid, datcrst, ");
        back.append(lid + "', '" + cd.getDateString() + "', ");

        if (cd.getRemarks() != null) {
            String remarks = cd.getRemarks();
            remarks = remarks.replaceAll("\n", "\\n");
            back.append("'" + remarks + "', ");
            front.append("cremark, ");
            // back.append("'" + cd.getRemarks() + "', ");
        }
        if (cd.isHighWater()) {
            front.append("hw, ");
            back.append("'" + insertmark + "', ");
        }
        if (cd.isIce()) {
            front.append("jam, ");
            back.append("'" + insertmark + "', ");
        }
        if (cd.isOldDatum()) {
            front.append("olddatum, ");
            back.append("'" + insertmark + "', ");
        }

        front.append("q, stage, ");
        back.append(cd.getFlow() + ", " + cd.getStage() + ", ");

        if (cd.isSuppress()) {
            front.append("suppress, ");
            back.append("'" + insertmark + "', ");
        }
        front.append("timcrst, prelim) values ('");
        back.append(" '" + cd.getTimeString() + "', '" + cd.getPrelim() + "')");

        String insertCrest = front.toString() + back.toString();

        boolean executeUpdate = false;
        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);
        if (debug) {
            System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                    + ad.getToken(HydroConstants.PGPORT) + ":"
                    + ad.getToken(HydroConstants.DB_NAME));
            System.out.println("Query: " + insertCrest);
        }

        try {

            DirectDbQuery.executeStatement(insertCrest, HydroConstants.IHFS,
                    QueryLanguage.SQL);
        } catch (VizException e) {

            e.printStackTrace();

            // exception with duplicate key value is throwed in the 2nd cause

            if (e.getCause().getCause().getMessage().contains("crest_pk")) {
                executeUpdate = true;
            } else {
                errMsg = "Error inserting data into database.";
            }
        }

        if (executeUpdate) {
            /* execute an update */
            StringBuilder query = new StringBuilder("update crest set ");

            // query.append("lid = '" + lid + "', ");
            // query.append("datcrst = '" + cd.getDateString() + "', ");

            if (cd.getRemarks() != null) {
                String remarks = cd.getRemarks();
                remarks = remarks.replace("\n", "\\n");
                query.append("cremark = E'" + remarks + "', ");
                // query.append("cremark = '" + cd.getRemarks() + "', ");
            }

            if (cd.isHighWater()) {
                query.append("hw = '" + insertmark + "', ");
            } else {
                query.append("hw = '', ");
            }

            if (cd.isIce()) {
                query.append("jam = '" + insertmark + "', ");
            } else {
                query.append("jam = '', ");
            }

            if (cd.isOldDatum()) {
                query.append("olddatum = '" + insertmark + "', ");
            } else {
                query.append("olddatum = '', ");
            }

            query.append("q = " + cd.getFlow() + ", ");
            query.append("stage = " + cd.getStage() + ", ");

            if (cd.isSuppress()) {
                query.append("suppress = '" + insertmark + "', ");
            } else {
                query.append("suppress = '', ");
            }

            query.append("prelim = '" + cd.getPrelim() + "' ");
            query.append(" where lid = '" + lid + "' ");
            query.append("and datcrst = '" + cd.getDateString() + "' ");
            query.append("and timcrst = '" + cd.getTimeString() + "' ");
            try {
                if (debug) {
                    System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                            + ad.getToken(HydroConstants.PGPORT) + ":"
                            + ad.getToken(HydroConstants.DB_NAME));
                    System.out.println("Query: " + query.toString());
                }

                DirectDbQuery.executeStatement(query.toString(),
                        HydroConstants.IHFS, QueryLanguage.SQL);
            } catch (VizException e) {
                errMsg = "Error updating data in database";
                e.printStackTrace();
            }
        }

        return errMsg;
    }
}
