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
 * Aug 9,  2016  19243  JingtaoD    CrestHistory dialog should allow insert either Flow or Stage data
 * 
 * </pre>
 *
 * @author dhladky
 * @version 1.0
 */

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.RiverDataPoint;
import com.raytheon.viz.hydrocommon.datamanager.RiverDataManager;
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
     * @return Error Message, or null if no errors
     * @throws VizException
     */
    public String insertUpdateCrest(CrestData cd, String lid) {
        String errMsg = null;

        AppsDefaults ad = AppsDefaults.getInstance();
        boolean debug = ad.getBoolean(HydroConstants.DEBUG_HYDRO_DB_TOKEN,
                false);

        if (checkData(cd, lid) == 0) {
            // Insert new record
            StringBuffer insertQuery = new StringBuffer();
            insertQuery
                    .append("insert into crest (lid, datcrst, cremark, hw, jam, olddatum, q, "
                            + "stage, suppress, timcrst, prelim) values (");

            insertQuery.append("'").append(lid).append("', ");
            insertQuery.append("'").append(cd.getDateString()).append("', ");

            if (cd.getRemarks() != null) {
                String remarks = cd.getRemarks();
                remarks = remarks.replaceAll("\n", "\\n");
                remarks = DbUtils.escapeSpecialCharforStr(remarks);
                insertQuery.append("'").append(remarks).append("', ");
            } else {
                insertQuery.append("null").append(", ");
            }

            if (cd.isHighWater()) {
                insertQuery.append("'").append(insertmark).append("', ");
            } else {
                insertQuery.append("null").append(", ");
            }
            if (cd.isIce()) {
                insertQuery.append("'").append(insertmark).append("', ");
            } else {
                insertQuery.append("null").append(", ");
            }
            if (cd.isOldDatum()) {
                insertQuery.append("'").append(insertmark).append("', ");
            } else {
                insertQuery.append("null").append(", ");
            }

            if (cd.getFlow() != HydroConstants.MISSING_VALUE) {
                insertQuery.append(cd.getFlow()).append(", ");
            } else {
                insertQuery.append("null").append(", ");
            }

            if (cd.getStage() != HydroConstants.MISSING_VALUE) {
                insertQuery.append(cd.getStage()).append(", ");
            } else {
                insertQuery.append("null").append(", ");
            }

            if (cd.isSuppress()) {
                insertQuery.append("'").append(insertmark).append("', ");
            } else {
                insertQuery.append("null").append(", ");
            }

            insertQuery.append("'").append(cd.getTimeString()).append("', ");
            insertQuery.append("'").append(cd.getPrelim()).append("')");

            if (debug) {
                System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                        + ad.getToken(HydroConstants.PGPORT) + ":"
                        + ad.getToken(HydroConstants.DB_NAME));
                System.out.println("Query: " + insertQuery.toString());
            }

            try {
                DirectDbQuery.executeStatement(insertQuery.toString(),
                        HydroConstants.IHFS, QueryLanguage.SQL);

            } catch (VizException e) {
                errMsg = "Error inserting data in database";
                statusHandler.handle(
                        Priority.ERROR,
                        "Error inserting crest data with query: "
                                + insertQuery.toString(), e);
            }
        } else {
            /* execute an update */
            StringBuilder updateQuery = new StringBuilder("update crest set ");

            if (cd.getRemarks() != null) {
                String remarks = cd.getRemarks();
                remarks = remarks.replace("\n", "\\n");
                remarks = DbUtils.escapeSpecialCharforStr(remarks);
                updateQuery.append("cremark = E'").append(remarks)
                        .append("', ");
            }

            if (cd.isHighWater()) {
                updateQuery.append("hw = '").append(insertmark).append("', ");
            } else {
                updateQuery.append("hw = null, ");
            }

            if (cd.isIce()) {
                updateQuery.append("jam = '").append(insertmark).append("', ");
            } else {
                updateQuery.append("jam = null, ");
            }

            if (cd.isOldDatum()) {
                updateQuery.append("olddatum = '").append(insertmark)
                        .append("', ");
            } else {
                updateQuery.append("olddatum = null, ");
            }

            if (cd.getFlow() == HydroConstants.MISSING_VALUE) {
                updateQuery.append("q = ").append("null").append(", ");
            } else {
                updateQuery.append("q = ").append(cd.getFlow()).append(", ");
            }

            if (cd.getStage() == HydroConstants.MISSING_VALUE) {
                updateQuery.append("stage = ").append("null").append(", ");
            } else {
                updateQuery.append("stage = ").append(cd.getStage())
                        .append(", ");
            }

            if (cd.isSuppress()) {
                updateQuery.append("suppress = '").append(insertmark)
                        .append("', ");
            } else {
                updateQuery.append("suppress = null, ");
            }

            updateQuery.append("prelim = '").append(cd.getPrelim())
                    .append("' ");
            updateQuery.append(" where lid = '").append(lid).append("' ");
            updateQuery.append("and datcrst = '").append(cd.getDateString())
                    .append("' ");
            updateQuery.append("and timcrst = '").append(cd.getTimeString())
                    .append("' ");

            if (debug) {
                System.out.println(ad.getToken(HydroConstants.PGHOST) + ":"
                        + ad.getToken(HydroConstants.PGPORT) + ":"
                        + ad.getToken(HydroConstants.DB_NAME));
                System.out.println("Query: " + updateQuery.toString());
            }

            try {
                DirectDbQuery.executeStatement(updateQuery.toString(),
                        HydroConstants.IHFS, QueryLanguage.SQL);
            } catch (VizException e) {
                errMsg = "Error updating data in database";
                statusHandler.handle(
                        Priority.ERROR,
                        "Error updating crest data with query: "
                                + updateQuery.toString(), e);
            }
        }

        return errMsg;
    }

    public int checkData(CrestData cd, String lid) {
        int rval = 0;

        String query = "select lid from crest where lid = '" + lid
                + "' and datcrst = '" + cd.getDateString()
                + "' and timcrst = '" + cd.getTimeString() + "'";
        try {
            rval = DirectDbQuery.executeMappedQuery(query.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL).getResultCount();
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error checking data for crest query:" + query, e);
        }

        return rval;

    }
}
