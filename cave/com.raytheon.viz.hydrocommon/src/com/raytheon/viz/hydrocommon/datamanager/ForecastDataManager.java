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

import java.util.ArrayList;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.ForecastData;
import com.raytheon.viz.hydrocommon.util.DbUtils;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;

/**
 * Class for managing database query calls. RejectedDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2008 1636       askripsky   Initial Creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class ForecastDataManager extends HydroDataManager {
    protected static ForecastDataManager manager = null;

    private final String INSERT_INTO_STATEMENT = "INSERT INTO %s (lid, pe, dur, ts, extremum, probability, validtime, "
            + "basistime, value, shef_qual_code, quality_code, revision, product_id, producttime, postingtime) VALUES ('%s', "
            + "'%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')";

    private final String SELECT_STATEMENT = "SELECT lid, pe, dur, ts, extremum, value, shef_qual_code, quality_code, revision, product_id, producttime, postingtime, validtime FROM %s";

    private final String UPDATE_STATEMENT = "UPDATE %s SET value='%s', shef_qual_code='%s', quality_code='%s', revision='%s', product_id='%s', producttime='%s', postingtime='%s' WHERE %s";

    private final String DELETE_STATEMENT = "DELETE FROM %s WHERE %s";

    /**
     * Private constructor.
     */
    protected ForecastDataManager() {
        super();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized ForecastDataManager getInstance() {
        if (manager == null) {
            manager = new ForecastDataManager();
        }

        return (ForecastDataManager) manager;
    }

    /**
     * Deletes each record passed in from some forecast tables.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecords(ArrayList<ForecastData> recordsToDelete)
            throws VizException {
        for (ForecastData currData : recordsToDelete) {
            deleteRecord(currData);
        }
    }

    /**
     * Deletes each record passed in from some forecast tables.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecord(ForecastData recordToDelete) throws VizException {
        runStatement(String.format(DELETE_STATEMENT, DbUtils.getTableName(
                recordToDelete.getPe(), recordToDelete.getTs()), HydroDataUtils
                .getPKStatement(recordToDelete)));
    }

    /**
     * Checks to see if the record exists by checking by PK, should only return
     * 0 or 1 records.
     * 
     * @param currData
     * @return
     * @throws VizException
     */
    public ArrayList<Object[]> getForecastData(ForecastData currData)
            throws VizException {
        ArrayList<Object[]> rval = new ArrayList<Object[]>();

        StringBuffer dataQuery = new StringBuffer();

        // Select Columns and table name
        dataQuery.append(String.format(SELECT_STATEMENT, DbUtils.getTableName(
                currData.getPe(), currData.getTs())));

        // Add Join
        dataQuery.append(" WHERE ");

        // Add PK statement
        dataQuery.append(HydroDataUtils.getPKStatement(currData));

        rval = runQuery(dataQuery.toString());

        return rval;
    }

    private void updateForecastData(ForecastData currData) throws VizException {
        runStatement(String.format(UPDATE_STATEMENT, DbUtils.getTableName(
                currData.getPe(), currData.getTs()), currData.getValue(),
                currData.getShefQualCode(), currData.getQualityCode(), currData
                        .getRevision(), currData.getProductID(), dbFormat
                        .format(currData.getProductTime()), dbFormat
                        .format(currData.getPostingTime()), HydroDataUtils
                        .getPKStatement(currData)));
    }

    private void insertForecastData(ForecastData currData) throws VizException {
        // Set table and values
        runStatement(String.format(INSERT_INTO_STATEMENT, DbUtils.getTableName(
                currData.getPe(), currData.getTs()), currData.getLid(),
                currData.getPe(), currData.getDur(), currData.getTs(), currData
                        .getExtremum(), currData.getProbability(), dbFormat
                        .format(currData.getValidTime()), dbFormat
                        .format(currData.getBasisTime()), currData.getValue(),
                currData.getShefQualCode(), currData.getQualityCode(), currData
                        .getRevision(), currData.getProductID(), dbFormat
                        .format(currData.getProductTime()), dbFormat
                        .format(currData.getPostingTime())));
    }

    public void putForecastData(ForecastData currData) throws VizException {
        // Check if it's going to be an update or insert
        if (getForecastData(currData).size() > 0) {
            // Do an update
            updateForecastData(currData);
        } else {
            // Do an insert
            insertForecastData(currData);
        }
    }
}
