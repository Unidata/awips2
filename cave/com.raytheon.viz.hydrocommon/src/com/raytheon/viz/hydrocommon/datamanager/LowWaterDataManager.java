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
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.LowWaterData;
import com.raytheon.viz.hydrocommon.util.DbUtils;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;

/**
 * Class for managing database query calls. LowWaterDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2008 1661       askripsky   Initial Creation
 * Apr 19, 2013 1790       rferrel     Cleanup method interfaces; 
 *                                      part of non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class LowWaterDataManager extends HydroDataManager {
    protected static LowWaterDataManager manager = null;

    private static final String INSERT_STATEMENT = "INSERT INTO lowwater (lid, lwdat, q, lwrem, stage) VALUES ('%s', '%s', %s, '%s', %s)";

    private static final String SELECT_STATEMENT = "SELECT lid, lwdat, q, lwrem, stage FROM lowwater";

    private static final String DELETE_STATEMENT = "DELETE FROM lowwater WHERE %s";

    private static final String UPDATE_STATEMENT = "UPDATE lowwater SET q=%s, lwrem='%s', stage=%s WHERE %s";

    private SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * Private constructor.
     */
    protected LowWaterDataManager() {
        super();

        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized LowWaterDataManager getInstance() {
        if (manager == null) {
            manager = new LowWaterDataManager();
        }

        return (LowWaterDataManager) manager;
    }

    /**
     * Deletes each record passed in from lowwater table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecords(List<LowWaterData> recordsToDelete)
            throws VizException {
        for (LowWaterData currData : recordsToDelete) {
            deleteRecord(currData);
        }
    }

    /**
     * Deletes each record passed in from lowwater table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecord(LowWaterData recordToDelete) throws VizException {
        runStatement(String.format(DELETE_STATEMENT,
                HydroDataUtils.getPKStatement(recordToDelete)));
    }

    /**
     * Checks to see if the record exists by checking by PK, should only return
     * 0 or 1 records.
     * 
     * @param currData
     * @return
     * @throws VizException
     * @throws VizException
     */
    public LowWaterData getLowWaterData(LowWaterData data) throws VizException {
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE "
                + HydroDataUtils.getPKStatement(data));

        return (result.getResultCount() > 0) ? new LowWaterData(
                result.getRows()[0], result.getColumnNames())
                : new LowWaterData();
    }

    /**
     * Get records associated with a location.
     * 
     * @param currData
     * @return
     * @throws VizException
     * @throws VizException
     */
    public List<LowWaterData> getLowWaterData(String lid) throws VizException {
        List<LowWaterData> rval = new ArrayList<LowWaterData>();

        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE lid='"
                + lid + "' ORDER BY stage DESC, lwdat DESC");

        for (QueryResultRow currRow : result.getRows()) {
            rval.add(new LowWaterData(currRow, result.getColumnNames()));
        }

        return rval;
    }

    /**
     * Checks to see if the record already exists
     * 
     * @param lid
     * @return
     * @throws VizException
     */
    public int checkLowWaterData(LowWaterData data) throws VizException {
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE "
                + HydroDataUtils.getPKStatement(data));

        return result.getResultCount();
    }

    private void updateLowWaterData(LowWaterData data) throws VizException {

        DbUtils.escapeSpecialCharforData(data);

        runStatement(String.format(
                UPDATE_STATEMENT,
                (data.getFlow() == LowWaterData.MISSING_VALUE) ? "null" : data
                        .getFlow(), data.getRemark(),
                (data.getStage() == LowWaterData.MISSING_VALUE_D) ? "null"
                        : data.getStage(), HydroDataUtils.getPKStatement(data)));
    }

    private void insertLowWaterData(LowWaterData currData) throws VizException {

        DbUtils.escapeSpecialCharforData(currData);

        runStatement(String.format(INSERT_STATEMENT, currData.getLid(),
                dateFormat.format(currData.getDate()),
                (currData.getFlow() == LowWaterData.MISSING_VALUE) ? "null"
                        : currData.getFlow(), currData.getRemark(), (currData
                        .getStage() == LowWaterData.MISSING_VALUE_D) ? "null"
                        : String.format("%8.2f", currData.getStage())));
    }

    public void putLowWaterData(String lid, Date lwDate, int flow,
            String remark, double stage) throws VizException {

        LowWaterData newData = new LowWaterData();

        newData.setLid(lid);
        newData.setDate(lwDate);
        newData.setFlow(flow);
        newData.setRemark(remark);
        newData.setStage(stage);

        // Check if it's going to be an update or insert
        if (checkLowWaterData(newData) > 0) {
            // Do an update
            updateLowWaterData(newData);
        } else {
            // Do an insert
            insertLowWaterData(newData);
        }
    }
}
