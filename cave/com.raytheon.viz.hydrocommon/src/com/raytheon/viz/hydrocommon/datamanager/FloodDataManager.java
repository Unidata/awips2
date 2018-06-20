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

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.FloodData;
import com.raytheon.viz.hydrocommon.util.DbUtils;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;

/**
 * Class for managing database query calls. FloodCategoryDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2008 1661       askripsky   Initial Creation
 * Jan 15, 2016 DCS18180     JingtaoD   code improvement based on code review for DR17935
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class FloodDataManager extends HydroDataManager {
    protected static FloodDataManager manager = null;

    private static final String INSERT_STATEMENT = "INSERT INTO flood (lid, stage, damage, dispstmt) VALUES ('%s', '%s', '%s', '%s')";

    private static final String SELECT_STATEMENT = "SELECT lid, stage, damage, dispstmt FROM flood";

    private static final String DELETE_STATEMENT = "DELETE FROM flood WHERE %s";

    private static final String UPDATE_STATEMENT = "UPDATE flood SET damage='%s', dispstmt='%s' WHERE %s";

    /**
     * Private constructor.
     */
    protected FloodDataManager() {
        super();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized FloodDataManager getInstance() {
        if (manager == null) {
            manager = new FloodDataManager();
        }

        return (FloodDataManager) manager;
    }

    /**
     * Deletes each record passed in from flood table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecords(ArrayList<FloodData> recordsToDelete)
            throws VizException {
        for (FloodData currData : recordsToDelete) {
            deleteRecord(currData);
        }
    }

    /**
     * Deletes each record passed in from flood table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecord(FloodData recordToDelete) throws VizException {
        runStatement(String.format(DELETE_STATEMENT,
                HydroDataUtils.getPKStatement(recordToDelete)));
    }

    /**
     * Deletes each record passed in from flood table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecord(String lid) throws VizException {
        runStatement(String.format(DELETE_STATEMENT, "lid='" + lid + "'"));
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
    public FloodData getFloodData(FloodData data) throws VizException {
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE "
                + HydroDataUtils.getPKStatement(data));

        return (result.getResultCount() > 0) ? new FloodData(
                result.getRows()[0], result.getColumnNames()) : new FloodData();
    }

    /**
     * Get records associated with a location.
     * 
     * @param currData
     * @return
     * @throws VizException
     * @throws VizException
     */
    public ArrayList<FloodData> getFloodData(String lid) throws VizException {
        ArrayList<FloodData> rval = new ArrayList<FloodData>();

        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE lid='"
                + lid + "' ORDER BY stage DESC");

        for (QueryResultRow currRow : result.getRows()) {
            rval.add(new FloodData(currRow, result.getColumnNames()));
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
    public int checkFloodData(FloodData data) throws VizException {
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE "
                + HydroDataUtils.getPKStatement(data));

        return result.getResultCount();
    }

    private void updateFloodData(FloodData data) throws VizException {

        runStatement(String
                .format(UPDATE_STATEMENT, data.getDamage(),
                        data.getDisplayStatement(),
                        HydroDataUtils.getPKStatement(data)));

    }

    private void insertFloodData(FloodData currData) throws VizException {

        runStatement(String.format(INSERT_STATEMENT, currData.getLid(),
                String.format("%8.2f", currData.getStage()),
                currData.getDamage(), currData.getDisplayStatement()));
    }

    public void putFloodCategoryData(String lid, double stage, String damage,
            String displayStatement) throws VizException {

        FloodData newData = new FloodData();

        newData.setLid(lid);
        newData.setDamage(damage);
        newData.setDisplayStatement(displayStatement);
        newData.setStage(stage);

        FloodData newDataForQuery = new FloodData();
        DbUtils.escapeSpecialCharforData(newData, newDataForQuery);

        // Check if it's going to be an update or insert
        if (checkFloodData(newDataForQuery) > 0) {
            // Do an update
            updateFloodData(newDataForQuery);
        } else {
            // Do an insert
            insertFloodData(newDataForQuery);
        }
    }
}
