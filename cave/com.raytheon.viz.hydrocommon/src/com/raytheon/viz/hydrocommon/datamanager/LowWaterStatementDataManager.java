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
import com.raytheon.viz.hydrocommon.data.LowWaterStatementData;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;

/**
 * Class for managing database query calls. LowWaterStatementDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008 1697       askripsky   Initial Creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class LowWaterStatementDataManager extends HydroDataManager {
    protected static LowWaterStatementDataManager manager = null;

    private static final String INSERT_STATEMENT = "INSERT INTO lwstmt (lid, pe, lower_value, upper_value, criteria_rank, statement, lw_criteria, lw_source) VALUES ('%s', '%s', %s, %s, %s, '%s', '%s', '%s')";

    private static final String SELECT_STATEMENT = "SELECT lid, pe, lower_value, upper_value, criteria_rank, statement, lw_criteria, lw_source FROM lwstmt";

    private static final String DELETE_STATEMENT = "DELETE from lwstmt WHERE %s";

    private static final String UPDATE_STATEMENT = "UPDATE lwstmt SET upper_value=%s, statement='%s', lw_criteria='%s', lw_source='%s' WHERE %s";

    /**
     * Private constructor.
     */
    protected LowWaterStatementDataManager() {
        super();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized LowWaterStatementDataManager getInstance() {
        if (manager == null) {
            manager = new LowWaterStatementDataManager();
        }

        return (LowWaterStatementDataManager) manager;
    }

    /**
     * Deletes each record passed in from lwstmt table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecords(ArrayList<LowWaterStatementData> recordsToDelete)
            throws VizException {
        for (LowWaterStatementData currData : recordsToDelete) {
            deleteRecord(currData);
        }
    }

    /**
     * Deletes each record passed in from lwstmt table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecord(LowWaterStatementData recordToDelete)
            throws VizException {
        runStatement(String.format(DELETE_STATEMENT, HydroDataUtils
                .getPKStatement(recordToDelete)));
    }

    /**
     * Get records associated with a location.
     * 
     * @param currData
     * @return
     * @throws VizException
     * @throws VizException
     */
    public ArrayList<LowWaterStatementData> getLowWaterStatementData(String lid)
            throws VizException {
        ArrayList<LowWaterStatementData> rval = new ArrayList<LowWaterStatementData>();

        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE lid='"
                + lid + "' ORDER BY criteria_rank, lower_value ASC");

        for (QueryResultRow currRow : result.getRows()) {
            rval
                    .add(new LowWaterStatementData(currRow, result
                            .getColumnNames()));
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
    public int checkLowWaterStatementData(LowWaterStatementData data)
            throws VizException {
        QueryResult result = runMappedQuery(SELECT_STATEMENT + " WHERE "
                + HydroDataUtils.getPKStatement(data));

        return result.getResultCount();
    }

    private void updateLowWaterStatementData(LowWaterStatementData data)
            throws VizException {
        runStatement(String.format(UPDATE_STATEMENT, data
                .getUpperValueDBString(), data.getStatement(), data
                .getLowWaterCriteria(), data.getLowWaterSource(),
                HydroDataUtils.getPKStatement(data)));
    }

    private void insertLowWaterData(LowWaterStatementData currData)
            throws VizException {
        runStatement(String.format(INSERT_STATEMENT, currData.getLid(),
                currData.getPe(), currData.getLowerValue(), currData
                        .getUpperValueDBString(), currData.getCriteriaRank(),
                currData.getStatement(), currData.getLowWaterCriteria(),
                currData.getLowWaterSource()));
    }

    public void putLowWaterStatementData(LowWaterStatementData dataToPut)
            throws VizException {
        // Check if it's going to be an update or insert
        if (checkLowWaterStatementData(dataToPut) > 0) {
            // Do an update
            updateLowWaterStatementData(dataToPut);
        } else {
            // Do an insert
            insertLowWaterData(dataToPut);
        }
    }
}
