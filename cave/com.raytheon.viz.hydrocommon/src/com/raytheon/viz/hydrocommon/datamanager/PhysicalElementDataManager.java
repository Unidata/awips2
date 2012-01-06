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
import com.raytheon.viz.hydrocommon.data.PhysicalElementData;
import com.raytheon.viz.hydrocommon.util.DbUtils;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.hydrocommon.util.QualityCodeUtil;

/**
 * Class for managing database query calls. QuestionableDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2008 1636       askripsky   Initial Creation.
 * May 08, 2009 2181       mpduff      Fixed query.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class PhysicalElementDataManager extends HydroDataManager {

    private static final String INSERT_QUERY = "INSERT INTO %s (lid, pe, dur, ts, extremum, obstime, value, shef_qual_code,"
            + " quality_code, revision, product_id, producttime, postingtime) VALUES ('%s', '%s', '%s', "
            + "'%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')";

    private static final String SELECT_STATEMENT = "SELECT lid, pe, dur, ts, extremum, value, shef_qual_code, quality_code, "
            + "revision, product_id, producttime, postingtime, obstime FROM %s WHERE %s";

    /**
     * Enumeration of the tables supported by this data manager
     */
    public enum PhysicalElementTable {
        agricultural, curpc, curpp, discharge, evaporation, fishcount, gatedam, ground, height, ice, lake, moisture, power, rawpc, rawpp, rawpother, pressure, radiation, snow, temperature, waterquality, weather, wind, yunique;
    }

    private static PhysicalElementDataManager manager = null;

    /**
     * Private constructor.
     */
    protected PhysicalElementDataManager() {
        super();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized PhysicalElementDataManager getInstance() {
        if (manager == null) {
            manager = new PhysicalElementDataManager();
        }

        return manager;
    }

    /**
     * Get tabularDisplayData from the DB
     * 
     * @param sortCriteria
     * @param daysBack
     * 
     * @return String[]
     */
    public ArrayList<Object[]> getPhysicalElementData(
            PhysicalElementTable selectedPE, String obsTimeConstraint,
            String sortCriteria) {
        ArrayList<Object[]> rval = new ArrayList<Object[]>();

        StringBuffer dataQuery = new StringBuffer();

        // Select Columns
        dataQuery.append("SELECT ");
        dataQuery.append("phs.lid, loc.name, phs.pe, phs.dur, phs.ts,");
        dataQuery
                .append("phs.extremum, cast(phs.value as float), phs.shef_qual_code,");
        dataQuery.append("phs.quality_code, phs.revision, phs.product_id,");
        dataQuery.append("phs.producttime, phs.postingtime, phs.obstime");

        // Add tables
        dataQuery.append(" FROM location loc, ");
        dataQuery.append(selectedPE.toString());
        dataQuery.append(" phs");

        // Add Join
        dataQuery.append(" WHERE loc.lid = phs.lid");

        // Add time constraints
        dataQuery.append(" AND phs.obstime > '");
        dataQuery.append(obsTimeConstraint);
        dataQuery.append("'");

        // Add Quality Code
        dataQuery.append(" AND quality_code < "
                + QualityCodeUtil.GOOD_QUESTIONABLE_THRESHOLD);

        // Add Ordering
        if (sortCriteria.compareTo("") != 0) {
            dataQuery.append(" ORDER BY ");
            dataQuery.append(sortCriteria);
        }

        try {
            rval = runQuery(dataQuery.toString());

        } catch (Exception e) {
            e.printStackTrace();
        }

        return rval;
    }

    public ArrayList<Object[]> getPhysicalElementData(
            PhysicalElementData currData) {
        ArrayList<Object[]> rval = new ArrayList<Object[]>();

        try {
            rval = runQuery(String.format(SELECT_STATEMENT, DbUtils
                    .getTableName(currData.getPe(), currData.getTs()),
                    HydroDataUtils.getPKStatement(currData)));

        } catch (Exception e) {
            e.printStackTrace();
        }

        return rval;
    }

    /**
     * Removes a Physical Element data record
     * 
     * @param recordsToDelete
     * @param selectedPETable
     * @throws VizException
     */
    protected void removePhysicalElementData(
            PhysicalElementData recordToDelete,
            PhysicalElementTable selectedPETable) throws VizException {
        String deleteQuery = String.format("DELETE FROM %s WHERE %s",
                selectedPETable.toString(), HydroDataUtils
                        .getPKStatement(recordToDelete));

        // Delete Physical Element record from the respective table
        runStatement(deleteQuery.toString());
    }

    /**
     * Deletes Physical Element records if the table is not known
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    protected void removePhysicalElementData(PhysicalElementData recordToDelete)
            throws VizException {
        removePhysicalElementData(recordToDelete, PhysicalElementTable
                .valueOf(DbUtils.getTableName(recordToDelete.getPe(),
                        recordToDelete.getTs()).toLowerCase()));
    }

    private void updatePhysicalElementData(PhysicalElementData currData)
            throws VizException {
        StringBuffer updateQuery = new StringBuffer("Update ");

        updateQuery.append(DbUtils.getTableName(currData.getPe(), currData
                .getTs()));

        long qcCode = QualityCodeUtil.setQcCode(currData.getQualityCode());

        // Set Columns and Values
        updateQuery.append(" SET ");
        updateQuery.append("value='");
        updateQuery.append(currData.getValue());
        updateQuery.append("', shef_qual_code='");
        updateQuery.append(currData.getShefQualCode());
        updateQuery.append("', quality_code=");
        updateQuery.append(qcCode);
        updateQuery.append(", revision='");
        updateQuery.append(currData.getRevision());
        updateQuery.append("', product_id='");
        updateQuery.append(currData.getProductID());
        updateQuery.append("', producttime='");
        updateQuery.append(currData.getProductTimeString());
        updateQuery.append("', postingtime='");
        updateQuery.append(currData.getPostingTimeString());
        updateQuery.append("'");

        // Where Clause
        updateQuery.append(" WHERE ");
        updateQuery.append(HydroDataUtils.getPKStatement(currData));

        runStatement(updateQuery.toString());
    }

    private void insertPhysicalElementData(PhysicalElementData currData)
            throws VizException {
        // Set table and values
        long qcCode = QualityCodeUtil.setQcCode(currData.getQualityCode());

        runStatement(String.format(INSERT_QUERY, DbUtils.getTableName(currData
                .getPe(), currData.getTs()), currData.getLid(), currData
                .getPe(), currData.getDur(), currData.getTs(), currData
                .getExtremum(), dbFormat.format(currData.getObstime()),
                currData.getValue(), currData.getShefQualCode(), qcCode,
                currData.getRevision(), currData.getProductID(), dbFormat
                        .format(currData.getProductTime()), dbFormat
                        .format(currData.getPostingTime())));
    }

    public void putPhysicalElementData(PhysicalElementData currData)
            throws VizException {
        // Check if it's going to be an update or insert
        if (getPhysicalElementData(currData).size() > 0) {
            // Do an update
            updatePhysicalElementData(currData);
        } else {
            // Do an insert
            insertPhysicalElementData(currData);
        }
    }
}
