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
import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.PhysicalElementData;
import com.raytheon.viz.hydrocommon.data.RejectedData;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;

/**
 * Class for managing database query calls. RejectedDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2008 1636       askripsky   Initial Creation
 * Feb 06, 2013 1578       rferrel     Code clean for non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class RejectedDataManager extends HydroDataManager {
    /** Handler for error messages. */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RejectedDataManager.class);

    /** Singleton instance of class. */
    private static final RejectedDataManager manager = new RejectedDataManager();

    /** Select columns query should retreive. */
    private final String SELECT_COLUMNS = "rd.lid, loc.name, rd.pe, rd.dur, rd.ts, rd.extremum, rd.value, "
            + "rd.shef_qual_code, rd.quality_code, rd.revision, rd.product_id, rd.producttime, rd.postingtime, cast(rd.probability as float), "
            + "rd.validtime, rd.basistime, rd.reject_type, rd.userid";

    /** The query select statement. */
    private final String SELECT_STATEMENT = "SELECT " + SELECT_COLUMNS
            + " FROM rejecteddata rd, location loc where rd.lid=loc.lid";

    /** The first part of the select statement to check if record exists. */
    private final String SELECT_CHECK_STATEMENT = "SELECT lid, pe, dur, ts, extremum, value, "
            + "shef_qual_code, quality_code, revision, product_id, producttime, postingtime, cast(probability as float), "
            + "validtime, basistime, reject_type, userid from rejecteddata";

    /** First part of insert statement to add a rejected record. */
    private final String INSERT_INTO_STATEMENT = "INSERT INTO rejecteddata (";

    /** The columns for the inserted rejected record. */
    private final String INSERT_COLUMNS = "lid, pe, dur, ts, extremum, probability, validtime, "
            + "basistime, postingtime, value, revision, shef_qual_code, product_id, producttime, quality_code, reject_type, userid";

    /** The formating for the values to in the insert of a rejected record. */
    private final String INSERT_VALUES = ") VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')";

    /** Formated query for updating a rejected record. */
    private final String UPDATE_STATEMENT = "UPDATE rejecteddata SET value='%s', shef_qual_code='%s', quality_code='%s', revision='%s', product_id='%s', producttime='%s', reject_type='%s', userid='%s' WHERE %s";

    /**
     * Private constructor.
     */
    protected RejectedDataManager() {
        super();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static RejectedDataManager getInstance() {
        return manager;
    }

    /**
     * Create a rejected data object from the deleted Physical Element and add
     * the rejected data to the rejecteddata table
     * 
     * @param questionableData
     * @throws VizException
     */
    public void putRejectedData(PhysicalElementData questionableData)
            throws VizException {
        putRejectedData(new RejectedData(questionableData));
    }

    /**
     * Insert the rejected record into the data base.
     * 
     * @param newRejectedData
     * @throws VizException
     */
    private void addRejectedDataRecord(RejectedData newRejectedData) {

        try {
            runStatement(INSERT_INTO_STATEMENT
                    + INSERT_COLUMNS
                    + String.format(INSERT_VALUES, newRejectedData.getLid(),
                            newRejectedData.getPe(), newRejectedData.getDur(),
                            newRejectedData.getTs(),
                            newRejectedData.getExtremum(),
                            newRejectedData.getProbability(),
                            dbFormat.format(newRejectedData.getValidTime()),
                            dbFormat.format(newRejectedData.getBasisTime()),
                            dbFormat.format(newRejectedData.getPostingTime()),
                            newRejectedData.getValue(),
                            newRejectedData.getRevision(),
                            newRejectedData.getShefQualCode(),
                            newRejectedData.getProductID(),
                            dbFormat.format(newRejectedData.getProductTime()),
                            newRejectedData.getQualityCode(),
                            newRejectedData.getRejectType(),
                            newRejectedData.getUserID()));
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to add record: ", e);
        }
    }

    /**
     * Get a list of rejected data.
     * 
     * @return rval
     * @throws VizException
     */
    public List<RejectedData> getRejectedData() throws VizException {
        List<RejectedData> rval = new ArrayList<RejectedData>();

        for (Object[] currData : getRejectedDataRawData()) {
            rval.add(new RejectedData(currData));
        }
        return rval;
    }

    /**
     * Returns the raw Object[] data from the DB
     * 
     * @return
     * @throws VizException
     */
    public List<Object[]> getRejectedDataRawData() throws VizException {
        return runQuery(SELECT_STATEMENT);
    }

    /**
     * Deletes each record passed in from the the rejecteddata table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecords(List<RejectedData> recordsToDelete)
            throws VizException {
        for (RejectedData currData : recordsToDelete) {
            deleteRecord(currData);
        }
    }

    /**
     * Deletes a record from the rejecteddata table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecord(RejectedData recordToDelete) throws VizException {
        String deleteStatement = "DELETE FROM rejecteddata";
        runStatement(deleteStatement + " WHERE "
                + HydroDataUtils.getPKStatement(recordToDelete));
    }

    /**
     * Checks to see if the record already exists
     * 
     * @param lid
     * @return
     * @throws VizException
     */
    public int checkRejectedData(RejectedData recordToCheck)
            throws VizException {
        QueryResult result = runMappedQuery(SELECT_CHECK_STATEMENT + " WHERE "
                + HydroDataUtils.getPKStatement(recordToCheck));

        return result.getResultCount();
    }

    /**
     * Updates an existing record
     * 
     * @param data
     * @throws VizException
     */
    private void updateRejectedData(RejectedData data) {
        try {
            runStatement(String.format(UPDATE_STATEMENT, data.getValue(),
                    data.getShefQualCode(), data.getQualityCode(),
                    data.getRevision(), data.getProductID(),
                    dbFormat.format(data.getProductTime()),
                    data.getRejectType(), data.getUserID(),
                    HydroDataUtils.getPKStatement(data)));
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Unble to update record: ",
                    e);
        }
    }

    /**
     * Update an existing record or insert a new record.
     * 
     * @param record
     */
    public void putRejectedData(RejectedData record) {
        try {
            if (checkRejectedData(record) > 0) {
                // Do an update
                updateRejectedData(record);
            } else {
                // Do an insert
                addRejectedDataRecord(record);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Check for record failed: ",
                    e);
        }
    }
}
