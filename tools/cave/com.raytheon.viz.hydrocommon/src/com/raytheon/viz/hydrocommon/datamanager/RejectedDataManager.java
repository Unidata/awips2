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
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class RejectedDataManager extends HydroDataManager {
    protected static RejectedDataManager manager = null;

    private static final String SELECT_COLUMNS = "rd.lid, loc.name, rd.pe, rd.dur, rd.ts, rd.extremum, rd.value, "
            + "rd.shef_qual_code, rd.quality_code, rd.revision, rd.product_id, rd.producttime, rd.postingtime, cast(rd.probability as float), "
            + "rd.validtime, rd.basistime, rd.reject_type, rd.userid";

    private static final String SELECT_STATEMENT = "SELECT " + SELECT_COLUMNS
            + " FROM rejecteddata rd, location loc where rd.lid=loc.lid";

    private static final String SELECT_CHECK_STATEMENT = "SELECT lid, pe, dur, ts, extremum, value, "
            + "shef_qual_code, quality_code, revision, product_id, producttime, postingtime, cast(probability as float), "
            + "validtime, basistime, reject_type, userid from rejecteddata";

    private static final String INSERT_INTO_STATEMENT = "INSERT INTO rejecteddata (";

    private static final String INSERT_COLUMNS = "lid, pe, dur, ts, extremum, probability, validtime, "
            + "basistime, postingtime, value, revision, shef_qual_code, product_id, producttime, quality_code, reject_type, userid";

    private static final String INSERT_VALUES = ") VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')";

    private static final String UPDATE_STATEMENT = "UPDATE rejecteddata SET value='%s', shef_qual_code='%s', quality_code='%s', revision='%s', product_id='%s', producttime='%s', reject_type='%s', userid='%s' WHERE %s";

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
    public static synchronized RejectedDataManager getInstance() {
        if (manager == null) {
            manager = new RejectedDataManager();
        }

        return (RejectedDataManager) manager;
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

    private void addRejectedDataRecord(RejectedData newRejectedData)
            throws VizException {
        runStatement(INSERT_INTO_STATEMENT
                + INSERT_COLUMNS
                + String.format(INSERT_VALUES, newRejectedData.getLid(),
                        newRejectedData.getPe(), newRejectedData.getDur(),
                        newRejectedData.getTs(), newRejectedData.getExtremum(),
                        newRejectedData.getProbability(), dbFormat
                                .format(newRejectedData.getValidTime()),
                        dbFormat.format(newRejectedData.getBasisTime()),
                        dbFormat.format(newRejectedData.getPostingTime()),
                        newRejectedData.getValue(), newRejectedData
                                .getRevision(), newRejectedData
                                .getShefQualCode(), newRejectedData
                                .getProductID(), dbFormat
                                .format(newRejectedData.getProductTime()),
                        newRejectedData.getQualityCode(), newRejectedData
                                .getRejectType(), newRejectedData.getUserID()));
    }

    public ArrayList<RejectedData> getRejectedData() throws VizException {
        ArrayList<RejectedData> rval = new ArrayList<RejectedData>();

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
    public ArrayList<Object[]> getRejectedDataRawData() throws VizException {
        return runQuery(SELECT_STATEMENT);
    }

    /**
     * Deletes each record passed in from the the rejecteddata table.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteRecords(ArrayList<RejectedData> recordsToDelete)
            throws VizException {
        for (RejectedData currData : recordsToDelete) {
            deleteRecord(currData);
        }
    }

    /**
     * Deletes each record passed in from the the rejecteddata table.
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
     * Updates a record
     * 
     * @param data
     * @throws VizException
     */
    private void updateRejectedData(RejectedData data) {
        try {
            runStatement(String.format(UPDATE_STATEMENT, data.getValue(), data
                    .getShefQualCode(), data.getQualityCode(), data
                    .getRevision(), data.getProductID(), dbFormat.format(data
                    .getProductTime()), data.getRejectType(), data.getUserID(),
                    HydroDataUtils.getPKStatement(data)));
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public void putRejectedData(RejectedData data) {
        try {
            if (checkRejectedData(data) > 0) {
                // Do an update
                updateRejectedData(data);
            } else {
                // Do an insert
                addRejectedDataRecord(data);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
