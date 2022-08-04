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

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.IGetSortType;
import com.raytheon.viz.hydrocommon.data.DataTrashCanData;
import com.raytheon.viz.hydrocommon.data.RejectedData;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;

/**
 * Class for managing database query calls. DataTrashCanDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   #1509      dhladky     Initial Creation
 * 30Oct2008               askripsky   Connect to DB and refactor.
 * 06Feb2013    #1578      rferrel     Code clean up for non-blocking dialogs.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class DataTrashCanDataManager extends RejectedDataManager {

    private static final DataTrashCanDataManager manager = new DataTrashCanDataManager();

    private static final String SELECT_PE_FILTER_QUERY = "SELECT distinct shef.pe, shef.name "
            + "FROM shefpe shef, ingestfilter ing "
            + "WHERE shef.pe = ing.pe "
            + "ORDER BY shef.pe";

    /**
     * Private constructor.
     */
    private DataTrashCanDataManager() {
        super();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static DataTrashCanDataManager getInstance() {
        return manager;
    }

    /**
     * Get tabularDisplayData from the DB
     * 
     * @return String[]
     */
    public List<DataTrashCanData> getDataTrashCanData(IGetSortType sortType)
            throws VizException {
        List<DataTrashCanData> rval = new ArrayList<DataTrashCanData>();

        for (Object[] currData : RejectedDataManager.getInstance()
                .getRejectedDataRawData()) {
            rval.add(new DataTrashCanData(currData, sortType));
        }

        return rval;
    }

    public List<String> getPEList() throws VizException {
        List<String> rval = new ArrayList<String>();

        List<Object[]> data = runQuery(SELECT_PE_FILTER_QUERY);

        for (Object[] currData : data) {
            rval.add((String) currData[0] + " " + (String) currData[1]);
        }

        return rval;
    }

    /**
     * Casts local objects to super type and uses the central delete method.
     * 
     * @param recordsToDelete
     * @throws VizException
     */
    public void deleteTrashRecords(List<DataTrashCanData> recordsToDelete)
            throws VizException {
        List<RejectedData> rejected = new ArrayList<RejectedData>();

        for (RejectedData currData : recordsToDelete) {
            rejected.add((RejectedData) currData);
        }

        deleteRecords(rejected);
    }

    /**
     * Reposts data to the correct PE table and deletes it from the rejecteddata
     * table.
     * 
     * @param currentlySelectedRange
     * @throws VizException
     */
    public void repostData(List<DataTrashCanData> dataToRepost)
            throws VizException {

        for (DataTrashCanData currData : dataToRepost) {

            // Check if the data is an observation or forecast,
            // Forecasts have probability and basistime
            if (currData.getTs().charAt(0) == 'R'
                    || currData.getTs().charAt(0) == 'P') {
                /* create an Observation record from a RejectedData */
                // update/insert data to correct PE table
                PhysicalElementDataManager
                        .getInstance()
                        .putPhysicalElementData(
                                HydroDataUtils
                                        .convertRejectedToPhysicalElement(currData));

            } else /* ts does NOT start with a 'R' or 'P' then assume forecast */
            {
                /* create a Forecast record from a RejectedData */
                ForecastDataManager.getInstance().putForecastData(
                        HydroDataUtils.convertRejectedToForecast(currData));
            }

            // Delete from rejecteddata table
            deleteRecord(currData);
        }
    }
}
