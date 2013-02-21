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
package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Post processor for the NAM80 (ETA) model. This post processor generates the
 * missing 6 hour total and convective precipitation grids.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class Nam80PostProcessor implements IDecoderPostProcessor {

    /** The number of seconds in 6 hours */
    private static final int SECONDS_IN_6_HRS = 21600;

    /** Parameter abbreviation for 12 hr total precipitation accumulation */
    private static final String TP_12HR = "TP12hr";

    /** Parameter abbreviation for 6 hr total precipitation accumulation */
    private static final String TP_6HR = "TP6hr";

    /** Parameter abbreviation for 12 hr convective precipitation accumulation */
    private static final String CP_12HR = "CP12hr";

    /** Parameter abbreviation for 6 hr convective precipitation accumulation */
    private static final String CP_6HR = "CP6hr";

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.plugin.grib.decoderpostprocessors.IDecoderPostProcessor
     * #process(com.raytheon.uf.common.dataplugin.grib.GridRecord)
     */
    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        /*
         * Determine if this record is a 6 or 12 hour total precipitation
         * accumulation grid
         */
        if (record.getParameter().getAbbreviation().equals(TP_12HR)) {
            return generate6HrGrids(record, false, TP_6HR, TP_12HR);
        } else if (record.getParameter().getAbbreviation().equals(TP_6HR)) {
            return generate6HrGrids(record, true, TP_6HR, TP_12HR);
        } else if (record.getParameter().getAbbreviation().equals(CP_12HR)) {
            return generate6HrGrids(record, false, CP_6HR, CP_12HR);
        } else if (record.getParameter().getAbbreviation().equals(CP_6HR)) {
            return generate6HrGrids(record, true, CP_6HR, CP_12HR);
        }

        return new GridRecord[] { record };
    }

    @SuppressWarnings("unchecked")
    private GridRecord[] generate6HrGrids(GridRecord currentRecord,
            boolean sixHr, String parameter6hr, String parameter12hr)
            throws GribException {
        // The 12 hr accumulation grid to use in the calculations
        GridRecord tp12record = null;

        // The 6 hr accumulation grid to use in the calculations
        GridRecord tp6record = null;
        Date refTime = currentRecord.getDataTime().getRefTime();
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating Grib Dao!", e);
        }

        /*
         * If the current record is a 6 hr accumulation grid, get the 12 hr grid
         * and vice versa
         */
        DatabaseQuery dbQuery = new DatabaseQuery(GridRecord.class);
        dbQuery.addQueryParam(GridConstants.DATASET_ID, "ETA");
        dbQuery.addQueryParam("dataTime.refTime", refTime);
        if (sixHr) {
            tp6record = currentRecord;
            dbQuery.addQueryParam(GridConstants.PARAMETER_ABBREVIATION,
                    parameter12hr);
            dbQuery.addQueryParam("dataTime.fcstTime", currentRecord
                    .getDataTime().getFcstTime() + SECONDS_IN_6_HRS);
        } else {
            tp12record = currentRecord;
            dbQuery.addQueryParam(GridConstants.PARAMETER_ABBREVIATION,
                    parameter6hr);
            dbQuery.addQueryParam("dataTime.fcstTime", currentRecord
                    .getDataTime().getFcstTime() - SECONDS_IN_6_HRS);
        }
        try {
            List<GridRecord> results = (List<GridRecord>) dao
                    .queryByCriteria(dbQuery);
            if (results.isEmpty()) {
                return new GridRecord[] { currentRecord };
            }
            if (sixHr) {
                tp12record = results.get(0);
            } else {
                tp6record = results.get(0);
            }

        } catch (DataAccessLayerException e) {
            throw new GribException("Error querying for 12 hr precip records!",
                    e);
        }

        Set<GridRecord> retVal = new HashSet<GridRecord>();
        retVal.add(currentRecord);
        retVal.add(generateGrid(tp12record, tp6record, dao, parameter6hr));

        return retVal.toArray(new GridRecord[] {});
    }

    private GridRecord generateGrid(GridRecord tp12HrRecord,
            GridRecord tp6HrRecord, GridDao dao, String parameter)
            throws GribException {

        GridRecord newRecord = new GridRecord();
        try {
            float[] newData = null;
            float[] tp6Data = null;
            if (tp12HrRecord.getMessageData() == null) {
                newData = (float[]) ((FloatDataRecord) dao.getHDF5Data(
                        tp12HrRecord, -1)[0]).getFloatData();
            } else {
                newData = (float[]) tp12HrRecord.getMessageData();
            }
            if (tp6HrRecord.getMessageData() == null) {
                tp6Data = (float[]) ((FloatDataRecord) dao.getHDF5Data(
                        tp6HrRecord, -1)[0]).getFloatData();
            } else {
                tp6Data = (float[]) tp6HrRecord.getMessageData();
            }
            for (int i = 0; i < newData.length; i++) {
                newData[i] -= tp6Data[i];
            }
            newRecord.setMessageData(newData);
        } catch (PluginException e) {
            throw new GribException("Error retrieving precipitation data", e);
        }

        newRecord.setLocation(tp6HrRecord.getLocation());
        newRecord.setDatasetId(tp6HrRecord.getDatasetId());
        newRecord.setLevel(tp6HrRecord.getLevel());
        Parameter param = new Parameter(parameter, tp6HrRecord.getParameter()
                .getUnit());
        newRecord.setParameter(param);
        Calendar refTime = tp12HrRecord.getDataTime().getRefTimeAsCalendar();
        Date start = new Date(tp12HrRecord.getDataTime().getValidPeriod()
                .getEnd().getTime()
                - SECONDS_IN_6_HRS * 1000);

        DataTime newDataTime = new DataTime(refTime, tp12HrRecord.getDataTime()
                .getFcstTime(), new TimeRange(start, tp12HrRecord.getDataTime()
                .getValidPeriod().getEnd()));

        // Reset the datauri since the datauri contains the DataTime
        newRecord.setDataTime(newDataTime);
        newRecord.getInfo().setId(null);
        newRecord.setDataURI(null);
        try {
            newRecord.setPluginName(GridConstants.GRID);
            newRecord.constructDataURI();
        } catch (PluginException e) {
            throw new GribException("Error constructing dataURI!", e);
        }
        newRecord.setOverwriteAllowed(true);
        return newRecord;
    }
}
