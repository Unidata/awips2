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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Used to generate 6hr record from 12hr intervals.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 25, 2011            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
public class GFSProcessor extends SixHrPrecipGridProcessor {
    private static final int SECONDS_IN_12_HRS = 43200;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GFSProcessor.class);

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        // Post process the data if this is a Total Precipitation grid
        if (record.getParameter().getAbbreviation().equals("TP12hr")
                && record.getDataTime().getFcstTime() / 3600 > 180) {
            return super.process(record);
        }
        return new GridRecord[] { record };
    }

    /**
     * Generates the 6 hour accumulated grid from the run accumulated
     * precipitation grids. This function will look in the inventory and
     * generate any 6 hr grids that can be generated.
     * 
     * @param record
     *            The grib record for which to generate the 6 hour accumulated
     *            precipitation grid
     * @return The generated 6-hr precipitation grids
     * @throws GribException
     */
    protected synchronized GridRecord[] generate6hrPrecipGrids(GridRecord record)
            throws GribException {
        List<GridRecord> generated6hrPrecips = new ArrayList<GridRecord>();
        // Get all 6hr records 180Hrs and greater
        List<GridRecord> precipInventory = getPrecipInventory(record
                .getDataTime().getRefTime());
        List<GridRecord> generatedRecords = new ArrayList<GridRecord>();
        // convert current record to 6hr and add it
        GridRecord transformed = transForm12to6(record);
        generated6hrPrecips.add(transformed);
        precipInventory.add(transformed);
        Comparator<GridRecord> comparator = new Comparator<GridRecord>() {
            @Override
            public int compare(GridRecord o1, GridRecord o2) {
                int retValue = 0;
                if (o1 != o2) {
                    retValue = Double.compare(o1.getDataTime().getFcstTime(),
                            o2.getDataTime().getFcstTime());
                }
                return retValue;
            }
        };

        Collections.sort(precipInventory, comparator);
        // loop through set, find twelve hour gaps and create new 6hr records.
        for (int i = 0; i < precipInventory.size() - 1; i++) {
            GridRecord sequence1Record = precipInventory.get(i);
            GridRecord sequence2Record = precipInventory.get(i + 1);
            if (sequence1Record.getDataTime().getFcstTime() == sequence2Record
                    .getDataTime().getFcstTime() - SECONDS_IN_12_HRS) {
                // we have a 12Hr gap
                generated6hrPrecips.add(calculate6hrPrecip(sequence1Record,
                        sequence2Record));
            }
        }
        for (GridRecord newRecord : generated6hrPrecips) {
            // Add the generated grid to the current inventory
            if (newRecord != null) {
                generatedRecords.add(newRecord);
            }
        }
        return generatedRecords.toArray(new GridRecord[] {});
    }

    @SuppressWarnings("unchecked")
    protected List<GridRecord> getPrecipInventory(Date refTime)
            throws GribException {
        GridDao dao = null;
        try {
            dao = new GridDao();
        } catch (PluginException e) {
            throw new GribException("Error instantiating grib dao!", e);
        }
        DatabaseQuery query = new DatabaseQuery(GridRecord.class);
        query.addQueryParam(GridConstants.PARAMETER_ABBREVIATION, "TP6hr",
                QueryOperand.IN);
        query.addQueryParam(GridConstants.DATASET_ID, "GFS213");
        query.addQueryParam("dataTime.refTime", refTime);
        query.addQueryParam("dataTime.fcstTime", 648000,
                QueryOperand.GREATERTHANEQUALS);
        query.addOrder("dataTime.fcstTime", true);
        try {
            return (List<GridRecord>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            throw new GribException(
                    "Error getting Precip inventory for ECMWF!", e);
        }
    }

    private GridRecord transForm12to6(GridRecord currentRecord)
            throws GribException {

        // Clone the current record and set the ID to 0 so Hibernate will
        // recognize it as a new record
        GridRecord tp6hrRecord = new GridRecord(currentRecord);
        tp6hrRecord.setId(0);
        if (currentRecord.getMessageData() == null) {
            GridDao dao = null;
            try {
                dao = new GridDao();
                currentRecord.setMessageData(((FloatDataRecord) dao
                        .getHDF5Data(currentRecord, -1)[0]).getFloatData());
            } catch (PluginException e) {
                throw new GribException("Error populating grib data!", e);
            }
        }

        // Copy the data to the new record so the data from the original record
        // does not get modified
        float[] currentData = (float[]) currentRecord.getMessageData();
        currentRecord.setMessageData(currentData);
        float[] newData = new float[currentData.length];
        System.arraycopy(currentData, 0, newData, 0, currentData.length);
        tp6hrRecord.setMessageData(newData);

        // Assign the new parameter abbreviation and cache it if necessary
        Parameter param = new Parameter("TP6hr", "Precip Accum 6 hr",
                currentRecord.getParameter().getUnit());
        tp6hrRecord.setParameter(param);
        tp6hrRecord.getInfo().setId(null);
        // Change the data time to include the 6-hr time range
        super.modifyDataTime(tp6hrRecord);
        return tp6hrRecord;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void calculatePrecipValues(float[] inventoryData, float[] newData) {
        for (int i = 0; i < inventoryData.length; i++) {
            newData[i] = (newData[i] + inventoryData[i]) / 2;
            if (newData[i] < 0) {
                newData[i] = 0;
            }
        }
    }

    @Override
    protected void modifyDataTime(GridRecord record) {

        Calendar refTime = record.getDataTime().getRefTimeAsCalendar();
        int fcstTime = record.getDataTime().getFcstTime();

        // Calculate the start time by subtracting 6 hours from the reference
        // time + forecast time
        Calendar startTime = (Calendar) refTime.clone();
        startTime.add(Calendar.SECOND, fcstTime - SECONDS_IN_6_HRS);

        // Calculate the end time by adding the reference time + forecast time
        Calendar endTime = (Calendar) refTime.clone();
        endTime.add(Calendar.SECOND, fcstTime);
        TimeRange validPeriod = new TimeRange(startTime, endTime);
        DataTime newDataTime = new DataTime(refTime, fcstTime
                - SECONDS_IN_6_HRS, validPeriod);

        // Reset the datauri since the datauri contains the DataTime
        record.setDataTime(newDataTime);
        record.setDataURI(null);
        try {
            record.setPluginName(GridConstants.GRID);
            record.constructDataURI();
        } catch (PluginException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error constructing dataURI!", e);
        }
    }
}
