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
import java.util.List;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Abstract class to generate 6hr records
 * 
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
public abstract class SixHrPrecipGridProcessor implements IDecoderPostProcessor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SixHrPrecipGridProcessor.class);

    /** The number of seconds in 6 hours */
    protected static final int SECONDS_IN_6_HRS = 21600;

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        // Post process the data if this is a Total Precipitation grid

        GridRecord[] newRecords = generate6hrPrecipGrids(record);
        GridRecord[] retVal = new GridRecord[newRecords.length + 1];
        retVal[0] = record;
        for (int i = 1; i < retVal.length; i++) {
            retVal[i] = newRecords[i - 1];
        }
        return retVal;

    }

    protected abstract GridRecord[] generate6hrPrecipGrids(GridRecord record)
            throws GribException;

    /**
     * Generates the 6hr precipitation grid
     * 
     * @param record
     *            The current record to clone and modify to produce the new 6hr
     *            grid
     * @param precipInventory
     *            The current run accumulated grid inventory
     * @param precip6hrInventory
     *            The current 6hr precipitation inventory
     * @return The generated 6hr precipitation grid
     * @throws GribException
     */
    protected List<GridRecord> generate6hrPrecip(GridRecord record,
            List<GridRecord> precipInventory, List<Integer> precip6hrInventory)
            throws GribException {
        List<GridRecord> tp6hrRecords = new ArrayList<GridRecord>();
        int currentFcstTime = record.getDataTime().getFcstTime();

        // If this is the first grid (the 6 hr grid), the 6hr precip
        // accumulation is the same as the 6hr run accumulated grid
        if (currentFcstTime == SECONDS_IN_6_HRS) {
            tp6hrRecords.add(calculate6hrPrecip(null, record));
        }
        // If this is not the first grid, generate the new grid using the
        // previous grid
        else {
            for (GridRecord rec : precipInventory) {
                if (rec.getDataTime().getFcstTime() == currentFcstTime
                        - SECONDS_IN_6_HRS) {
                    tp6hrRecords.add(calculate6hrPrecip(rec, record));
                }
            }
        }
        return tp6hrRecords;
    }

    /**
     * Generates the 6hr precipitation grid from the current grid and the
     * previous grid
     * 
     * @param inventoryRecord
     *            The previous grid from the inventory
     * @param currentRecord
     *            The current grid
     * @return The generated 6hr precipitation grid
     * @throws GribException
     */
    protected GridRecord calculate6hrPrecip(GridRecord inventoryRecord,
            GridRecord currentRecord) throws GribException {

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
        modifyDataTime(tp6hrRecord);

        // Calculate the new data values
        if (inventoryRecord != null) {
            if (inventoryRecord.getMessageData() == null) {
                GridDao dao = null;
                try {
                    dao = new GridDao();
                    inventoryRecord
                            .setMessageData(((FloatDataRecord) dao.getHDF5Data(
                                    inventoryRecord, 0)[0]).getFloatData());
                } catch (PluginException e) {
                    throw new GribException("Error populating grib data!", e);
                }
            }
            calculatePrecipValues((float[]) inventoryRecord.getMessageData(),
                    (float[]) tp6hrRecord.getMessageData());
        }
        return tp6hrRecord;
    }

    /**
     * Calculates the new data by subtracting the previous inventory data from
     * the current data
     * 
     * @param inventoryData
     *            The data from the previous precipitation record
     * @param newData
     *            The data from the current precipitation record
     */
    protected abstract void calculatePrecipValues(float[] messageData,
            float[] messageData2);

    /**
     * Modifies the DataTime of the provided record to include a 6hr time range
     * 
     * @param record
     *            The record to modify the datatime for
     */
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
        DataTime newDataTime = new DataTime(refTime, fcstTime, validPeriod);

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
