package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Abstract class to generate 1-hour precip grids
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer      Description
 * ------------- -------- ------------- --------------------------
 * Sep 05, 2014           M. Foster     Initial creation
 * Oct 07, 2015  3756     nabowle       Extends DecoderPostProcessor.
 * 
 * </pre>
 * 
 * @author matthew.foster
 * @version 1.0
 * 
 */

public abstract class OneHrPrecipGridProcessor extends DecoderPostProcessor {
    /** The number of seconds in 1 hour */
    protected static final int SECONDS_IN_1_HR = 3600;

    public GridRecord[] process(GridRecord record) throws GribException {

        // Post process the data if this is a 2hr or 3hr precip accumulation

        GridRecord[] newRecords = generate1hrPrecipGrids(record);
        GridRecord[] retVal = new GridRecord[newRecords.length + 1];
        retVal[0] = record;
        for (int i = 1; i < retVal.length; i++) {
            retVal[i] = newRecords[i - 1];
        }
        return retVal;

    }

    protected abstract GridRecord[] generate1hrPrecipGrids(GridRecord record)
            throws GribException;

    /**
     * Generates the 1hr precipitation grid
     *
     * @param record
     *            The current record to clone and modify to produce the new 1hr
     *            grid
     * @param precipInventory
     *            The current run accumulated grid inventory
     * @return The generated 1hr precipitation grid
     * @throws GribException
     */
    protected List<GridRecord> generate1hrPrecip(GridRecord record,
            List<GridRecord> precipInventory)
            throws GribException {
        List<GridRecord> tp1hrRecords = new ArrayList<GridRecord>();
        int currentFcstTime = record.getDataTime().getFcstTime();

        for (GridRecord rec : precipInventory) {
            if (rec.getDataTime().getFcstTime() == (currentFcstTime - SECONDS_IN_1_HR)) {
                tp1hrRecords.add(calculate1hrPrecip(rec, record));
            }
        }
        return tp1hrRecords;
    }

    /**
     * Generates the 1hr precipitation grid from the current grid and the
     * previous grid
     *
     * @param inventoryRecord
     *            The previous grid from the inventory
     * @param currentRecord
     *            The current grid
     * @return The generated 1hr precipitation grid
     * @throws GribException
     */
    protected GridRecord calculate1hrPrecip(GridRecord inventoryRecord,
            GridRecord currentRecord) throws GribException {

        // Clone the current record and set the ID to 0 so Hibernate will
        // recognize it as a new record
        GridRecord tp1hrRecord = new GridRecord(currentRecord);
        tp1hrRecord.setId(0);
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
        tp1hrRecord.setMessageData(newData);

        // Assign the new parameter abbreviation and cache it if necessary

        Parameter param = new Parameter("TP1hr", "Precip Accum 1 hr",
                currentRecord.getParameter().getUnit());
        tp1hrRecord.setParameter(param);
        tp1hrRecord.getInfo().setId(null);
        // Change the data time to include the 1-hr time range
        modifyDataTime(tp1hrRecord);

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
                    (float[]) tp1hrRecord.getMessageData());
        }
        return tp1hrRecord;
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
     * Modifies the DataTime of the provided record to include a 1hr time range
     *
     * @param record
     *            The record to modify the datatime for
     */
    protected void modifyDataTime(GridRecord record) {

        Calendar refTime = record.getDataTime().getRefTimeAsCalendar();
        int fcstTime = record.getDataTime().getFcstTime();

        // Calculate the start time by subtracting 1 hour from the reference
        // time + forecast time
        Calendar startTime = (Calendar) refTime.clone();
        startTime.add(Calendar.SECOND, fcstTime - SECONDS_IN_1_HR);

        // Calculate the end time by adding the reference time + forecast time
        Calendar endTime = (Calendar) refTime.clone();
        endTime.add(Calendar.SECOND, fcstTime);
        TimeRange validPeriod = new TimeRange(startTime, endTime);
        DataTime newDataTime = new DataTime(refTime, fcstTime, validPeriod);

        // Reset the datauri since the datauri contains the DataTime
        record.setDataTime(newDataTime);
        record.setDataURI(null);
    }
}
