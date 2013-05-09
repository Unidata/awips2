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
package com.raytheon.edex.plugin.bufrua.decoder;

import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.bufrua.UAObs;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.bufrtools.BUFRPointDataAdapter;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * This class contains several utility methods that construct a ProfilerObs
 * instance from the BUFR decoded data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 03, 2008 969        jkorman     Initial implementation.
 * May 09, 2013 1869       bsteffen    Modified D2D time series of point data to
 *                                     work without dataURI.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class AbstractBUFRUAAdapter extends BUFRPointDataAdapter<UAObs> {

    // Allowable future time in milliseconds (2 hours).
    private static final long ALLOWABLE_TIME = 2 * 3600 * 1000;

    private static final int[] HOUR_MAP = {
            // 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
            0, -1, -2, -3, 2, 1, 0, -1, -2, -3, 2, 1, 0, -1, -2, -3, 2, 1, 0,
            -1, -2, -3, 2, 1 };

    private static final int YEAR_POS = 4;

    private static final int MONTH_POS = 5;

    private static final int DAY_POS = 6;

    private static final int HOUR_POS = 7;

    private static final int MINUTE_POS = 8;

    Log logger = LogFactory.getLog(getClass());

    WMOHeader wmoHeader;

    /**
     * 
     * @param pdd
     * @param dao
     * @param pluginName
     */
    public AbstractBUFRUAAdapter(PointDataDescription pdd,
            PointDataPluginDao<UAObs> dao, String pluginName) {
        super(pdd, dao, pluginName);
    }

    @Override
    public UAObs createData(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {
        UAObs obsData = null;

        this.wmoHeader = wmoHeader;

        BUFRDataDocument dataDoc = iterator.next();

        if (dataDoc != null) {
            // Get the primary data list.
            List<IBUFRDataPacket> dataList = dataDoc.getList();

            obsData = getHeaderData(dataList);
            Integer wmoStaId = getWMOStationId(dataList);

            if ((obsData != null) && (wmoStaId != null)) {
                // pickup the data.
                obsData.setPluginName(getPluginName());
                obsData.setWmoHeader(wmoHeader.getWmoHeader());

                Calendar validTime = obsData.getValidTime();

                // Now offset the "record" validTime using the hour mapping.
                int hour = validTime.get(Calendar.HOUR_OF_DAY);
                validTime.add(Calendar.HOUR_OF_DAY, HOUR_MAP[hour]);
                // Set the new validTime back into the UAObs record.
                obsData.setValidTime(validTime);

                Calendar maxFutureTime = Calendar.getInstance();
                maxFutureTime.add(Calendar.HOUR, 12);
                if (validTime.after(maxFutureTime)) {
                    logger.error("Invalid time value: " + validTime.getTime());
                    return null;
                }

                obsData.setRefHour(TimeTools.copy(validTime));
                obsData.setDataTime(new DataTime(TimeTools.copy(validTime)));

                // We have times now, so ok to get container.
                PointDataContainer container = getContainer(obsData);
                if (container != null) {

                    logger.debug("Creating raob data ");

                    PointDataView view = container.append();

                    SurfaceObsLocation location = new SurfaceObsLocation();
                    location.setStationId(String.format("%05d", wmoStaId));
                    view.setInt("wmoStaNum", wmoStaId);
                    obsData.setLocation(location);

                    if ((obsData = getSpecificData(obsData, view, dataList)) != null) {
                        obsData.setPointDataView(view);
                    }
                }
            }
        }
        return obsData;
    }

    /**
     * Empty implementation of this method.
     * 
     * @see com.raytheon.uf.edex.bufrtools.BUFRPointDataAdapter#createDataList(java.util.Iterator,
     *      com.raytheon.uf.edex.wmo.message.WMOHeader)
     */
    @Override
    public List<UAObs> createDataList(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {
        return null;
    }

    /**
     * 
     * @param obsData
     * @return
     */
    abstract UAObs getSpecificData(UAObs obsData, PointDataView view,
            List<IBUFRDataPacket> dataList);

    /**
     * 
     * @param dataList
     * @return
     */
    UAObs getHeaderData(List<IBUFRDataPacket> dataList) {
        UAObs obsData = null;

        Calendar obsTime = getTimeInfo(dataList);
        if (obsTime != null) {
            obsData = new UAObs();
            obsData.setValidTime(obsTime);
        }
        if (isValidTime(obsData)) {

        }

        return obsData;
    }

    /**
     * Checks that the observation time is not in the future.
     * 
     * @param report
     *            The UAObs observation report to check.
     * @return Is the report time valid?
     */
    private boolean isValidTime(UAObs report) {
        // boolean isValid = false;
        // if (report != null) {
        // Calendar curr = TimeTools.getSystemCalendar();
        // curr.add(Calendar.HOUR_OF_DAY, -2);
        //
        // long delta = report.getRefHour().getTimeInMillis()
        // - curr.getTimeInMillis() - ALLOWABLE_TIME;
        //
        // isValid = (delta <= 0);
        // }
        return true;
    }

    /**
     * 
     * @param dataList
     * @return
     */
    Calendar getTimeInfo(List<IBUFRDataPacket> dataList) {

        int year = getInt(dataList.get(YEAR_POS), IDecoderConstants.VAL_MISSING);
        int month = getInt(dataList.get(MONTH_POS),
                IDecoderConstants.VAL_MISSING);
        int day = getInt(dataList.get(DAY_POS), IDecoderConstants.VAL_MISSING);
        int hour = getInt(dataList.get(HOUR_POS), IDecoderConstants.VAL_MISSING);
        int minute = getInt(dataList.get(MINUTE_POS),
                IDecoderConstants.VAL_MISSING);

        Calendar baseTime = null;

        // Ensure that we have all of the time info and create the
        // date-time and datatime info.
        if ((year > 0) && (month > 0) && (day > 0) && (hour >= 0)
                && (minute >= 0)) {
            if (year < 100) {
                // NWS data doesn't have the century.
                // I'm making a BIG assumption here.
                if ((year >= 0) && (year < 80)) {
                    year += 2000;
                } else {
                    year += 1900;
                }
            }
            baseTime = TimeTools.getBaseCalendar(year, month, day);
            baseTime.set(Calendar.HOUR_OF_DAY, hour);
            baseTime.set(Calendar.MINUTE, minute);
            baseTime.set(Calendar.SECOND, 0);
            baseTime.set(Calendar.MILLISECOND, 0);
        }
        return baseTime;
    }

    Integer getWMOStationId(List<IBUFRDataPacket> dataList) {
        Integer wmoStaId = null;

        int block = getInt(dataList.get(0), -1);
        int station = getInt(dataList.get(1), -1);
        if ((block > 0) && (station > 0)) {
            wmoStaId = (block * 1000) + station;
        }
        return wmoStaId;
    }

    /**
     * Check that a value is between a defined range of value (inclusive).
     * 
     * @param loValue
     *            The lowest valid value.
     * @param hiValue
     *            The highest valid value.
     * @param value
     *            The value to range check.
     * @return The value if valid, otherwise returns null.
     */
    public static Double checkRange(Double loValue, Double hiValue, Double value) {
        Double retValue = null;
        if (value != null) {
            if ((loValue <= value) && (value <= hiValue)) {
                retValue = value;
            }
        }
        return retValue;
    }

    public static final void main(String[] args) {

    }

}
