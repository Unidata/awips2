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
package com.raytheon.uf.edex.plugin.bufrascat.decoder;

import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.bufrascat.AScatObs;
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
 * May 17, 2013 1869       bsteffen    Remove DataURI column from sat plot
 *                                     types.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class AScatDataAdapter extends BUFRPointDataAdapter<AScatObs> {
    // Note inverted logic
    private static final int RAIN_FLAG_NOT_USABLE = 0x00001000;

    private static final int RAIN_FLAG = 0x00002000;

    private static final int RAIN_DATA_MASK = RAIN_FLAG_NOT_USABLE | RAIN_FLAG;

    private static final int RAIN_NOT_PRESENT = 0x00000000;

    private static final int RAIN_PRESENT = RAIN_FLAG;

    private static final int YEAR_POS = 2;

    private static final int MONTH_POS = 3;

    private static final int DAY_POS = 4;

    private static final int HOUR_POS = 5;

    private static final int MINUTE_POS = 6;

    private static final int SECOND_POS = 7;

    private static final int SAT_ID_POS = 0;

    private static final int ORBIT_N_POS = 1;

    /**
     * 
     * @param pdd
     * @param dao
     * @param pluginName
     */
    public AScatDataAdapter(PointDataDescription pdd,
            PointDataPluginDao<AScatObs> dao, String pluginName) {
        super(pdd, dao, pluginName);
    }

    /**
     * Construct a ProfilerObs instance from the BUFR decoded data contained in
     * the specified separator.
     * 
     * @param iterator
     *            An iterator containing decoded BUFR data.
     * @param wmoHeader
     *            a wmoHeader
     * @return A ProfilerObs instance, or null in the event of an error.
     */
    @Override
    public AScatObs createData(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {

        AScatObs obsData = null;

        BUFRDataDocument dataDoc = iterator.next();

        if (dataDoc != null) {
            // Get the primary data list.
            List<IBUFRDataPacket> dataList = dataDoc.getList();

            obsData = getHeaderData(dataList);
            if (obsData != null) {
                // pickup the data.
                obsData.setPluginName(getPluginName());
                obsData.setWmoHeader(wmoHeader.getWmoHeader());
                PointDataContainer container = getContainer(obsData);
                if (container != null) {

                    logger.debug("Creating obs data ");

                    PointDataView view = container.append();
                    //
                    view.setLong("validTime", obsData.getValidTime()
                            .getTimeInMillis());

                    double lat = getDouble(dataList.get(8),
                            IDecoderConstants.VAL_MISSING);
                    double lon = getDouble(dataList.get(9),
                            IDecoderConstants.VAL_MISSING);

                    SurfaceObsLocation location = new SurfaceObsLocation();
                    location.assignLocation(lat, lon);
                    location.generateCoordinateStationId();
                    obsData.setLocation(location);

                    int rainFlag = getInt(dataList.get(10),
                            IDecoderConstants.VAL_MISSING);
                    switch ((rainFlag & RAIN_DATA_MASK)) {
                    case RAIN_NOT_PRESENT: {
                        rainFlag = 0;
                        break;
                    }
                    case RAIN_PRESENT: {
                        rainFlag = 1;
                        break;
                    }
                    default: { //
                        rainFlag = -9999;
                        break;
                    }
                    }
                    view.setInt("rainIndex", rainFlag);
                    setViewData("probRain", view, dataList.get(11));
                    // Wind data
                    float val = (float) getDouble(dataList.get(12),
                            PDV_FILL_DBL);
                    if (val < 0) {
                        val = (float) PDV_FILL_DBL;
                    }
                    obsData.setWindSpd(val);
                    val = (float) getDouble(dataList.get(13), PDV_FILL_DBL);
                    if (val < 0) {
                        val = (float) PDV_FILL_DBL;
                    }
                    view.setFloat("windDir", val);

                    obsData.setPointDataView(view);
                }
            }
        }

        return obsData;
    }

    /**
     * 
     */
    @Override
    public List<AScatObs> createDataList(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {
        List<AScatObs> obsList = null;

        return obsList;
    }

    /**
     * 
     * @param dataList
     * @return
     */
    private AScatObs getHeaderData(List<IBUFRDataPacket> dataList) {
        AScatObs obsData = null;

        Calendar obsTime = getTimeInfo(dataList);
        if (obsTime != null) {
            obsData = new AScatObs();
            obsData.setValidTime(obsTime);
            obsData.setDataTime(new DataTime(TimeTools.copy(obsTime)));

            int satId = getInt(dataList.get(SAT_ID_POS),
                    IDecoderConstants.VAL_MISSING);
            int orbNo = getInt(dataList.get(ORBIT_N_POS),
                    IDecoderConstants.VAL_MISSING);

            logger.debug("getHeaderData(" + satId + "," + orbNo + ")"
                    + obsData.getDataTime());

        }
        return obsData;
    }

    /**
     * 
     * @param dataList
     * @return
     */
    private Calendar getTimeInfo(List<IBUFRDataPacket> dataList) {

        int year = getInt(dataList.get(YEAR_POS), IDecoderConstants.VAL_MISSING);
        int month = getInt(dataList.get(MONTH_POS),
                IDecoderConstants.VAL_MISSING);
        int day = getInt(dataList.get(DAY_POS), IDecoderConstants.VAL_MISSING);
        int hour = getInt(dataList.get(HOUR_POS), IDecoderConstants.VAL_MISSING);
        int minute = getInt(dataList.get(MINUTE_POS),
                IDecoderConstants.VAL_MISSING);
        int second = getInt(dataList.get(SECOND_POS),
                IDecoderConstants.VAL_MISSING);

        Calendar baseTime = null;

        // Ensure that we have all of the time info and create the
        // date-time and datatime info.
        if ((year > 0) && (month > 0) && (day > 0) && (hour >= 0)
                && (minute >= 0) && (second >= 0)) {
            baseTime = TimeTools.getBaseCalendar(year, month, day);
            baseTime.set(Calendar.HOUR_OF_DAY, hour);
            baseTime.set(Calendar.MINUTE, minute);
            baseTime.set(Calendar.SECOND, second);
            baseTime.set(Calendar.MILLISECOND, 0);
        }
        return baseTime;
    }
}
