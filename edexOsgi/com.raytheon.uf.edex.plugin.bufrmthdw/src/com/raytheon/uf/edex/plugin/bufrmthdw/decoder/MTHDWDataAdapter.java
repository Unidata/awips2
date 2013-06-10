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
package com.raytheon.uf.edex.plugin.bufrmthdw.decoder;

import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.bufrmthdw.BUFRMTHDWSatType;
import com.raytheon.uf.common.dataplugin.bufrmthdw.BufrMTHDWObs;
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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2010            jkorman     Initial creation
 * May 17, 2013 1869       bsteffen    Remove DataURI column from sat plot
 *                                     types.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class MTHDWDataAdapter extends BUFRPointDataAdapter<BufrMTHDWObs> {

    private static final int YEAR_POS = 5;

    private static final int MONTH_POS = 6;

    private static final int DAY_POS = 7;

    private static final int HOUR_POS = 8;

    private static final int MINUTE_POS = 9;

    private static final int SECOND_POS = 10;

    private static final int SAT_ID_POS = 0;

    /**
     * 
     * @param container
     */
    public MTHDWDataAdapter(PointDataDescription pdd,
            PointDataPluginDao<BufrMTHDWObs> dao, String pluginName) {
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
    public BufrMTHDWObs createData(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {

        BufrMTHDWObs obsData = null;

        BUFRDataDocument dataDoc = iterator.next();
        if (dataDoc != null) {
            // Get the primary data list.
            List<IBUFRDataPacket> dataList = dataDoc.getList();

            // Extract the header data.
            if ((obsData = getHeaderData(dataList)) != null) {
                // Need to set plugin name before getting container!
                obsData.setWmoHeader(wmoHeader.getWmoHeader());
                obsData.setPluginName(getPluginName());
                obsData = getPointData(obsData, dataList);
            }
        }

        return obsData;
    }

    /**
     * 
     */
    @Override
    public List<BufrMTHDWObs> createDataList(
            Iterator<BUFRDataDocument> iterator, WMOHeader wmoHeader) {
        List<BufrMTHDWObs> obsList = null;

        return obsList;
    }

    /**
     * 
     * @param dataList
     * @return
     */
    private BufrMTHDWObs getHeaderData(List<IBUFRDataPacket> dataList) {
        BufrMTHDWObs obsData = null;

        Calendar obsTime = getTimeInfo(dataList);
        if (obsTime != null) {
            obsData = new BufrMTHDWObs();
            obsData.setValidTime(obsTime);
            obsData.setDataTime(new DataTime(TimeTools.copy(obsTime)));

            double satId = getDouble(dataList.get(SAT_ID_POS),
                    IDecoderConstants.VAL_MISSING);
            if (satId != IDecoderConstants.VAL_MISSING) {
                obsData.setSatelliteID(satId);
                double freq = getDouble(dataList.get(18),
                        IDecoderConstants.VAL_MISSING);
                int wndType = getInt(dataList.get(14),
                        IDecoderConstants.VAL_MISSING);

                BUFRMTHDWSatType type = BUFRMTHDWSatType.getType(freq);
                if (!BUFRMTHDWSatType.ERROR.equals(type)) {
                    obsData.setSatType(type);
                } else {
                    obsData = null;
                }
            } else {
                obsData = null;
            }
        }
        return obsData;
    }

    /**
     * Get the time information. If any is missing return a null reference.
     * 
     * @param dataList
     *            The packet list containing the time info.
     * @return A calendar reference for the time info, or null.
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

    /**
     * Get the point data for this observation. If the pressure data is not
     * present, then return a null reference.
     * 
     * @param pointData
     * @param dataList
     */
    private BufrMTHDWObs getPointData(BufrMTHDWObs pointData,
            List<IBUFRDataPacket> dataList) {

        PointDataContainer container = getContainer(pointData);
        if (container != null) {

            logger.debug("Creating obs point data ");

            PointDataView view = container.append();
            //
            view.setFloat("satelliteID", pointData.getSatelliteID()
                    .floatValue());
            view.setLong("validTime", pointData.getValidTime()
                    .getTimeInMillis());

            double lat = getDouble(dataList.get(11),
                    IDecoderConstants.VAL_MISSING);
            double lon = getDouble(dataList.get(12),
                    IDecoderConstants.VAL_MISSING);

            SurfaceObsLocation location = new SurfaceObsLocation();
            location.setLatitude(lat);
            location.setLongitude(lon);
            location.generateCoordinateStationId();
            pointData.setLocation(location);
            // ****************************************
            // Now pickup the rest of the data.
            setViewData("originatingID", view, dataList.get(1));
            setViewData("satelliteClass", view, dataList.get(2));
            setViewData("sgmtSzX", view, dataList.get(3));
            setViewData("sgmtSzY", view, dataList.get(4));

            setViewData("satelliteInstr", view, dataList.get(13));
            setViewData("satelliteWindMethod", view, dataList.get(14));
            setViewData("windDir", view, dataList.get(16));
            setViewData("windSpd", view, dataList.get(17));
            setViewData("satelliteFreq", view, dataList.get(18));
            setViewData("satelliteBandWidth", view, dataList.get(19));
            setViewData("coldestTemp", view, dataList.get(20));
            setViewData("heightMethod", view, dataList.get(21));
            setViewData("tracerCorrelation", view, dataList.get(22));
            setViewData("landSea", view, dataList.get(23));
            setViewData("satelliteZenith", view, dataList.get(24));
            setViewData("firstGuess", view, dataList.get(25));
            setViewData("timeSignificance", view, dataList.get(26));
            // ****************************************
            double pressure = getDouble(dataList.get(15),
                    IDecoderConstants.VAL_MISSING);
            pointData.setPressure(pressure);
            if (pressure > 0) {
                pointData.setPointDataView(view);
            } else {
                pointData = null;
            }
        }
        return pointData;
    }

}
