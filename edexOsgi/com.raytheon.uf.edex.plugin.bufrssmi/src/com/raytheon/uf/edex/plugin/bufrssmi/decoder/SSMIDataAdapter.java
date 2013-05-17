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
package com.raytheon.uf.edex.plugin.bufrssmi.decoder;

import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.RepSubList;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.bufrssmi.SSMIScanData;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.bufrtools.BUFRPointDataAdapter;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
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
 * Jul 06, 2009 2538       jsanchez    Added latitude,longitude to point data.
 * May 17, 2013 1869       bsteffen    Remove DataURI column from sat plot
 *                                     types.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SSMIDataAdapter extends BUFRPointDataAdapter<SSMIScanData> {

    private static final int YEAR_POS = 1;

    private static final int MONTH_POS = 2;

    private static final int DAY_POS = 3;

    private static final int HOUR_POS = 4;

    private static final int MINUTE_POS = 5;

    private static final int SECOND_POS = 6;

    private static final int SAT_ID_POS = 0;

    private static final int ORBIT_N_POS = 7;

    private static final int SCAN_N_POS = 8;

    /**
     * 
     * @param container
     */
    public SSMIDataAdapter(PointDataDescription pdd,
            PointDataPluginDao<SSMIScanData> dao, String pluginName) {
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
    public SSMIScanData createData(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {

        SSMIScanData obsData = null;

        BUFRDataDocument dataDoc = iterator.next();
        if (dataDoc != null) {
            // Get the primary data list.
            List<IBUFRDataPacket> dataList = dataDoc.getList();

            // Extract the header data.
            obsData = getHeaderData(dataList);

            if (obsData != null) {
                // Need to set plugin name before getting container!
                obsData.setPluginName(getPluginName());
                PointDataContainer container = getContainer(obsData);
                if (container != null) {
                    PointDataView view = container.append();
                    //
                    setViewData("satelliteID", view, dataList.get(SAT_ID_POS));
                    setViewData("orbitNumber", view, dataList.get(ORBIT_N_POS));
                    setViewData("scanNumber", view, dataList.get(SCAN_N_POS));

                    view.setLong("validTime", obsData.getTimeObs()
                            .getTimeInMillis());

                    obsData.setPointDataView(view);
                    obsData.setWmoHeader(wmoHeader.getWmoHeader());
                }
            }
        }

        return obsData;
    }

    /**
     * 
     */
    @SuppressWarnings("unchecked")
    public List<SSMIScanData> createDataList(
            Iterator<BUFRDataDocument> iterator, WMOHeader wmoHeader) {

        List<SSMIScanData> obsList = null;

        SSMIScanData obsData = null;

        BUFRDataDocument dataDoc = iterator.next();
        if (dataDoc != null) {
            // Get the primary data list.
            List<IBUFRDataPacket> dataList = dataDoc.getList();

            // Extract the header data.
            obsData = getHeaderData(dataList);
            obsData.setWmoHeader(wmoHeader.getWmoHeader());

            if (obsData != null) {

                obsList = new ArrayList<SSMIScanData>();
                logger.debug("Created master observation for scanline");
                // Now go get the point data for this obs
                // get the location and data sublists.
                IBUFRDataPacket p1 = dataList.get(9);
                IBUFRDataPacket p2 = dataList.get(10);
                List<IBUFRDataPacket> locList = null;
                List<IBUFRDataPacket> pointList = null;
                if ((p1 instanceof BUFRSublistPacket)
                        && (p1 instanceof BUFRSublistPacket)) {
                    if (RepSubList.getPacketType().equals(p1.getUnits())) {
                        locList = (List<IBUFRDataPacket>) p1.getValue();
                    }
                    if (RepSubList.getPacketType().equals(p2.getUnits())) {
                        pointList = (List<IBUFRDataPacket>) p2.getValue();
                    }
                }
                if ((locList != null) && (pointList != null)) {
                    logger.info("Decoding " + locList.size() + " observations");
                    Iterator<IBUFRDataPacket> it = pointList.iterator();
                    for (IBUFRDataPacket p : locList) {
                        // We have the obs data that describes the scan line.
                        // Make a copy for each data point.
                        SSMIScanData pointData = obsData.copyObs();
                        // Need to set plugin name before getting container!
                        pointData.setPluginName(getPluginName());
                        pointData = getPointData(pointData, p, it.next());
                        if (pointData != null) {
                            obsList.add(pointData);
                        }
                    }
                }
            }
        }

        return obsList;
    }

    /**
     * 
     * @param dataList
     * @return
     */
    private SSMIScanData getHeaderData(List<IBUFRDataPacket> dataList) {
        SSMIScanData obsData = null;

        Calendar obsTime = getTimeInfo(dataList);
        if (obsTime != null) {
            obsData = new SSMIScanData();
            obsData.setTimeObs(obsTime);
            obsData.setDataTime(new DataTime(TimeTools.copy(obsTime)));

            int satId = getInt(dataList.get(SAT_ID_POS),
                    IDecoderConstants.VAL_MISSING);
            int orbNo = getInt(dataList.get(ORBIT_N_POS),
                    IDecoderConstants.VAL_MISSING);
            int scnNo = getInt(dataList.get(SCAN_N_POS),
                    IDecoderConstants.VAL_MISSING);

            logger.info("getHeaderData(" + satId + "," + orbNo + "," + scnNo
                    + ")" + obsData.getDataTime());

            if (satId != IDecoderConstants.VAL_MISSING) {
                obsData.setSatId(satId);
                if (orbNo != IDecoderConstants.VAL_MISSING) {
                    obsData.setOrbitNumber(orbNo);
                    if (scnNo != IDecoderConstants.VAL_MISSING) {
                        obsData.setScanNumber(scnNo);
                    } else {
                        obsData = null;
                    }
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

    /**
     * 
     * @param pointData
     * @param locPoint
     * @param dataPoint
     * @param index
     */
    @SuppressWarnings("unchecked")
    private SSMIScanData getPointData(SSMIScanData pointData,
            IBUFRDataPacket locPoint, IBUFRDataPacket dataPoint) {

        if (locPoint instanceof BUFRSublistPacket) {
            if (dataPoint instanceof BUFRSublistPacket) {
                List<IBUFRDataPacket> locList = (List<IBUFRDataPacket>) locPoint
                        .getValue();
                List<IBUFRDataPacket> datList = (List<IBUFRDataPacket>) dataPoint
                        .getValue();

                PointDataContainer container = getContainer(pointData);
                if (container != null) {

                    logger.debug("Creating obs scanline obs data ");

                    PointDataView view = container.append();
                    //
                    view.setInt("orbitNumber", pointData.getOrbitNumber());
                    view.setInt("scanNumber", pointData.getScanNumber());
                    view.setLong("validTime", pointData.getTimeObs()
                            .getTimeInMillis());

                    double lat = getDouble(locList.get(0),
                            IDecoderConstants.VAL_MISSING);
                    double lon = getDouble(locList.get(1),
                            IDecoderConstants.VAL_MISSING);

                    SurfaceObsLocation location = new SurfaceObsLocation();
                    location.assignLocation(lat, lon);
                    location.generateCoordinateStationId();
                    pointData.setLocation(location);
                    setViewData("surfaceTag", view, locList.get(2));
                    int posNum = getInt(locList.get(3), -9999);
                    view.setInt("positionNumber", posNum);
                    pointData.setPosNumber(posNum);

                    setViewData("windSpeed", view, datList.get(0));
                    setViewData("precipWater", view, datList.get(1));
                    setViewData("vilMetFeatureSignif", view, datList.get(2));
                    setViewData("vertIntegWater", view, datList.get(3));
                    setViewData("seaTemp", view, datList.get(5));

                    pointData.setPointDataView(view);
                }
            }
        }
        return pointData;
    }

}
