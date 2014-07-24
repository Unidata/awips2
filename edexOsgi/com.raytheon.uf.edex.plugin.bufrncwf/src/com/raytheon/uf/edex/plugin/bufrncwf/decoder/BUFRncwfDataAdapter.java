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
package com.raytheon.uf.edex.plugin.bufrncwf.decoder;

import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.RepSubList;
import static com.raytheon.uf.edex.decodertools.bufr.packets.DataPacketTypes.SubSetList;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.bufrncwf.BUFRncwf;
import com.raytheon.uf.common.dataplugin.bufrncwf.NCWFFeature;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.bufrtools.BUFRPointDataAdapter;
import com.raytheon.uf.edex.decodertools.bufr.BUFRDataDocument;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRSublistPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Point data view framework adapter for National Convective Weather Forecast
 * Product
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2009            jkorman     Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed TimeTools usage
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BUFRncwfDataAdapter extends BUFRPointDataAdapter<BUFRncwf> {

    // Position within sub-list
    private static final int YEAR_POS = 0;

    private static final int MONTH_POS = 1;

    private static final int DAY_POS = 2;

    private static final int HOUR_POS = 3;

    private static final int MINUTE_POS = 4;

    private static final int SECOND_POS = 5;

    private static final int FEATURES_POS = 6;

    // private static final int FEATURE_TIMSIG_POS = 0;

    // private static final int FEATURE_TIMEPD_POS = 1;

    // private static final int FEATURE_ATTSIG_POS = 2;

    // private static final int FEATURE_DIMSIG_POS = 3;

    private static final int FEATURE_LAT_POS = 4;

    private static final int FEATURE_LON_POS = 5;

    // private static final int BOUNDARY_ATTSIG_POS = 6;

    // private static final int BOUNDARY_DIMSIG_POS = 7;

    private static final int BOUNDARY_SUBLIST_POS = 8;

    private static final int VERTEX_LAT_POS = 0;

    private static final int VERTEX_LON_POS = 1;

    private static final int STORM_TOP_POS = 7;

    private static final int STORM_DIR_POS = 8;

    private static final int STORM_SPD_POS = 9;

    private static final int INVALID = -9999;

    /**
     * 
     * @param pdd
     * @param dao
     * @param pluginName
     */
    public BUFRncwfDataAdapter(PointDataDescription pdd,
            PointDataPluginDao<BUFRncwf> dao, String pluginName) {
        super(pdd, dao, pluginName);
    }

    /**
     * Not used.
     * 
     * @see com.raytheon.uf.edex.bufrtools.BUFRPointDataAdapter#createData(Iterator,
     *      WMOHeader)
     */
    @Override
    public BUFRncwf createData(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {
        return null;
    }

    /**
     * 
     * 
     * 
     * 
     * @see com.raytheon.uf.edex.bufrtools.BUFRPointDataAdapter#createDataList(Iterator,
     *      WMOHeader)
     */
    @Override
    public List<BUFRncwf> createDataList(Iterator<BUFRDataDocument> iterator,
            WMOHeader wmoHeader) {

        List<BUFRncwf> reports = new ArrayList<BUFRncwf>();

        BUFRDataDocument dataDoc = iterator.next();
        if (dataDoc != null) {
            // Get the primary data list.
            List<IBUFRDataPacket> dataList = dataDoc.getList();
            for (IBUFRDataPacket p : dataList) {
                if (p != null) {
                    List<IBUFRDataPacket> sList = getPacketSubList(p);
                    for (IBUFRDataPacket pp : sList) {
                        BUFRncwf ncwfReport = getReport(pp);

                        if (ncwfReport != null) {
                            PointDataContainer container = getContainer(ncwfReport);
                            if (container != null) {
                                PointDataView view = container.append();

                                long vt = ncwfReport.getDataTime()
                                        .getValidTime().getTimeInMillis();
                                view.setLong("validTime", vt);

                                view.setFloat("storm_top", ncwfReport
                                        .getStormTop().floatValue());
                                view.setFloat("storm_dir", ncwfReport
                                        .getStormDir().floatValue());
                                view.setFloat("storm_speed", ncwfReport
                                        .getStormSpeed().floatValue());

                                NCWFFeature detect = ncwfReport.getDetection();
                                view.setFloat("or_centroid_lat",
                                        (float) detect.getCentroidLatitude());
                                view.setFloat("or_centroid_lon",
                                        (float) detect.getCentroidLongitude());
                                int pos = 0;
                                for (Coordinate dc : detect
                                        .getFeatureBoundary()) {
                                    float lat = (float) DecoderTools
                                            .getCoordinateLatitude(dc);
                                    float lon = (float) DecoderTools
                                            .getCoordinateLongitude(dc);
                                    view.setFloat("oalat", lat, pos);
                                    view.setFloat("oalon", lon, pos++);
                                }
                                view.setInt("or_num_of_vertices", pos);
                                NCWFFeature forecast = ncwfReport.getForecast();
                                view.setFloat("centroid_lat",
                                        (float) forecast.getCentroidLatitude());
                                view.setFloat("centroid_lon",
                                        (float) forecast.getCentroidLongitude());
                                pos = 0;
                                for (Coordinate dc : forecast
                                        .getFeatureBoundary()) {
                                    float lat = (float) DecoderTools
                                            .getCoordinateLatitude(dc);
                                    float lon = (float) DecoderTools
                                            .getCoordinateLongitude(dc);
                                    view.setFloat("alat", lat, pos);
                                    view.setFloat("alon", lon, pos++);
                                }
                                view.setInt("num_of_vertices", pos);

                                ncwfReport.setPointDataView(view);
                                reports.add(ncwfReport);
                            }
                        }
                    }
                }
            }
        }
        return reports;
    }

    @SuppressWarnings("unchecked")
    private BUFRncwf getReport(IBUFRDataPacket packet) {
        BUFRncwf ncwfReport = null;
        if (packet != null) {
            List<IBUFRDataPacket> sList = (List<IBUFRDataPacket>) packet
                    .getValue();

            Calendar c = getTimeInfo(sList);
            if (c != null) {
                ncwfReport = new BUFRncwf();

                ncwfReport.setDataTime(new DataTime((Calendar) c.clone()));
                List<IBUFRDataPacket> features = getPacketSubList(sList
                        .get(FEATURES_POS));

                ncwfReport
                        .setDetection(getFeatureData(getPacketSubList(features
                                .get(0))));
                ncwfReport.setForecast(getFeatureData(getPacketSubList(features
                        .get(1))));

                double height = getDouble(sList.get(STORM_TOP_POS), INVALID);
                if (height >= 0) {
                    height = Math.floor((height * 3.280839) / 100);
                }
                ncwfReport.setStormTop(height);
                ncwfReport.setStormDir(getDouble(sList.get(STORM_DIR_POS),
                        INVALID));
                ncwfReport.setStormSpeed(getDouble(sList.get(STORM_SPD_POS),
                        INVALID));
                if (ncwfReport.getDetection() != null) {
                    Coordinate centroid = ncwfReport.getDetection()
                            .getCentroidLocation();
                    SurfaceObsLocation loc = new SurfaceObsLocation();
                    float lat = (float) DecoderTools
                            .getCoordinateLatitude(centroid);
                    float lon = (float) DecoderTools
                            .getCoordinateLongitude(centroid);
                    loc.assignLocation(lat, lon);
                    ncwfReport.setLocation(loc);
                }
            }
        }
        return ncwfReport;
    }

    private NCWFFeature getFeatureData(List<IBUFRDataPacket> featureList) {
        NCWFFeature feature = null;
        if ((featureList != null) && (featureList.size() > 0)) {
            // 0 0 08 021 : CODE TABLE Comments: Time significance
            // 1 0 04 026 : Second Comments: Time period or displacement
            // 2 0 08 005 : Comments: Meteorological attribute significance
            // 3 0 08 007 : Comments: Dimensional significance
            // 4 0 05 001 : Comments: Latitude (high accuracy)
            // 5 0 06 001 : Comments: Longitude (high accuracy)
            // 6 0 08 005 : Comments: Meteorological attribute significance
            // 7 0 08 007 : Comments: Dimensional significance
            // 8 1 01 000 : Comments: Boundary-Sublist

            double lat = getDouble(featureList.get(FEATURE_LAT_POS), INVALID);
            double lon = getDouble(featureList.get(FEATURE_LON_POS), INVALID);
            if ((lat != INVALID) && (lon != INVALID)) {
                feature = new NCWFFeature(lat, lon);

                IBUFRDataPacket p1 = featureList.get(BOUNDARY_SUBLIST_POS);
                feature.setFeatureBoundary(getVertices(getPacketSubList(p1)));
            }
        }
        return feature;
    }

    /**
     * 
     * @param vertexList
     * @return
     */
    private List<Coordinate> getVertices(List<IBUFRDataPacket> vertexList) {
        List<Coordinate> vertices = new ArrayList<Coordinate>();
        if (vertexList != null) {
            for (IBUFRDataPacket p : vertexList) {
                List<IBUFRDataPacket> vertex = getPacketSubList(p);
                double lat = getDouble(vertex.get(VERTEX_LAT_POS), INVALID);
                double lon = getDouble(vertex.get(VERTEX_LON_POS), INVALID);
                if ((lat != INVALID) && (lon != INVALID)) {
                    vertices.add(DecoderTools.createCoordinate(lat, lon));
                }
            }
        }
        return vertices;
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
            baseTime = TimeUtil.newGmtCalendar(year, month, day);
            baseTime.set(Calendar.HOUR_OF_DAY, hour);
            baseTime.set(Calendar.MINUTE, minute);
            baseTime.set(Calendar.SECOND, second);
            baseTime.set(Calendar.MILLISECOND, 0);
        }
        return baseTime;
    }

    /**
     * 
     * @param packet
     * @return
     */
    @SuppressWarnings("unchecked")
    private static List<IBUFRDataPacket> getPacketSubList(IBUFRDataPacket packet) {
        List<IBUFRDataPacket> list = null;
        if (packet instanceof BUFRSublistPacket) {
            if (RepSubList.getPacketType().equals(packet.getUnits())) {
                list = (List<IBUFRDataPacket>) packet.getValue();
            } else if (SubSetList.getPacketType().equals(packet.getUnits())) {
                list = (List<IBUFRDataPacket>) packet.getValue();
            }
        }
        return list;
    }

}
