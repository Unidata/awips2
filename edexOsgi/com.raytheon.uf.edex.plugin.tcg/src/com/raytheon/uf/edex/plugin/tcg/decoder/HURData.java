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
package com.raytheon.uf.edex.plugin.tcg.decoder;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.tcg.TCGStormType;
import com.raytheon.uf.common.dataplugin.tcg.TropicalCycloneGuidance;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.plugin.tcg.TropicalCycloneGuidanceDao;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Parses a subset of Tropical Cyclone Guidance data that contain multiple storm
 * track predictions from various models such as BAM(S/M/D) and LBAR.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2009            jsanchez    Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Jun 23, 2014 3235       nabowle     Clear the coordinates map properly.
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class HURData extends TCGDataAdapter {

    private final List<DataTime> forecastTimes = new ArrayList<DataTime>();

    private final HashMap<String, List<Coordinate>> coordinates = new HashMap<String, List<Coordinate>>();

    public HURData(PointDataDescription pdd, TropicalCycloneGuidanceDao dao,
            String pluginName) {
        super(pdd, dao, pluginName);
    }

    @Override
    public List<TropicalCycloneGuidance> findReports(byte[] message) {

        List<TropicalCycloneGuidance> reports = new ArrayList<TropicalCycloneGuidance>();
        List<InternalReport> parts = InternalReport.identifyMessage(message);
        List<Coordinate> list;

        if (parts != null) {
            clearData();
            for (InternalReport iRpt : parts) {
                InternalType t = iRpt.getLineType();
                String s = iRpt.getReportLine();
                if (InternalType.PRODUCT.equals(t)) {
                    productType = s;
                } else if (InternalType.DATA_INFO.equals(t)) {
                    parseDataInfo(s);
                } else if (InternalType.DATETIME_INFO.equals(t)) {
                    parseDateTimeInfo(s);
                } else if (InternalType.MODEL_INFO.equals(t)) {
                    parseModelInfo(s);
                } else if (InternalType.END.equals(t)) {
                    for (Entry<String, List<Coordinate>> entry : coordinates
                            .entrySet()) {
                        list = entry.getValue();
                        if (list.size() == forecastTimes.size()) {
                            for (int i = 0; i < list.size(); i++) {
                                TropicalCycloneGuidance rpt = new TropicalCycloneGuidance();
                                SurfaceObsLocation location = new SurfaceObsLocation(
                                        stationId);
                                location.setLongitude((float) list.get(i).x);
                                location.setLatitude((float) list.get(i).y);

                                rpt.setWmoHeader(wmoHeader.getWmoHeader());
                                rpt.setTraceId(traceId);
                                rpt.setStormName(stormName);
                                rpt.setType(stormType);
                                rpt.setProductType(productType);
                                rpt.setModelName(entry.getKey());
                                rpt.setLocation(location);
                                rpt.setInsertTime(Calendar.getInstance(TimeZone
                                        .getTimeZone("GMT")));
                                DataTime dt;
                                if (i != 0) {
                                    int fcstTime = new Double(
                                            (forecastTimes.get(i)
                                                    .getValidTime()
                                                    .getTimeInMillis() - refTime
                                                    .getValidTime()
                                                    .getTimeInMillis()) / 1000L)
                                            .intValue();
                                    dt = new DataTime(
                                            refTime.getRefTimeAsCalendar(),
                                            fcstTime);
                                } else {
                                    dt = new DataTime(
                                            refTime.getRefTimeAsCalendar());
                                }
                                rpt.setDataTime(dt);
                                reports.add(rpt);
                            }
                        }
                    }
                    clearData();
                }
            }
        }
        return reports;
    }

    private void parseModelInfo(String modelInfo) {
        Matcher m = InternalReport.MODEL_PTRN.matcher(modelInfo);
        if (m.find()) {
            String model = m.group();
            List<Coordinate> coordinate = coordinates.get(model);
            if (coordinate == null) {
                coordinate = new ArrayList<Coordinate>();
            }
            m = InternalReport.LATLON_PTRN.matcher(modelInfo);
            while (m.find()) {
                String latlon[] = m.group().split(" ");
                int n = latlon.length - 1;

                double lon = Double.valueOf(latlon[n].trim().substring(0,
                        latlon[n].length() - 1));
                double lat = Double.valueOf(latlon[0].trim().substring(0,
                        latlon[0].length() - 1));

                lon = latlon[n].charAt(latlon[n].length() - 1) == 'E' ? lon
                        : lon * -1;
                lat = latlon[0].charAt(latlon[0].length() - 1) == 'N' ? lat
                        : lat * -1;

                Coordinate c = new Coordinate(lon, lat);
                coordinate.add(c);
            }
            coordinates.put(model, coordinate);

        }
    }

    private void parseDateTimeInfo(String dateTimeInfo) {
        Pattern dateTimePtrn = Pattern.compile(InternalReport.DATETIME);
        Matcher m = dateTimePtrn.matcher(dateTimeInfo);
        while (m.find()) {
            // YYMMDD HHMM
            String yy = m.group().substring(0, 2);
            if (Integer.valueOf(yy) < 70) {
                yy = "20" + yy;
            } else {
                yy = "19" + yy;
            }
            int year = Integer.valueOf(yy);
            int month = Integer.valueOf(m.group().substring(2, 4));
            int day = Integer.valueOf(m.group().substring(4, 6));
            int hour = Integer.valueOf(m.group().substring(8, 10));
            int minute = Integer.valueOf(m.group().substring(10));
            forecastTimes
                    .add(getDataTime(year, month, day, hour, minute, "GMT"));
        }
    }

    private void parseDataInfo(String dataInfo) {
        Pattern stationIdPtrn = Pattern.compile(InternalReport.STATIONID_PTRN);
        Pattern refTimePtrn = Pattern.compile(InternalReport.REFTIME_PTRN);
        Matcher m = stationIdPtrn.matcher(dataInfo);

        if (m.find()) {
            stationId = m.group().substring(1, m.group().length() - 1);
        }

        m = refTimePtrn.matcher(dataInfo);
        if (m.find()) {
            // Reference Time
            // YYYYMMDD HHMM
            int year = Integer.valueOf(m.group().substring(0, 4));
            int month = Integer.valueOf(m.group().substring(4, 6));
            int day = Integer.valueOf(m.group().substring(6, 8));
            int hour = Integer.valueOf(m.group().substring(9, 11));
            int minute = Integer.valueOf(m.group().substring(11));
            refTime = getDataTime(year, month, day, hour, minute, "UTC");
        } else {
            refTime = new DataTime();
        }

        int index = setStormType(dataInfo);

        if (index != -1) {
            stormName = dataInfo.substring(index, dataInfo.indexOf("(")).trim();
        }
    }

    @Override
    public void clearData() {
        coordinates.clear();
        forecastTimes.clear();
        stationId = null;
        stormType = TCGStormType.UNKNOWN;
        stormName = null;
        refTime = null;
    }

}
