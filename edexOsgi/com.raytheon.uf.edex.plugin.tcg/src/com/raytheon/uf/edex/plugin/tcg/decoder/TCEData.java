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
import java.util.List;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.tcg.TCGStormType;
import com.raytheon.uf.common.dataplugin.tcg.TropicalCycloneGuidance;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.plugin.tcg.TropicalCycloneGuidanceDao;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 12, 2009            jsanchez    Initial creation
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Jun 24, 2014 3235       nabowle     InternalReport patterns are now
 *                                     precompiled.
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class TCEData extends TCGDataAdapter {

    private static final Pattern STATION_ID_PATTERN = Pattern.compile("(\\w{2,2}\\d{6,6})");

    private static final Pattern REF_HOUR_PATTERN = Pattern.compile("(\\d{4,4})");

    private float latitude = -9999;

    private float longitude = -9999;

    private int month;

    private int year;

    private int day;

    private int hour;

    private int minute;

    public TCEData(PointDataDescription pdd, TropicalCycloneGuidanceDao dao,
            String pluginName) {
        super(pdd, dao, pluginName);
    }

    @Override
    public List<TropicalCycloneGuidance> findReports(byte[] message) {
        List<TropicalCycloneGuidance> reports = new ArrayList<TropicalCycloneGuidance>();
        List<InternalReport> parts = InternalReport.identifyMessage(message);
        if (parts != null) {
            clearData();
            for (InternalReport iRpt : parts) {
                InternalType t = iRpt.getLineType();
                String s = iRpt.getReportLine();
                if (InternalType.PRODUCT.equals(t)) {
                    productType = s;
                } else if (InternalType.STORM_TYPE_INFO.equals(t)) {
                    parseStormTypeInfo(s);
                } else if (InternalType.STATIONID.equals(t)) {
                    parseStationIdInfo(s);
                } else if (InternalType.INIT_TIME_INFO.equals(t)) {
                    parseInitTimeInfo(s);
                } else if (InternalType.TCE_REFHOUR.equals(t)) {
                    parseRefhour(s);
                } else if (InternalType.LATITUDE.equals(t)) {
                    parseLatitude(s);
                } else if (InternalType.LONGITUDE.equals(t)) {
                    parseLongitude(s);
                } else if (InternalType.END.equals(t)) {
                    if ((latitude != -9999) && (longitude != -9999)) {
                        TropicalCycloneGuidance rpt = new TropicalCycloneGuidance();
                        SurfaceObsLocation location = new SurfaceObsLocation(
                                stationId);
                        location.setLongitude(longitude);
                        location.setLatitude(latitude);
                        rpt.setWmoHeader(wmoHeader.getWmoHeader());
                        rpt.setTraceId(traceId);
                        rpt.setStormName(stormName);
                        rpt.setType(stormType);
                        rpt.setProductType(productType);
                        rpt.setLocation(location);
                        rpt.setInsertTime(Calendar.getInstance(TimeZone
                                .getTimeZone("GMT")));
                        refTime = getDataTime(year, month, day, hour, minute,
                                "GMT");
                        DataTime dt = new DataTime(
                                refTime.getRefTimeAsCalendar());
                        rpt.setDataTime(dt);
                        reports.add(rpt);
                    }
                    clearData();
                }
            }
        }
        return reports;
    }

    private void parseStormTypeInfo(String stormTypeInfo) {
        int index = setStormType(stormTypeInfo);
        if (index != -1) {
            String temp[] = stormTypeInfo.substring(index).trim().split(" ");
            stormName = temp[0];
        }
    }

    private void parseStationIdInfo(String stationIdInfo) {
        Matcher m = STATION_ID_PATTERN.matcher(stationIdInfo);
        if (m.find()) {
            stationId = m.group();
        }
    }

    private void parseInitTimeInfo(String initTimeInfo) {
        String parts[] = getParts(initTimeInfo, 7);
        month = MONTH_MAP.get(parts[4]);
        day = Integer.parseInt(parts[5]);
        year = Integer.parseInt(parts[6]);
    }

    private void parseRefhour(String refhourInfo) {
        Matcher m = REF_HOUR_PATTERN.matcher(refhourInfo);
        if (m.find()) {
            hour = Integer.parseInt(m.group().substring(0, 2));
            minute = Integer.parseInt(m.group().substring(2));
        }
    }

    private void parseLatitude(String latitudeInfo) {
        Matcher m = InternalReport.LAT_PTRN.matcher(latitudeInfo);
        if (m.find()) {
            latitude = Float.parseFloat(m.group().substring(8).trim());
            if (latitudeInfo.contains("SOUTH")) {
                latitude *= -1;
            }
        }

    }

    private void parseLongitude(String longitudeInfo) {
        Matcher m = InternalReport.LON_PTRN.matcher(longitudeInfo);
        if (m.find()) {
            longitude = Float.parseFloat(m.group().substring(10).trim());
            if (longitudeInfo.contains("WEST")) {
                longitude *= -1;
            } else if (longitudeInfo.contains("SOUTH") && (latitude > 0)) {
                latitude *= -1;
            }
        }
    }

    @Override
    public void clearData() {
        latitude = -9999;
        longitude = -9999;
        stationId = null;
        stormType = TCGStormType.UNKNOWN;
        stormName = null;
        refTime = null;
    }
}
