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
 * Oct 26, 2009            jsanchez    Initial creation
 * Jun 28, 2012 826        dgilling    Use wmoHeader headerDate to  set refTime
 *                                     so times are set  correctly when
 *                                     processing archive  data.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class QLMData extends TCGDataAdapter {

    private class ForecastPosition {
        public int hour;

        public float latitude;

        public float longitude;

        public ForecastPosition(int hour, float lat, float lon) {
            this.hour = hour;
            this.latitude = lat;
            this.longitude = lon;
        }
    }

    private final List<ForecastPosition> list = new ArrayList<ForecastPosition>();

    private static final int MAX_FORECASTS = 22;

    public QLMData(PointDataDescription pdd, TropicalCycloneGuidanceDao dao,
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
                } else if (InternalType.INIT_TIME_INFO.equals(t)) {
                    parseInitTimeInfo(s);
                } else if (InternalType.FORECAST_POSITION_INFO.equals(t)) {
                    parseForecastPositionInfo(s);
                } else if (InternalType.STORM_DISSIPATED.equals(t)
                        || (list.size() == MAX_FORECASTS)) {
                    boolean firstValue = true;
                    for (ForecastPosition fp : list) {
                        TropicalCycloneGuidance rpt = new TropicalCycloneGuidance();
                        SurfaceObsLocation location = new SurfaceObsLocation(
                                stationId);
                        location.setLongitude((float) fp.longitude);
                        location.setLatitude((float) fp.latitude);

                        rpt.setWmoHeader(wmoHeader.getWmoHeader());
                        rpt.setTraceId(traceId);
                        rpt.setStormName(stormName);
                        rpt.setType(stormType);
                        rpt.setProductType(productType);
                        rpt.setLocation(location);
                        rpt.setInsertTime(Calendar.getInstance(TimeZone
                                .getTimeZone("GMT")));
                        DataTime dt;
                        if (firstValue) {
                            firstValue = false;
                            dt = new DataTime(refTime.getRefTimeAsCalendar());
                        } else {
                            dt = new DataTime(refTime.getRefTimeAsCalendar(),
                                    fp.hour * 3600);
                        }
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

    private void parseInitTimeInfo(String initTimeInfo) {
        String data[] = getParts(initTimeInfo.substring(12), 3);

        int hour = Integer.valueOf(data[0].substring(0, data[0].length() - 1));
        int minute = 0;
        int month = MONTH_MAP.get(data[1]);
        int day = Integer.valueOf(data[2]);
        Calendar cal = wmoHeader.getHeaderDate();
        int year = cal.get(Calendar.YEAR);

        refTime = getDataTime(year, month, day, hour, minute, "GMT");
    }

    private void parseForecastPositionInfo(String positionInfo) {

        String data[] = getParts(positionInfo, 3);

        int hour = Integer.valueOf(data[0]);
        float lat = Float.valueOf(data[1]);
        float lon = Float.valueOf(data[2]) * -1;
        list.add(new ForecastPosition(hour, lat, lon));
    }

    @Override
    public void clearData() {
        list.clear();
        stationId = null;
        stormType = TCGStormType.UNKNOWN;
        stormName = null;
        refTime = null;
    }
}
