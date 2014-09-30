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
package com.raytheon.uf.edex.plugin.tcs.decoder;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.tcs.Radius;
import com.raytheon.uf.common.dataplugin.tcs.TropicalCycloneSummary;
import com.raytheon.uf.common.dataplugin.tcs.util.Util;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.plugin.tcs.TropicalCycloneSummaryDao;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2010            jsanchez    Initial creation
 * Apr 19, 2012 457        dgilling    Use TimeTools.findDataTime()  to
 *                                     calculate times.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * May 14, 2014 2536       bclement    moved WMO Header to common, removed TimeTools usage
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class TCMData extends TCSDataAdapter {

    private static final Pattern productTypePtrn = Pattern
            .compile("TCM(AT|EP|CP|WP)[1-5]");

    private static final Pattern stormNamePtrn = Pattern
            .compile("(TROPICAL STORM|TROPICAL DEPRESSION|HURRICANE|TYPHOON)\\s{1,}(\\w{1,})");

    private static final Pattern pressurePtrn = Pattern
            .compile("(.*)PRESSURE\\s{1,}(\\d{1,5})(.*)");

    private static final Pattern windPtrn = Pattern
            .compile(".*(MAX WIND|MAX SUSTAINED WINDS)\\s{1,}(\\d{1,}).*");

    private static final Pattern timePtrn = Pattern
            .compile(".*((\\d{2})(/|)(\\d{2})(\\d{2}))Z.*");

    private static final Pattern latlonPtrn = Pattern
            .compile(".*\\s{1,}(\\d{1,2}.\\d{1})(N|S)\\s{1,}(\\d{1,3}.\\d{1})(E|W)");

    // private static final Pattern datatimePtrn = Pattern
    // .compile("(\\d{2})(\\d{2})\\s{1,}\\w{3}\\s{1,}\\w{3}\\s{1,}(\\w{3})\\s{1,}(\\d{1,2})\\s{1,}(\\d{4})");

    private static final Pattern windRadiiPtrn = Pattern
            .compile("\\s{1,}WIND RADII VALID OVER OPEN WATER ONLY");

    private static final HashMap<String, Integer> MONTH_MAP = new HashMap<String, Integer>();

    static {
        MONTH_MAP.put("JAN", 0);
        MONTH_MAP.put("FEB", 1);
        MONTH_MAP.put("MAR", 2);
        MONTH_MAP.put("APR", 3);
        MONTH_MAP.put("MAY", 4);
        MONTH_MAP.put("JUN", 5);
        MONTH_MAP.put("JUL", 6);
        MONTH_MAP.put("AUG", 7);
        MONTH_MAP.put("SEP", 8);
        MONTH_MAP.put("OCT", 9);
        MONTH_MAP.put("NOV", 10);
        MONTH_MAP.put("DEC", 11);
    }

    private static final int MINIMUM_LINES = 5;

    private String name;

    private int pressure = 0;

    private DataTime refTime;

    public TCMData(PointDataDescription pdd, TropicalCycloneSummaryDao dao,
            String pluginName) {
        super(pdd, dao, pluginName);
    }

    @Override
    public List<TropicalCycloneSummary> findReports(byte[] message) {
        boolean isExtraTropical = false;
        boolean isLocation = false;
        String time;
        String type = null;
        Matcher m;
        Radius radius = new Radius();
        TropicalCycloneSummary storm = new TropicalCycloneSummary();
        ArrayList<TropicalCycloneSummary> stormList = new ArrayList<TropicalCycloneSummary>();

        List<String> lines = separateLines(message);
        if (lines.size() < MINIMUM_LINES) {
            // Insufficient input text
            return stormList;
        }

        for (String line : lines) {
            // TCP (from Navy) format is different from TCM (from TPC)
            if (type == null) {
                m = productTypePtrn.matcher(line);
                if (m.find()) {// TCMATx, TCMCPx, TCMEPx
                    type = m.group(0);
                } else if (line.startsWith("WTPN3")) {// WTPN3x
                    type = "TCP";
                }
            }

            if ((name == null) || (name.length() == 0)) {
                m = stormNamePtrn.matcher(line);
                if (m.find()) {
                    name = m.group(2);
                    continue;
                }
            }

            // if (refTime == null) {
            // m = datatimePtrn.matcher(line);
            // if (m.find()) {
            // calendar = Calendar.getInstance(TimeZone
            // .getTimeZone(TIMEZONE));
            // calendar.set(Calendar.HOUR_OF_DAY,
            // Integer.parseInt(m.group(1)));
            // calendar.set(Calendar.MINUTE, Integer.parseInt(m.group(2)));
            // calendar.set(Calendar.SECOND, 0);
            // calendar.set(Calendar.MILLISECOND, 0);
            // calendar.set(Calendar.MONTH, MONTH_MAP.get(m.group(3)));
            // calendar.set(Calendar.DAY_OF_MONTH,
            // Integer.parseInt(m.group(4)));
            // calendar.set(Calendar.YEAR, Integer.parseInt(m.group(5)));
            // refTime = new DataTime(calendar);
            // continue;
            // }
            // }

            if (line.contains("REMARKS")) {
                break;
            }
            // Searching for the press
            if ((pressure == 0) && line.contains(" PRESSURE ")) {
                m = pressurePtrn.matcher(line);
                if (m.find()) {
                    pressure = Integer.parseInt(m.group(2));
                    if ((pressure < 800) || (pressure > 1050)) {
                        pressure = 0;
                    }
                    continue;
                }

            }

            if (line.contains("MAX WIND ")
                    || line.contains("MAX SUSTAINED WINDS ")) {
                m = windPtrn.matcher(line.replace("-", ""));
                if (m.find()) {
                    int wind = Integer.parseInt(m.group(2));
                    storm.setWindSpeed((wind > 0) && (wind < 250) ? wind : 0);
                    continue;
                }
            }

            if (line.contains("EXTRATROPICAL")) {
                isExtraTropical = true;
            }

            m = windRadiiPtrn.matcher(line);
            if (m.find()) {
                continue;
            }
            String mask = line;
            // Searching for the wind radius
            if (Util.isWindRadius(type, mask, radius)) {
                if ((radius.getKFUnit() != 'x') && (radius.getKT_FT() != -1)
                        && (radius.getNE() != -1) && (radius.getSE() != -1)
                        && (radius.getSW() != -1) && (radius.getNW() != -1)) {

                    boolean exist = false;
                    ArrayList<Radius> radiusList = storm.getRadiusList();
                    if (radiusList == null) {
                        radiusList = new ArrayList<Radius>();
                    }
                    for (Radius r : radiusList) {
                        if (r.getKT_FT() == radius.getKT_FT()) {
                            exist = true;
                            break;
                        }
                    }

                    if (!exist) {
                        radiusList.add(radius);
                    }
                    radius = new Radius();
                    storm.setRadiusList(radiusList);
                }
                continue;
            }

            if (isLocation && !storm.getDisplayTime().equals("")
                    && (storm.getWindSpeed() != 0)) {
                storm.setTropical(!isExtraTropical);
                storm.setName(name);
                storm.setPressure(pressure);
                storm.setProductType(type.toString());
                stormList.add(storm);

                // Reset values
                isLocation = false;
                storm = new TropicalCycloneSummary();
            }

            int fcstTime = 0;
            // Searching for a time
            m = timePtrn.matcher(mask);
            if (m.find()) {
                time = m.group(2) + "." + m.group(4);
                String fileName = (String) headers
                        .get(WMOHeader.INGEST_FILE_NAME);
                Calendar calendar = WMOTimeParser.findDataTime(
                        m.group(2) + m.group(4) + m.group(5), fileName);
                DataTime fcastTime = new DataTime(calendar);
                if (refTime == null) {
                    refTime = new DataTime(fcastTime.getRefTimeAsCalendar());
                }
                fcstTime = (int) ((fcastTime.getValidTime().getTimeInMillis() - refTime
                        .getValidTime().getTimeInMillis()) / 1000L);
            } else {
                continue;
            }

            // Do nothing if the time is existing in the list
            int k = 0;
            for (k = 0; k < stormList.size(); k++) {
                if (stormList.get(k).getDisplayTime().equals(time)) {
                    break;
                }
            }
            if ((k < stormList.size()) || storm.getDisplayTime().equals(time)) {
                continue;
            }

            // Searching for the location
            m = latlonPtrn.matcher(mask);
            if (m.find()) {
                SurfaceObsLocation location = new SurfaceObsLocation();
                float latitude = Float.parseFloat(((m.group(1))));
                float longitude = Float.parseFloat((m.group(3)));
                location.setLatitude(m.group(2).equals("S") ? -1 * latitude
                        : latitude);
                location.setLongitude(m.group(4).equals("W") ? -1 * longitude
                        : longitude);
                location.setStationId(name);
                storm.setLocation(location);
                storm.setDisplayTime(time);

                storm.setDataTime(fcstTime == 0 ? refTime : new DataTime(
                        refTime.getRefTimeAsCalendar(), fcstTime));
                isLocation = true;
            }

        }
        return stormList;
    }

    /**
     * 
     * @param message
     * @return
     */
    private static List<String> separateLines(byte[] message) {
        List<String> reportLines = null;

        if (message != null) {
            BufferedReader reader = null;
            try {
                reader = new BufferedReader(new InputStreamReader(
                        new ByteArrayInputStream(message)));
                String s;
                reportLines = new ArrayList<String>();
                while ((s = reader.readLine()) != null) {
                    if (s.length() > 0) {
                        reportLines.add(s);
                    }
                }
            } catch (Exception e) {
                logger.error("Error reading from reader", e);
            } finally {
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException ioe) {
                        logger.error("Error closing reader", ioe);
                    }
                }
            }
        }
        return reportLines;
    }

}
