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
package com.raytheon.uf.common.dataplugin.tcs.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.tcs.Radius;
import com.raytheon.uf.common.dataplugin.tcs.TropicalCycloneSummary;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2010            jsanchez     Initial creation
 * Jul 23, 2014 3410       bclement    location changed to floats
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class Util implements TCSConstants {
    private static final Pattern tcmWindRadius = Pattern
            .compile(".*((\\d{2}) KT...|12 FT SEAS.)\\W{0,}\\s{0,}(\\d{1,3})NE\\s{1,}(\\d{1,3})SE\\s{1,}(\\d{1,3})SW\\s{1,}(\\d{1,3})NW.");

    private static final Pattern tcpWindRadius = Pattern
            .compile(".*(\\s{1,}(\\d{3}) KT WINDS|12 FT SEAS)");

    private static final Pattern quadrantPtrn = Pattern
            .compile(".*(\\d{3}) NM\\s{1,}(NORTHEAST|SOUTHEAST|SOUTHWEST|NORTHWEST).*");

    public static TropicalCycloneSummary interplateStorm(PointDataView pdv,
            DataTime dataTime) {
        TropicalCycloneSummary tcs = null;

        float t1 = 0.0f, t2 = 0.0f, tf = 0.0f, dt = 0.0f, tc = 0.0f;

        Calendar calendar = dataTime.getRefTimeAsCalendar();
        int day = dataTime.getValidTime().get(Calendar.DAY_OF_MONTH);
        int hour = dataTime.getValidTime().get(Calendar.HOUR_OF_DAY);

        boolean tropical = true;
        int wind = 0;
        float lat = 29.0f; // This came from TCMplotter.C
        float lon = -89.6f; // This came from TCMplotter.C
        String dTime = "Dissipated";

        int size = pdv.getInt(SIZE);
        int pressure = pdv.getInt(PRESSURE);
        String name = pdv.getString(NAME);
        Number[] isTropical = pdv.getNumberAllLevels(TROPICAL);
        Number[] windSpeed = pdv.getNumberAllLevels(WIND_SPEED);
        Number[] longitude = pdv.getNumberAllLevels(LON);
        Number[] latitude = pdv.getNumberAllLevels(LAT);
        String[] displayTime = pdv.getStringAllLevels(DISPLAY_TIME);
        if (size < 2) {
            if (size > 0) {
                // set location at last one
                lat = latitude[size - 1].floatValue();
                lon = longitude[size - 1].floatValue();
            }
            return new TropicalCycloneSummary(name, 0, lon, lat, dTime,
                    windSpeed[size - 1].intValue(),
                    isTropical[size - 1].intValue() == 1 ? true : false);
        }

        // Searching two storms with the time, s1 and s2 in the list.
        // The time is between s1.time and s2.time. We need not do the
        // interplattion
        // if time == s1.time or time == s2.time.

        // Convert the given forecast time to hours.
        tf = dataTime.getValidTime().get(Calendar.HOUR_OF_DAY);

        t1 = Integer.parseInt(displayTime[0].split("\\.")[1]);

        // fore cast time is 0
        if (dataTime.getValidTime().getTimeInMillis() == dataTime.getRefTime()
                .getTime()) {
            tcs = new TropicalCycloneSummary(name, pressure,
                    longitude[0].floatValue(), latitude[0].floatValue(),
                    displayTime[0], windSpeed[0].intValue(),
                    isTropical[0].intValue() == 1 ? true : false);
            tcs.setRadiusList(loadRadiusList(pdv, 0));
            return tcs;
        }

        int i = 1;
        while (i < size) {

            dt = Integer.parseInt(displayTime[i - 1].split("\\.")[1]);
            t2 = Integer.parseInt(displayTime[i].split("\\.")[1]);

            dt = t2 - dt; // dt is the delta between dt and t2
            if (dt <= 0) {
                dt += 24;
            }

            t2 = t1 + dt;

            tc = Math.abs(t2 - tf);

            if (tc < 0.01) {
                tcs = new TropicalCycloneSummary(name, 0,
                        longitude[i].floatValue(), latitude[i].floatValue(),
                        displayTime[i], windSpeed[i].intValue(),
                        isTropical[i].intValue() == 1 ? true : false);
                tcs.setRadiusList(loadRadiusList(pdv, i));
                return tcs;
            } else if (t2 < tf) {
                t1 = t2; // shifting the base or t1 time
            } else {
                break;
            }
            i++;
        }

        // None matched, return empty
        if (i >= size) {
            if (size > 0) {
                // set location at last one
                lat = latitude[size - 1].floatValue();
                lon = longitude[size - 1].floatValue();
            }
            return new TropicalCycloneSummary(name, 0, lon, lat, dTime,
                    windSpeed[size - 1].intValue(),
                    isTropical[size - 1].intValue() == 1 ? true : false);
        }

        tc = (t2 - tf) / (t2 - t1);
        if (tc > 1 && t2 == 24) {
            tc = -tf / (t2 - t1);
        }
        // Create the "dd.hh" string of the given time
        dt = tf - t1;

        int isTrop = (dt / 2.0 + t1) >= tf ? isTropical[i].intValue()
                : isTropical[i - 1].intValue();
        tropical = isTrop == 1 ? true : false;

        t1 = Integer.parseInt(displayTime[i - 1].split("\\.")[1]);
        day = Integer.parseInt(displayTime[i - 1].split("\\.")[0]);
        hour = (int) (t1 + dt + 0.5);
        if (hour > 24) {
            hour = hour - 24;
            calendar.set(Calendar.DAY_OF_MONTH,
                    Integer.parseInt(displayTime[i].split("\\.")[0]));
            calendar.set(Calendar.HOUR_OF_DAY,
                    Integer.parseInt(displayTime[i].split("\\.")[1]));
            day++;
        }

        dTime = day + "." + (hour < 10 ? "0" + hour : hour);

        // Interplating with the t1, t2 and tm.

        // Location
        lat = latitude[i].floatValue() - tc
                * (latitude[i].floatValue() - latitude[i - 1].floatValue());
        lon = longitude[i].floatValue() - tc
                * (longitude[i].floatValue() - longitude[i - 1].floatValue());

        // Wind speed
        wind = (int) (windSpeed[i].intValue() - tc
                * (windSpeed[i].intValue() - windSpeed[i - 1].intValue()) + 0.6);

        // Loading the radius
        // lists[0] = i; lists[i] = i - 1
        ArrayList<ArrayList<Radius>> lists = new ArrayList<ArrayList<Radius>>();
        for (int j = 0; j < 2; j++) {
            lists.add(loadRadiusList(pdv, i - j));
        }

        // Wind radius, we use longer one of the _list[i-1].radiusList(t1) and
        // _list[i].radiusList(t2) to loop. The distance is "0" if a wind radius
        // is not exsiting in another list. How about it is "-1"?, we will
        // consider
        // it latter.
        int j = 0, rx1, rx2;
        Radius radius = new Radius();
        ArrayList<Radius> radiusList = new ArrayList<Radius>();
        if (lists.get(0).size() > lists.get(1).size()) {
            while (j < lists.get(0).size()) {
                radius = new Radius();
                radius.setKT_FT(lists.get(0).get(j).getKT_FT());
                if (radius.getKT_FT() == 12 || wind < radius.getKT_FT()) {
                    j++;
                    continue;
                }
                radius.setKFUnit('K');
                rx2 = lists.get(0).get(j).getKT_FT();
                rx1 = getAradius(lists.get(1), rx2, 0);
                rx2 = lists.get(0).get(j).getNE();
                radius.setNE((int) (rx2 - tc * (rx2 - rx1) + 0.5));

                rx1 = getAradius(lists.get(1), lists.get(0).get(j).getKT_FT(),
                        1);
                rx2 = lists.get(0).get(j).getNW();
                radius.setNW((int) (rx2 - tc * (rx2 - rx1) + 0.5));

                rx1 = getAradius(lists.get(1), lists.get(0).get(j).getKT_FT(),
                        2);
                rx2 = lists.get(0).get(j).getSE();
                radius.setSE((int) (rx2 - tc * (rx2 - rx1) + 0.5));

                rx1 = getAradius(lists.get(1), lists.get(0).get(j).getKT_FT(),
                        3);
                rx2 = lists.get(0).get(j).getSW();
                radius.setSW((int) (rx2 - tc * (rx2 - rx1) + 0.5));

                radiusList.add(radius);
                j++;
            }
        } else {
            while (j < lists.get(1).size()) {
                radius = new Radius();
                radius.setKT_FT(lists.get(1).get(j).getKT_FT());
                if (radius.getKT_FT() == 12 || wind < radius.getKT_FT()) {
                    j++;
                    continue;
                }

                radius.setKFUnit('K');
                rx2 = getAradius(lists.get(0), lists.get(1).get(j).getKT_FT(),
                        0);
                rx1 = lists.get(1).get(j).getNE();
                radius.setNE((int) (rx2 - tc * (rx2 - rx1) + 0.5));

                rx2 = getAradius(lists.get(0), lists.get(1).get(j).getKT_FT(),
                        1);
                rx1 = lists.get(1).get(j).getNW();
                radius.setNW((int) (rx2 - tc * (rx2 - rx1) + 0.5));

                rx2 = getAradius(lists.get(0), lists.get(1).get(j).getKT_FT(),
                        2);
                rx1 = lists.get(1).get(j).getSE();
                radius.setSE((int) (rx2 - tc * (rx2 - rx1) + 0.5));

                rx2 = getAradius(lists.get(0), lists.get(1).get(j).getKT_FT(),
                        3);
                rx1 = lists.get(1).get(j).getSW();
                radius.setSW((int) (rx2 - tc * (rx2 - rx1) + 0.5));

                radiusList.add(radius);
                j++;
            }
        }
        tcs = new TropicalCycloneSummary(name, 0, lon, lat, dTime, wind,
                tropical);
        tcs.setRadiusList(radiusList);
        return tcs;
    }

    private static ArrayList<Radius> loadRadiusList(PointDataView pdv, int index) {
        // Loading the radius
        int COLUMN_WIDTH = 4;
        String[] radiusNames = { RAD_64, RAD_50, RAD_34, RAD_12 };
        ArrayList<Radius> radiusList = new ArrayList<Radius>();

        for (String radiusName : radiusNames) {
            char unit = radiusName.equals(RAD_12) ? 'F' : 'K';
            Number[] radiusSet = pdv.getNumberAllLevels(radiusName);

            if (radiusSet[(index * COLUMN_WIDTH)] != null) {
                Radius rad = new Radius(unit, Integer.parseInt(radiusName
                        .substring(6, 8)),
                        radiusSet[((index) * COLUMN_WIDTH)].intValue(),
                        radiusSet[((index) * COLUMN_WIDTH) + 1].intValue(),
                        radiusSet[((index) * COLUMN_WIDTH) + 2].intValue(),
                        radiusSet[((index) * COLUMN_WIDTH) + 3].intValue());
                radiusList.add(rad);
            }
        }

        return radiusList;
    }

    private static int getAradius(ArrayList<Radius> radiusList, int speed,
            int dir) {
        int j = 0;
        int radius = 0;

        while (j < radiusList.size()) {
            if (radiusList.get(j).getKT_FT() == speed) {
                switch (dir) {
                case 0: // D_NE:
                    radius = radiusList.get(j).getNE();
                    break;
                case 1: // D_NW:
                    radius = radiusList.get(j).getNW();
                    break;
                case 2: // D_SE:
                    radius = radiusList.get(j).getSE();
                    break;
                case 3: // D_SW:
                    radius = radiusList.get(j).getSW();
                    break;
                }
                break;
            }
            j++;
        }

        return radius < 0 ? 0 : radius;
    }

    public static boolean isWindRadius(String productType, String line,
            Radius radius) {
        if (productType == null) {
            return false;
        } else if (productType.startsWith("TCM")) {
            return isTCMWindRadius(line, radius);
        } else if (productType.equals("TCP")) {
            return isTCPWindRadius(line, radius);
        }
        return false;
    }

    private static boolean isTCMWindRadius(String line, Radius radius) {
        Matcher m = tcmWindRadius.matcher(line);
        if (m.find()) {
            if (line.contains("12 FT SEAS.")) {
                radius.setKT_FT(12);
                radius.setKFUnit('F');
            } else if (m.group(2) != null) {
                radius.setKT_FT(Integer.parseInt(m.group(2)));
                radius.setKFUnit('K');
            } else {
                return false;
            }
            radius.setNE(Integer.parseInt(m.group(3)));
            radius.setSE(Integer.parseInt(m.group(4)));
            radius.setSW(Integer.parseInt(m.group(5)));
            radius.setNW(Integer.parseInt(m.group(6)));
            return true;
        }
        return false;
    }

    private static boolean isTCPWindRadius(String line, Radius radius) {
        Matcher m;
        if (radius.getKFUnit() == 'x') {
            m = tcpWindRadius.matcher(line);
            if (m.find()) {
                if (line.contains("12 FT SEAS")) {
                    radius.setKT_FT(12);
                    radius.setKFUnit('F');
                } else if (m.group(2) != null) {
                    radius.setKFUnit('K');
                    radius.setKT_FT(Integer.parseInt(m.group(2)));
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }

        m = quadrantPtrn.matcher(line);
        if (m.find()) {
            int i = Integer.parseInt(m.group(1));
            if (m.group(2).equals("NORTHEAST")) {
                radius.setNE(i);
            } else if (m.group(2).equals("SOUTHEAST")) {
                radius.setSE(i);
            } else if (m.group(2).equals("SOUTHWEST")) {
                radius.setSW(i);
            } else if (m.group(2).equals("NORTHWEST")) {
                radius.setNW(i);
            } else {
                radius.setNE(i);
                radius.setSE(i);
                radius.setSW(i);
                radius.setNW(i);
            }
        }

        return true;
    }

}
