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
package com.raytheon.uf.common.dataplugin.shef.util;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.SimpleTimeZone;
import java.util.TimeZone;


public class SHEFTimezone {
    public static final HashMap<String, TimeZone> sysTimeZones = new HashMap<String, TimeZone>();
    
    public static final int TZ_NOTFOUND = -9999;

    public static final TimeZone GMT_TIMEZONE = TimeZone
            .getTimeZone(ShefConstants.Z);

    private static final HashMap<String, TimeZoneEntry> timeZoneMap = new HashMap<String, TimeZoneEntry>();
    
    static {
        timeZoneMap.put("Z", new TimeZoneEntry("Z", 0, null,
                "Zulu time, also Greenwich Mean Time (GMT)"));
        timeZoneMap.put(
                "N",
                new TimeZoneEntry("N", 210, TimeZone
                        .getTimeZone("Canada/Newfoundland"),
                        "Newfoundland local time"));
        timeZoneMap.put(
                "NS",
                new TimeZoneEntry("NS", 210, TimeZone
                        .getTimeZone("Canada/Newfoundland"),
                        "Newfoundland standard time"));
        // timeZoneMap.put("ND",new TimeZoneEntry("ND",
        // 210,false,"Newfoundland daylight time"));
        timeZoneMap
                .put("A",
                        new TimeZoneEntry("A", 240, TimeZone
                                .getTimeZone("Canada/Atlantic"),
                                "Atlantic local time"));
        timeZoneMap.put("AS", new TimeZoneEntry("AS", 240, null,
                "Atlantic standard time"));
        timeZoneMap.put("AD", new TimeZoneEntry("AD", 180, null,
                "Atlantic daylight time"));
        timeZoneMap.put("E",
                new TimeZoneEntry("E", 300, TimeZone.getTimeZone("US/Eastern"),
                        "Eastern local time"));
        timeZoneMap.put("ES", new TimeZoneEntry("ES", 300, null,
                "Eastern standard time"));
        timeZoneMap.put("ED", new TimeZoneEntry("ED", 240, null,
                "Eastern daylight time"));
        timeZoneMap.put("C",
                new TimeZoneEntry("C", 360, TimeZone.getTimeZone("US/Central"),
                        "Central local time"));
        timeZoneMap.put("CS", new TimeZoneEntry("CS", 360, null,
                "Central standard time"));
        timeZoneMap.put("CD", new TimeZoneEntry("CD", 300, null,
                "Central daylight time"));
        timeZoneMap.put("M",
                new TimeZoneEntry("M", 420,
                        TimeZone.getTimeZone("US/Mountain"),
                        "Mountain local time"));
        timeZoneMap.put("MS", new TimeZoneEntry("MS", 420, null,
                "Mountain standard time"));
        timeZoneMap.put("MD", new TimeZoneEntry("MD", 360, null,
                "Mountain daylight time"));
        timeZoneMap.put("P",
                new TimeZoneEntry("P", 480, TimeZone.getTimeZone("US/Pacific"),
                        "Pacific local time"));
        timeZoneMap.put("PS", new TimeZoneEntry("PS", 480, null,
                "Pacific standard time "));
        timeZoneMap.put("PD", new TimeZoneEntry("PD", 420, null,
                "Pacific daylight time "));
        timeZoneMap.put(
                "Y",
                new TimeZoneEntry("Y", 540, TimeZone
                        .getTimeZone("Canada/Yukon"), "Yukon local time"));
        timeZoneMap.put("YS", new TimeZoneEntry("YS", 480, null,
                "Yukon standard time"));
        timeZoneMap.put("YD", new TimeZoneEntry("YD", 420, null,
                "Yukon daylight time"));
        timeZoneMap.put("L",
                new TimeZoneEntry("L", 540, TimeZone.getTimeZone("US/Alaska"),
                        "Alaskan local time"));
        timeZoneMap.put("LS", new TimeZoneEntry("LS", 540, null,
                "Alaskan standard time"));
        timeZoneMap.put("LD", new TimeZoneEntry("LD", 480, null,
                "Alaskan daylight time"));
        timeZoneMap.put("H",
                new TimeZoneEntry("H", 600, TimeZone.getTimeZone("US/Hawaii"),
                        "Hawaiian local time"));
        timeZoneMap.put("HS",
                new TimeZoneEntry("HS", 600, TimeZone.getTimeZone("US/Hawaii"),
                        "Hawaiian standard time"));
        // This doesn't actually exist. But its in the test data.
        timeZoneMap.put("HD", new TimeZoneEntry("HD", 600, null,
                "Hawaiian daylight time"));
        timeZoneMap.put(
                "B",
                new TimeZoneEntry("B", 660, TimeZone
                        .getTimeZone("America/Adak"), "Bering local time"));
        timeZoneMap.put("BS", new TimeZoneEntry("BS", 660, null,
                "Bering standard time"));
        timeZoneMap.put("BD", new TimeZoneEntry("BD", 600, null,
                "Bering daylight time"));
        timeZoneMap.put("J",
                new TimeZoneEntry("J", -480, TimeZone.getTimeZone("PRC"),
                        "China"));
        timeZoneMap.put(
                "G",
                new TimeZoneEntry("G", -600, TimeZone
                        .getTimeZone("Pacific/Guam"), "Chamorro local time"));
        timeZoneMap
                .put("GS",
                        new TimeZoneEntry("GS", -600, TimeZone
                                .getTimeZone("Pacific/Guam"),
                                "Chamorro standard time"));
        timeZoneMap.put(
                "S",
                new TimeZoneEntry("S", 660, TimeZone
                        .getTimeZone("Pacific/Samoa"), "Samoan local time"));
        timeZoneMap.put(
                "SS",
                new TimeZoneEntry("SS", 660, TimeZone
                        .getTimeZone("Pacific/Samoa"), "Samoan standard time"));
    }

    static {
        // Use this timezone to "clone" the daylight savings time rules.
        for (String key : timeZoneMap.keySet()) {
            TimeZoneEntry tze = timeZoneMap.get(key);
            TimeZone tz = null;
            int tzOffset = -tze.getOffsetMinutes() * 60 * 1000;
            if (tze.getDaylightSaving() != null) {
                tz = (TimeZone) tze.getDaylightSaving().clone();
                tz.setID(tze.getKey());
            } else {
                tz = new SimpleTimeZone(tzOffset, tze.getKey());
            }
            sysTimeZones.put(key, tz);
        }
    }

    public static TimeZone getSysTimeZone(String timeZoneCode) {
        return sysTimeZones.get(timeZoneCode);
    }

    /**
     * Return the time zone offset (in minutes) from GMT for a given time zone
     * code.
     * 
     * @param timeZoneCode
     * @return The time zone offset in minutes.
     */
    public static int getTimezoneOffset(String timeZoneCode) {
        int zoneOffset = TZ_NOTFOUND;
        if (timeZoneMap.containsKey(timeZoneCode)) {
            zoneOffset = timeZoneMap.get(timeZoneCode).getOffsetMinutes();
        }

        return zoneOffset;
    }

    public static boolean isValidTimezone(String tz) {
        return timeZoneMap.containsKey(tz);
    }

    public static TimeZoneEntry getTimeZone(String key) {
        return timeZoneMap.get(key);
    }
    
    
    public static final void main(String [] args) {
        
        TimeZone tz = getSysTimeZone("C");
        GregorianCalendar c = new GregorianCalendar(tz);

        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
        sdf.setTimeZone(TimeZone.getTimeZone("Zulu"));

        for(int h = 0;h < 12;h++) {
            c.set(Calendar.YEAR, 1989);
            c.set(Calendar.MONTH, Calendar.OCTOBER);
            c.set(Calendar.DAY_OF_MONTH, 29);
            c.set(Calendar.HOUR_OF_DAY, h);
            c.set(Calendar.MINUTE, 0);
            c.set(Calendar.SECOND, 0);
            c.set(Calendar.MILLISECOND, 0);
            
            System.out.println(String.format("hour = %2d time = %s", h, sdf.format(c.getTime())));
        }

        
//        String [] zones = TimeZone.getAvailableIDs();
//        for(String s : zones) {
//            tz = TimeZone.getTimeZone(s);
//            if(tz.getRawOffset() == -32400000L) {
//                System.out.println(tz);
//            }
//        
//        }
        
        
        
        
    }
    
    
    
    
    
    
    
}
