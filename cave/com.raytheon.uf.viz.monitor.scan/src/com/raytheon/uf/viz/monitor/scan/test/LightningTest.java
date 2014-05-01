package com.raytheon.uf.viz.monitor.scan.test;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;

public class LightningTest {

    /**
     * @param args
     */
    public static void main(String[] args) {

        // Copy output to a file called "AWIPS_..." and drop in the manual
        // directory

        String timeFmt = "";
        // "03/23/2010 13:35:01 72.00 -157.00 -14  1"

        int numberOfMinutes = 20;
        int numberOfStrikes = 1380;
        double southLatitude = 40;
        double easternLongitude = 94.5;

        double latitudeWidth = 3; // degrees
        double longitudeWidth = 3; // degrees

        int startMonth = 2; // 0 based, feb = 1
        int startDay = 14; // day of month
        int startHour = 20; // 24 hour clock
        int startMinute = 30;

        for (int i = 0; i < numberOfStrikes; i++) {
            Calendar c = new GregorianCalendar(TimeZone.getTimeZone("GMT"));
            c.set(Calendar.YEAR, 2011);
            c.set(Calendar.MONTH, startMonth);
            c.set(Calendar.DAY_OF_MONTH, startDay);
            c.set(Calendar.HOUR_OF_DAY, startHour);
            c.set(Calendar.MINUTE, startMinute);
            c.set(Calendar.SECOND, 0);
            c.set(Calendar.MILLISECOND, 0);

            long t = Math.round((Math.random() * numberOfMinutes) * 60 * 1000)
                    + c.getTimeInMillis();

            c.setTimeInMillis(t);

            double lat = (Math.random() * latitudeWidth) + southLatitude;
            double lon = easternLongitude + (Math.random() * longitudeWidth);
            double count = (Math.random() * 10) + 1;

            System.out
                    .println(String
                            .format("%1$tm/%1$te/%1$tY %1$tH:%1$tM:%1$tS %2$5.2f %3$7.2f -20 %4$2d",
                                    c, lat, -lon, Math.round(count)));

        }

    }

}
