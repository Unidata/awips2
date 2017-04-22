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

package com.raytheon.viz.awipstools.common;

/**
 * SunriseSunsetCalculator.java
 * 
 * Class for calculating sunrise/sunset times, with azimuths. Ported from
 * original file. TclAlgorithms.C (Repository date 23May2003)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 08/15/07     426         Eric Babin    Initial Creation.
 * 05/25/10     5603        Bryan Kowal   Updated the 'getOffset(String)'
 *                                        function so that it would be
 *                                        capable of recognizing additional
 *                                        timezones.
 * Jul 28, 2015  4633       bsteffen       Remove println
 * </pre>
 * 
 * @author Eric Babin
 * @version 1
 */
public class SunriseSunsetCalculator {

    private String sunRiseAzimuth = "";

    private String sunSetAzimuth = "";

    private String zone = "0";

    private String sunriseTime = "";

    private String sunsetTime = "";

    private double longitude, latitude, sunriseAzimuth, sunsetAzimuth,
            timezone;

    private int year, month, day, sunriseHour, sunriseMinute, sunsetHour,
            sunsetMinute;

    private static double p2 = 6.28318530718;

    private static double dr = 0.01745329251994f;

    private static double k1 = 0.262516168343f;

    public SunriseSunsetCalculator() {

    }

    /**
     * Method for calculating sunrise, sunset time's with azimuths.
     * 
     * @param longitude
     *            Longitude to calculate
     * @param latitude
     *            Latitude to calculate
     * @param zone
     *            Time zone to calculate for
     * @param year
     *            Year to calculate for
     * @param month
     *            Month to calculate for
     * @param day
     *            Day to calculate for.
     */

    public void calculate(double longitude, double latitude, String zone,
            int year, int month, int day) {

        this.longitude = longitude;
        this.latitude = latitude;
        this.timezone = getOffSet(zone);
        this.year = year;
        this.month = month;
        this.day = day;
        this.zone = zone;

        calculateSunRiseSet();
    }

    /**
     * Method for determing offset given specific Timezone string.
     * 
     * @param zone
     *            String of timezone. Supports PST,PDT,MST,MDT,CST,CDT,EST,GMT,
     *            EDT,CHST,HST,AKST,AKDT only
     * @return offset increment
     */
    private double getOffSet(String zone) {

    	if (zone.equalsIgnoreCase("ChST")) {
    		return -10.0;
    	} else if (zone.equalsIgnoreCase("HST")) {
    		return 10.0;
    	} else if (zone.equalsIgnoreCase("AKST")) {
    		return 9.0;
    	} else if (zone.equalsIgnoreCase("PST") || zone.equalsIgnoreCase("AKDT")) {
            return 8.0;
        } else if (zone.equalsIgnoreCase("PDT") || zone.equalsIgnoreCase("MST")) {
            return 7.0;
        } else if (zone.equalsIgnoreCase("MDT") || zone.equalsIgnoreCase("CST")) {
            return 6.0;
        } else if (zone.equalsIgnoreCase("CDT") || zone.equalsIgnoreCase("EST")) {
            return 5.0;
        } else if (zone.equalsIgnoreCase("EDT") || zone.equalsIgnoreCase("AST")) {
            return 4.0;
        } else {
            return 0.0;
        }
    }

    /*
     * Method with the majority of calcuations.
     */
    private void calculateSunRiseSet() {

        double t, tt, t0, s, z0;
        double z1, c, z, w8, a0, da, dd;

        int c0;
        double m8, p, a2, d2, l0, l2, h0, h2, d0, d1, v0 = 0, v1, v2 = 0;
        double h1, b, A, D, e, t3, h3, m3, h7, n7, d7, az;

        longitude = longitude / 360;
        z0 = timezone / 24;

        t = (calToJul() - 2451545);
        tt = t / 36525 + 1;

        t0 = t / 36525;
        s = ((24110.5 + 8640184.813 * t0) + (86636.6 * z0 + 86400 * longitude)) / 86400;
        s = s - Math.floor(s);
        t0 = s * 360 * dr;
        t = t + z0;

        double returnValues1[] = fundArg(t, tt);

        t = t + 1;

        double returnValues2[] = fundArg(t, tt);

        if (returnValues2[0] < returnValues1[0]) {
            returnValues2[0] = returnValues2[0] + p2;
        }

        z1 = dr * 90.833;
        s = Math.sin(latitude * dr);
        c = Math.cos(latitude * dr);
        z = Math.cos(z1);

        m8 = 0;
        w8 = 0;
        a0 = returnValues1[0];
        d0 = returnValues1[1];
        da = returnValues2[0] - returnValues1[0];
        dd = returnValues2[1] - returnValues1[1];

        for (c0 = 0; c0 <= 23; c0++) {
            p = (c0 + 1) / 24.0;
            a2 = returnValues1[0] + p * da;
            d2 = returnValues1[1] + p * dd;
            l0 = t0 + c0 * k1;
            l2 = l0 + k1;
            h0 = l0 - a0;
            h2 = l2 - a2;
            h1 = (h2 + h0) / 2;
            d1 = (d2 + d0) / 2;

            if (c0 <= 0) {
                v0 = s * Math.sin(d0) + c * Math.cos(d0) * Math.cos(h0) - z;
            }
            v2 = s * Math.sin(d2) + c * Math.cos(d2) * Math.cos(h2) - z;

            if (Math.signum(v0) != Math.signum(v2)) {
                v1 = s * Math.sin(d1) + c * Math.cos(d1) * Math.cos(h1) - z;
                A = 2 * v2 - 4 * v1 + 2 * v0;
                b = 4 * v1 - 3 * v0 - v2;
                D = b * b - 4 * A * v0;
                if (D >= 0) {
                    D = Math.sqrt(D);
                    if (v0 < 0 && v2 > 0) {
                        m8 = 1;
                        e = (-b + D) / (2 * A);
                        if (e > 1 || e < 0) {
                            e = (-b - D) / (2 * A);
                        }
                        t3 = c0 + e + (1.0 / 120.0);
                        h3 = Math.floor(t3);
                        m3 = Math.floor((t3 - h3) * 60);

                        sunriseHour = (int) h3;
                        sunriseMinute = (int) m3;

                        h7 = h0 + e * (h2 - h0);
                        n7 = -Math.cos(d1) * Math.sin(h7);
                        d7 = c * Math.sin(d1) - s * Math.cos(d1) * Math.cos(h7);
                        az = Math.atan(n7 / d7) / dr;
                        if (d7 < 0)
                            az = az + 180;
                        if (az < 0)
                            az = az + 360;
                        if (az > 360)
                            az = az - 360;
                        sunriseAzimuth = az;

                    }
                    if (v0 > 0 && v2 < 0) {
                        w8 = 1;

                        e = (-b + D) / (2 * A);
                        if (e > 1 || e < 0) {
                            e = (-b - D) / (2 * A);
                        }

                        t3 = c0 + e + (1.0 / 120.0);
                        h3 = Math.floor(t3);
                        m3 = Math.floor((t3 - h3) * 60);
                        sunsetHour = (int) h3;
                        sunsetMinute = (int) m3;

                        h7 = h0 + e * (h2 - h0);
                        n7 = -Math.cos(d1) * Math.sin(h7);
                        d7 = c * Math.sin(d1) - s * Math.cos(d1) * Math.cos(h7);
                        az = Math.atan(n7 / d7) / dr;
                        if (d7 < 0)
                            az = az + 180;
                        if (az < 0)
                            az = az + 360;
                        if (az > 360)
                            az = az - 360;
                        sunsetAzimuth = az;
                    }
                }
            }

            a0 = a2;
            d0 = d2;
            v0 = v2;
        }

        if (m8 == 0 && w8 == 0) {
            if (v2 < 0) {
                sunriseAzimuth = 0;
                sunsetAzimuth = 0;
                sunsetHour = 0;
                sunsetMinute = 0;
                sunriseHour = 0;
                sunriseMinute = 0;

            }
            if (v2 > 0) {
                sunriseAzimuth = 0;
                sunsetAzimuth = 360;
                sunsetHour = 24;
                sunsetMinute = 0;
                sunriseHour = 0;
                sunriseMinute = 0;
            }
        } else {
            if (m8 == 0)
                sunriseTime = "No Sunrise this date";
            if (w8 == 0)
                sunriseTime = "No Sunset this date";
        }
    }

    /*
     * Calculates the julian date.
     */
    private double calToJul() {

        double a, b, jd;

        if (month <= 2) {
            year = year - 1;
            month = month + 12;
        }

        a = Math.floor(year / 100);
        b = 2 - a + Math.floor(a / 4);
        if (year < 1583)
            b = 0;

        jd = Math.floor(365.25 * (year + 4716))
                + Math.floor(30.6001 * (month + 1)) + day + b - 1524.5;
        return jd;
    }

    /**
     * Sets up various calculations.
     * 
     * @param t
     * @param tt
     * @return
     */
    private double[] fundArg(double t, double tt) {
        double l;
        double g;
        double v;
        double u;
        double w;
        double s;

        double returnValues[] = new double[2];

        l = .779072 + .00273790931 * t;
        g = .993126 + .0027377785 * t;
        l = l - Math.floor(l);
        g = g - Math.floor(g);
        l = l * p2;
        g = g * p2;
        v = .39785 * Math.sin(l) - .010000 * Math.sin(l - g) + .003333
                * Math.sin(l + g) - .00021 * tt * Math.sin(l);
        u = 1 - .03349 * Math.cos(g) - .00014 * Math.cos(2 * l) + .00008
                * Math.cos(l);
        w = -.00010 - .04129 * Math.sin(2 * l) + .03211 * Math.sin(g) + .00104
                * Math.sin(2 * l - g);
        w = w - .00035 * Math.sin(2 * l + g) - .00008 * tt * Math.sin(g);

        s = w / Math.sqrt(u - v * v);
        returnValues[0] = l + Math.atan(s / Math.sqrt(1 - s * s));

        s = v / Math.sqrt(u);
        returnValues[1] = Math.atan(s / Math.sqrt(1 - s * s));

        return returnValues;
    }

    /**
     * @return the sunRiseTime
     */
    public String getSunRiseTime() {
        return String.format("Rise: %02d:%02d %s", sunriseHour, sunriseMinute,
                zone);
    }

    /**
     * @return the sunSetTime
     */
    public String getSunSetTime() {
        return String.format("Set: %02d:%02d %s", sunsetHour, sunsetMinute,
                zone);
    }

    /**
     * @return the sunRiseAzimuth
     */
    public String getSunRiseAzimuth() {
        return sunRiseAzimuth;
    }

    /**
     * @param sunRiseAzimuth
     *            the sunRiseAzimuth to set
     */
    public void setSunRiseAzimuth(String sunRiseAzimuth) {
        this.sunRiseAzimuth = sunRiseAzimuth;
    }

    /**
     * @return the sunSetAzimuth
     */
    public String getSunSetAzimuth() {
        return sunSetAzimuth;
    }

    /**
     * @param sunSetAzimuth
     *            the sunSetAzimuth to set
     */
    public void setSunSetAzimuth(String sunSetAzimuth) {
        this.sunSetAzimuth = sunSetAzimuth;
    }

    /**
     * @return the longitude
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the latitude
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the timezone
     */
    public double getTimezone() {
        return timezone;
    }

    /**
     * @param timezone
     *            the timezone to set
     */
    public void setTimezone(double timezone) {
        this.timezone = timezone;
    }

    /**
     * @return the month
     */
    public int getMonth() {
        return month;
    }

    /**
     * @param month
     *            the month to set
     */
    public void setMonth(int month) {
        this.month = month;
    }

    /**
     * @return the day
     */
    public int getDay() {
        return day;
    }

    /**
     * @param day
     *            the day to set
     */
    public void setDay(int day) {
        this.day = day;
    }

    /**
     * 
     * @return
     */
    public double getSunriseAzimuth() {
        return sunriseAzimuth;
    }

    /**
     * 
     * @param sunriseAzimuth
     */
    public void setSunriseAzimuth(double sunriseAzimuth) {
        this.sunriseAzimuth = sunriseAzimuth;
    }

    /**
     * 
     * @return
     */
    public double getSunsetAzimuth() {
        return sunsetAzimuth;
    }

    /**
     * 
     * @param sunsetAzimuth
     */
    public void setSunsetAzimuth(double sunsetAzimuth) {
        this.sunsetAzimuth = sunsetAzimuth;
    }

    /**
     * 
     * @return
     */
    public int getSunriseHour() {
        return sunriseHour;
    }

    /**
     * 
     * @param sunriseHour
     */
    public void setSunriseHour(int sunriseHour) {
        this.sunriseHour = sunriseHour;
    }

    /**
     * 
     * @return
     */
    public int getSunriseMinute() {
        return sunriseMinute;
    }

    /**
     * 
     * @param sunriseMinute
     */
    public void setSunriseMinute(int sunriseMinute) {
        this.sunriseMinute = sunriseMinute;
    }

    /**
     * 
     * @return
     */
    public int getSunsetHour() {
        return sunsetHour;
    }

    /**
     * 
     * @param sunsetHour
     */
    public void setSunsetHour(int sunsetHour) {
        this.sunsetHour = sunsetHour;
    }

    /**
     * 
     * @return
     */
    public int getSunsetMinute() {
        return sunsetMinute;
    }

    /**
     * 
     * @param sunsetMinute
     */
    public void setSunsetMinute(int sunsetMinute) {
        this.sunsetMinute = sunsetMinute;
    }

    /**
     * Returns a string of "hh hrs(s) mm min(s) of daylength.
     * 
     * @return
     */
    public String getDayLength() {

        int upTime = sunriseHour * 60 + sunriseMinute;
        int downTime = sunsetHour * 60 + sunsetMinute;
        int length = 0;

        if (upTime <= downTime) {
            length = downTime - upTime;
        } else {
            length = 1440 - upTime + downTime;
        }

        int lengthHour = length / 60;
        int lengthMin = length - (lengthHour * 60);

        return String.format("Length: %d hr %02d min", lengthHour, lengthMin);
    }

}
