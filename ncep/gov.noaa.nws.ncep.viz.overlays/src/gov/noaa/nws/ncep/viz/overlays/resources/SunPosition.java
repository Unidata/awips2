package gov.noaa.nws.ncep.viz.overlays.resources;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import com.vividsolutions.jts.geom.Coordinate;

public class SunPosition {

    /** 2*Math.PI */
    final public static transient float TWO_PI = (float) Math.PI * 2.0f;

    /** Epoch Julian Date. */
    public final static double EPOCH_JULIAN_DATE = 2447891.5;

    /** Epoch start time in seconds. */
    public final static double EPOCH_TIME_SECS = 631065600;

    /**
     * Constant denoting the number of radians an object would travel if it
     * orbited around the earth in a day.
     */
    public static double ORBIT_RADS_PER_DAY = TWO_PI / 365.242191;

    /**
     * Ecliptic Longitude of earth at 1990 January epoch. From Duffett-Smith,
     * chapter 46, table 6. (279.403303 degrees converted to radians).
     */
    public static final double ECLIPTIC_LONGITUDE_EPOCH = 4.87650757893409;

    /**
     * Variable notation of ECLIPTIC_LONGITUDE_EPOCH from Duffett-Smith.
     */
    public static final double epsilon_g = ECLIPTIC_LONGITUDE_EPOCH;

    /**
     * Ecliptic Longitude of of perigee. From Duffett-Smith, chapter 46, table
     * 6. (282.768422 degrees converted to radians).
     */
    public static final double ECLIPTIC_LONGITUDE_PERIGEE = 4.935239985213178;

    /**
     * Variable notation of ECLIPTIC_LONGITUDE_PERIGEE from Duffett-Smith.
     */
    public static final double omega_bar_g = ECLIPTIC_LONGITUDE_PERIGEE;

    /**
     * Eccentricity of orbit, from Duffett-Smith, chapter 46, table 6.
     */
    public static final double ECCENTRICITY = 0.016713;

    /**
     * MEAN_OBLIQUITY_OF_EPOCH gives the mean obliquity of the ecliptic, which
     * is the angle between the planes of the equator and the ecliptic. Using
     * the algorithm described in Duffett-Smith, chapter 27, this is calculated
     * for the 1990 January epoch to be .4091155 radians (23.440592 degrees).
     */
    public static final double MEAN_OBLIQUITY_OF_EPOCH = .4091155;

    // These parameters are used in the Moon position calculations.

    /**
     * Moon parameter, from Duffett-Smith, chapter 65, table 10. In radians.
     */
    public static final double MOON_EPOCH_MEAN_LONGITUDE = 318.351648 * Math.PI / 180.0;

    /**
     * The algorithm representation for the moon MOON_EPOCH_MEAN_LONGITUDE, "l".
     */
    public static final double el0 = MOON_EPOCH_MEAN_LONGITUDE;

    /**
     * Moon parameter, from Duffett-Smith, chapter 65, table 10. In radians.
     */
    public static final double PERIGEE_EPOCH_MEAN_LONGITUDE = 36.340410 * Math.PI / 180.0;

    /**
     * The algorithm representation for the moon PERIGEE_EPOCH_MEAN_LONGITUDE.
     */
    public static final double P0 = PERIGEE_EPOCH_MEAN_LONGITUDE;

    /**
     * Moon parameter, from Duffett-Smith, chapter 65, table 10. In radians.
     */
    public static final double NODE_EPOCH_MEAN_LONGITUDE = 318.510107 * Math.PI / 180.0;

    /**
     * The algorithm representation for the moon NODE_EPOCH_MEAN_LONGITUDE.
     */
    public static final double N0 = NODE_EPOCH_MEAN_LONGITUDE;

    /**
     * Moon parameter, from Duffett-Smith, chapter 65, table 10. In radians.
     */
    public static final double MOON_ORBIT_INCLINATION = 5.145396 * Math.PI / 180.0;

    /**
     * The algorithm representation for the moon MOON_ORBIT_INCLINATION, "i".
     */
    public static final double eye = MOON_ORBIT_INCLINATION;

    /** Moon parameter, from Duffett-Smith, chapter 65, table 10. */
    public static final double MOON_ECCENTRICITY = .054900;

    /** Moon parameter, from Duffett-Smith, chapter 65, table 10. */
    public static final double MAJOR_AXIS_MOON_ORBIT = 384401; // km

    /**
     * Moon parameter, from Duffett-Smith, chapter 65, table 10. In radians.
     */
    public static final double MOON_ANGULAR_SIZE = .5181 * Math.PI / 180.0;

    /**
     * Moon parameter, from Duffett-Smith, chapter 65, table 10. In radians.
     */
    public static final double MOON_PARALLAX = .9507 * Math.PI / 180.0;

    /**
     * Use Kepllers's equation to find the eccentric anomaly. From
     * Duffett-Smith, chapter 47.
     * 
     * @param M
     *            the angle that the Sun has moved since it passed through
     *            perigee.
     */
    public static double eccentricAnomaly(double M) {
        double delta;
        double E = M;
        while (true) {
            delta = E - (ECCENTRICITY * Math.sin(E)) - M;

            if (Math.abs(delta) <= 1E-10)
                break;
            E -= (delta / (1.0 - (ECCENTRICITY * Math.cos(E))));
        }
        return E;
    }

    /**
     * Calculate the mean anomaly of sun, in radians. From Duffett-Smith,
     * chapter 47.
     * 
     * @param daysSinceEpoch
     *            number of days since 1990 January epoch.
     */
    protected static double sunMeanAnomaly(double daysSinceEpoch) {

        double N = ORBIT_RADS_PER_DAY * daysSinceEpoch;
        N %= TWO_PI;
        if (N < 0)
            N += TWO_PI;

        double M0 = N + epsilon_g - omega_bar_g;
        if (M0 < 0)
            M0 += TWO_PI;
        return M0;
    }

    /**
     * Calculate the ecliptic longitude of sun, in radians. From Duffett-Smith,
     * chapter 47.
     * 
     * @param M0
     *            sun's mean anomaly, calculated for the requested time relative
     *            to the 1990 epoch.
     */
    protected static double sunEclipticLongitude(double M0) {
        double E = eccentricAnomaly(M0);
        double v = 2 * Math.atan(Math.sqrt((1 + ECCENTRICITY)
                / (1 - ECCENTRICITY))
                * Math.tan(E / 2.0));
        double ret = v + omega_bar_g;
        ret = adjustWithin2PI(ret);
        return ret;
    }

    /**
     * Conversion from ecliptic to equatorial coordinates for ascension. From
     * Duffett-Smith, chapter 27.
     * 
     * @param lambda
     *            ecliptic longitude
     * @param beta
     *            ecliptic latitude
     */
    protected static double eclipticToEquatorialAscension(double lambda,
            double beta) {
        double sin_e = Math.sin(MEAN_OBLIQUITY_OF_EPOCH);
        double cos_e = Math.cos(MEAN_OBLIQUITY_OF_EPOCH);

        return Math.atan2(Math.sin(lambda) * cos_e - Math.tan(beta) * sin_e,
                Math.cos(lambda));
    }

    /**
     * Conversion from ecliptic to equatorial coordinates for declination. From
     * Duffett-Smith, chapter 27.
     * 
     * @param lambda
     *            ecliptic longitude
     * @param beta
     *            ecliptic latitude
     */
    protected static double eclipticToEquatorialDeclination(double lambda,
            double beta) {
        double sin_e = Math.sin(MEAN_OBLIQUITY_OF_EPOCH);
        double cos_e = Math.cos(MEAN_OBLIQUITY_OF_EPOCH);

        return Math.asin(Math.sin(beta) * cos_e + Math.cos(beta) * sin_e
                * Math.sin(lambda));
    }

    /**
     * Given a date from a gregorian calendar, give back a julian date. From
     * Duffett-Smith, chapter 4.
     * 
     * @param cal
     *            Gregorian calendar for requested date.
     * @return julian date of request.
     */
    public static double calculateJulianDate(GregorianCalendar cal) {
        int year = cal.get(Calendar.YEAR);
        int month = cal.get(Calendar.MONTH);
        int day = cal.get(Calendar.DAY_OF_MONTH);

        // Algorithm expects that the January is = 1, which is
        // different from the Java representation
        month++;

        if ((month == 1) || (month == 2)) {
            year -= 1;
            month += 12;
        }

        int A = year / 100;
        int B = (int) (2 - A + (A / 4));
        int C = (int) (365.25 * (float) year);
        int D = (int) (30.6001 * (float) (month + 1));

        double julianDate = (double) (B + C + D + day) + 1720994.5;

        return julianDate;
    }

    /**
     * Calculate the greenwich sidereal time (GST). From Duffett-Smith, chapter
     * 12.
     * 
     * @param julianDate
     *            julian date of request
     * @param time
     *            calendar reflecting local time zone change to greenwich
     * @return GST relative to unix epoch.
     */
    public static double greenwichSiderealTime(double julianDate,
            GregorianCalendar time) {

        double T = (julianDate - 2451545.0) / 36525.0;
        double T0 = 6.697374558 + (T * (2400.051336 + (T + 2.5862E-5)));

        T0 %= 24.0;
        if (T0 < 0) {
            T0 += 24.0;
        }

        double UT = time.get(Calendar.HOUR_OF_DAY)
                + (time.get(Calendar.MINUTE) + time.get(Calendar.SECOND) / 60.0)
                / 60.0;

        T0 += UT * 1.002737909;

        T0 %= 24.0;
        if (T0 < 0) {
            T0 += 24.0;
        }

        return T0;
    }

    /**
     * Given the number of milliseconds since the unix epoch, compute position
     * on the earth (lat, lon) such that sun is directly overhead. From
     * Duffett-Smith, chapter 46-47.
     * 
     * @param mssue
     *            milliseconds since unix epoch
     * @return LatLonPoint of the point on the earth that is closest.
     */
    public static Coordinate sunPosition(long mssue) {

        // Set the date and clock, based on the millisecond count:
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(new Date(mssue));

        double julianDate = calculateJulianDate(cal);

        // Need to correct time to GMT
        long gmtOffset = cal.get(Calendar.ZONE_OFFSET);
        // thanks to Erhard...
        long dstOffset = cal.get(Calendar.DST_OFFSET); // ins.
                                                       // 12.04.99
        cal.setTime(new Date(mssue - (gmtOffset + dstOffset))); // rep.
                                                                // 12.04.99

        double numDaysSinceEpoch = ((mssue / 1000) - EPOCH_TIME_SECS)
                / (24.0f * 3600.0f);

        // M0 - mean anomaly of the sun
        double M0 = sunMeanAnomaly(numDaysSinceEpoch);
        // lambda
        double sunLongitude = sunEclipticLongitude(M0);
        // alpha
        double sunAscension = eclipticToEquatorialAscension(sunLongitude, 0.0);
        // delta
        double sunDeclination = eclipticToEquatorialDeclination(sunLongitude,
                0.0);

        double tmpAscension = sunAscension - (TWO_PI / 24)
                * greenwichSiderealTime(julianDate, cal);

        return new Coordinate(Math.toDegrees(tmpAscension),
                Math.toDegrees(sunDeclination));
    }

    /**
     * Given the number of milliseconds since the unix epoch, compute position
     * on the earth (lat, lon) such that moon is directly overhead. From
     * Duffett-Smith, chapter 65. Note: This is acting like it works, but I
     * don't have anything to test it against. No promises.
     * 
     * @param mssue
     *            milliseconds since unix epoch
     * @return LatLonPoint of the point on the earth that is closest.
     */
    public static Coordinate moonPosition(long mssue) {

        // Set the date and clock, based on the millisecond count:
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTime(new Date(mssue));

        double julianDate = calculateJulianDate(cal);

        // Need to correct time to GMT
        long gmtOffset = cal.get(Calendar.ZONE_OFFSET);
        cal.setTime(new Date(mssue - gmtOffset));

        // Step 1,2
        double numDaysSinceEpoch = ((mssue / 1000) - EPOCH_TIME_SECS)
                / (24.0f * 3600.0f);
        // Step 3
        // M0 - mean anomaly of the sun
        double M0 = sunMeanAnomaly(numDaysSinceEpoch);
        // lambda
        double sunLongitude = sunEclipticLongitude(M0);
        // Step 4
        double el = (13.1763966 * numDaysSinceEpoch * Math.PI / 180) + el0;
        el = adjustWithin2PI(el);
        // Step 5
        double Mm = el - (.1114041 * numDaysSinceEpoch * Math.PI / 180) - P0;
        Mm = adjustWithin2PI(Mm);
        // Step 6
        double N = N0 - (.0529539 * numDaysSinceEpoch * Math.PI / 180);
        N = adjustWithin2PI(N);
        // Step 7
        double C = el - sunLongitude;
        double Ev = 1.2739 * Math.sin(2 * C - Mm);
        // Step 8
        double Ae = .1858 * Math.sin(M0);
        double A3 = .37 * Math.sin(M0);
        // Step 9
        double Mmp = Mm + Ev - Ae - A3;
        // Step 10
        double Ec = 6.2886 * Math.sin(Mmp);
        // Step 11
        double A4 = 0.214 * Math.sin(2 * Mmp);
        // Step 12
        double elp = el + Ev + Ec - Ae + A4;
        // Step 13
        double V = .6583 * Math.sin(2 * (elp - sunLongitude));
        // Step 14
        double elpp = elp + V;
        // Step 15
        double Np = N - (.16 * Math.sin(M0));
        // Step 16
        double y = Math.sin(elpp - Np) * Math.cos(eye);
        // Step 17
        double x = Math.cos(elpp - Np);
        // Step 18
        double amb = Math.atan2(y, x);
        // Step 19
        double lambda_m = amb + Np;
        // Step 20
        double beta_m = Math.asin(Math.sin(elpp - Np) * Math.sin(eye));
        // Step 21
        // alpha
        double moonAscension = eclipticToEquatorialAscension(lambda_m, beta_m);
        // delta
        double moonDeclination = eclipticToEquatorialDeclination(lambda_m,
                beta_m);

        double tmpAscension = moonAscension - (TWO_PI / 24)
                * greenwichSiderealTime(julianDate, cal);

        return new Coordinate(Math.toDegrees(tmpAscension),
                Math.toDegrees(moonDeclination));
    }

    /**
     * Little function that resets the input to be within 0 - 2*PI, by adding or
     * subtracting 2PI as needed.
     * 
     * @param num
     *            The number to be modified, if needed.
     */
    protected static double adjustWithin2PI(double num) {
        if (num < 0) {
            do
                num += TWO_PI;
            while (num < 0);
        } else if (num > TWO_PI) {
            do
                num -= TWO_PI;
            while (num > TWO_PI);
        }
        return num;
    }

    public static void main(String[] arg) {
        System.out.println("Sun is over "
                + SunPosition.sunPosition(System.currentTimeMillis()));
        System.out.println("Moon is over "
                + SunPosition.moonPosition(System.currentTimeMillis()));
    }
}