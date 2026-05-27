package com.raytheon.uf.edex.plugin.mpe;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import com.raytheon.uf.common.time.util.TimeUtil;
import org.locationtech.jts.geom.Coordinate;

/**
 * Common utilities for MPE
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2016 4623       skorolev    Initial creation
 * Aug 31, 2016 5631       bkowal      Made a protected constructor so that this
 *                                     class cannot be instantiated.
 * 
 * 
 * </pre>
 * 
 * @author skorolev
 */
public class CommonMPEUtils {

    private static final String DATA_LIMIT_DATE_FORMAT = "MM-dd";

    /*
     * Begin constants for lat/lon to hrap conversion.
     */

    private static final double RADIANS_PER_DEGREE = 0.017453293;

    private static final double MESH_LEN = 4.7625;

    private static final double STDLAT = 60;

    private static final double STDLON = 105;

    private static final double EARTH_RADIUS = 6371.2;

    /*
     * End constants for lat/lon to hrap conversion.
     */

    private static final ThreadLocal<SimpleDateFormat> dataLimitDF = TimeUtil
            .buildThreadLocalSimpleDateFormat(DATA_LIMIT_DATE_FORMAT,
                    TimeUtil.GMT_TIME_ZONE);

    protected CommonMPEUtils() {
    }

    /**
     * Determines if the specified observation date/time is within the data
     * limits formatted (MM-dd) date range.
     * 
     * @param obstime
     *            the specified observation date/time
     * @param monthdaystart
     *            the data limits formatted start of the date range
     * @param monthdayend
     *            the data limits formatted end of the date range
     * @return {@code true} when the specified observation date/time is within
     *         the range; {code false}, otherwise.
     * @throws Exception
     */
    public static boolean withinDataLimitTimeRange(final Date obstime,
            final String monthdaystart, final String monthdayend)
                    throws Exception {
        Date parsedLimitStart = null;
        Date parsedLimitEnd = null;
        try {
            parsedLimitStart = dataLimitDF.get().parse(monthdaystart);
        } catch (ParseException e) {
            throw new MpeException(
                    "Failed to parse start date: " + monthdaystart + ".");
        }

        try {
            parsedLimitEnd = dataLimitDF.get().parse(monthdayend);
        } catch (ParseException e) {
            throw new MpeException(
                    "Failed to parse end date: " + monthdayend + ".");
        }

        /*
         * Most of {@link Date} is deprecated; so, it is easier to work with
         * {@link Calendar}.
         */
        Calendar limitStart = TimeUtil.newGmtCalendar(parsedLimitStart);
        Calendar limitEnd = TimeUtil.newGmtCalendar(parsedLimitEnd);
        Calendar obsCompareTime = TimeUtil.newGmtCalendar(obstime);

        /*
         * Only the month/day are important for this comparison. So, set all
         * other fields to the minimum value.
         */
        TimeUtil.minCalendarFields(limitStart, Calendar.YEAR,
                Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);
        TimeUtil.minCalendarFields(limitEnd, Calendar.YEAR,
                Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);
        TimeUtil.minCalendarFields(obsCompareTime, Calendar.YEAR,
                Calendar.HOUR_OF_DAY, Calendar.MINUTE, Calendar.SECOND,
                Calendar.MILLISECOND);

        /*
         * The specified obs time must be within (not before, not after) the
         * limit start date and end date.
         */
        if (obsCompareTime.before(limitStart)
                || obsCompareTime.after(limitEnd)) {
            return false;
        }

        return true;
    }

    /**
     * Converts the specified latitude and longitude to a hrap bin. Based on:
     * /rary.ohd.whfs/src/GeoUtil/TEXT/convert_hrap.c. Note: this method
     * produces a different result than
     * com.raytheon.uf.common.xmrg.hrap.HrapUtil.latLonToHrap(Coordinate).
     * 
     * @param lon
     *            the specified longitude
     * @param lat
     *            the specified latitude
     * @return the calculated hrap {@link Coordinate}
     */
    public static Coordinate convertLatLonToHrapByReference(final double lon,
            final double lat) {
        final double tlat = STDLAT * RADIANS_PER_DEGREE;
        final double re = (EARTH_RADIUS * (1. + Math.sin(tlat))) / MESH_LEN;
        final double latrad = lat * RADIANS_PER_DEGREE;
        final double lonrad = (lon + 180. - STDLON) * RADIANS_PER_DEGREE;

        double r = re * Math.cos(latrad) / (1. + Math.sin(latrad));
        double x = r * Math.sin(lonrad);
        double y = r * Math.cos(lonrad);

        double col = x + 401.;
        double row = y + 1601.;

        return new Coordinate(col, row);
    }
}