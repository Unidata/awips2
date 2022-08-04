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
package com.raytheon.viz.hydrocommon.pdc;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants.QueryMode;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants.TimeModeType;

/**
 * Point Data Control DB Access code.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            mpduff      Initial creation
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 * Feb 03, 2020 20555      ryu         Fix SQL time constraints for river height 
 *                                     for selected time and value change options
 * Mar 11, 2020 19533   mgamazaychikov Use constants from CommonHydroConstants
 * </pre>
 * 
 * @author mpduff
 */

public class PDCDBUtils {
    private static final double MINMAX_DUR_MULTIPLIER = 1.5;

    /**
     * Builds the where clause for Height or Discharge queries
     * 
     * @param pe
     *            The Physical Element
     * @return the where clause
     */
    public static String buildRiverWhere(String pe) {
        SimpleDateFormat dateFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));

        PDCOptionData pcOptions = PDCOptionData.getInstance();
        String durCode = null;
        int durHours = pcOptions.getDurHours();

        Date minTime = new Date(pcOptions.getValidTime().getTime());
        Date maxTime = new Date(pcOptions.getValidTime().getTime());

        Date now = SimulatedTime.getSystemTime().getTime();
        Date lowerChangeLowertime = new Date(
                pcOptions.getValidTime().getTime());
        Date lowerChangeUppertime = new Date(
                pcOptions.getValidTime().getTime());
        Date upperChangeUppertime = new Date(
                pcOptions.getValidTime().getTime());
        Date upperChangeLowertime = new Date(
                pcOptions.getValidTime().getTime());

        /*
         * begin the where clause with the pe filter. the function argument pe
         * is only used for special cases, when reading data for the "primary"
         * pe, which can be some H* pe from the height table or some Q* pe from
         * the discharge table.
         */
        StringBuilder where = new StringBuilder();
        if ((pe != null) && (pe.length() > 0)) {
            where.append("WHERE pe like '" + pe + "%%'");
        } else {
            where.append("WHERE pe = '"
                    + pcOptions.getSelectedAdHocElementString() + "'");
        }

        /* if a type source is specified, then filter on the type-source */
        if (pcOptions.getFilterByTypeSource() == 1) {
            where.append(buildTypeSourceWhereFilter());
        }

        /*
         * set the time window based on the time mode, and filter further if
         * min/max time modes in effect
         */
        if ((pcOptions.getTimeMode() == TimeModeType.MAXSELECT.getTimeMode())
                || (pcOptions.getTimeMode() == TimeModeType.MINSELECT
                        .getTimeMode())) {
            Date validTime = new Date(pcOptions.getValidTime().getTime());

            Calendar cal = new GregorianCalendar();
            cal.setTimeInMillis((long) ((validTime.getTime())
                    - durHours * PDCConstants.MILLIS_PER_HOUR
                            * PDCConstants.MINMAX_DUR_MULTIPLIER));
            minTime.setTime(cal.getTimeInMillis());
            maxTime = validTime;

            durCode = durHoursToShefCode();

            where.append(" and extremum = '" + durCode + "' ");
        } else if (pcOptions.getTimeMode() == TimeModeType.LATEST
                .getTimeMode()) {
            long millis = now.getTime()
                    - durHours * PDCConstants.MILLIS_PER_HOUR;
            minTime.setTime(millis);
            maxTime = now;
        } else if (pcOptions.getTimeMode() == TimeModeType.SETTIME
                .getTimeMode()) {
            Calendar min = new GregorianCalendar();
            min.setTime(minTime);
            min.add(Calendar.HOUR, (durHours * -1));
            minTime = min.getTime();

            Calendar max = new GregorianCalendar();
            max.setTime(maxTime);
            max.add(Calendar.HOUR, durHours);
            maxTime = max.getTime();
        } else if (pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE
                .getTimeMode()) {
            /*
             * Retrieve the number of hours that can be searched around the end
             * times of the change period.
             */
            int changeHourWindow = AppsDefaults.getInstance().getInt(
                    PDCConstants.HV_HOURS_IN_WINDOW,
                    PDCConstants.MISSING_VALUE);

            Calendar cal = new GregorianCalendar();

            cal.setTime(pcOptions.getValidTime());
            cal.add(Calendar.HOUR, changeHourWindow / 2 - durHours);
            lowerChangeUppertime = cal.getTime();
            cal.add(Calendar.HOUR,-changeHourWindow);
            lowerChangeLowertime = cal.getTime();

            cal.setTime(pcOptions.getValidTime());
            cal.add(Calendar.HOUR, changeHourWindow / 2);
            upperChangeUppertime = cal.getTime();
            cal.add(Calendar.HOUR,-changeHourWindow);
            upperChangeLowertime = cal.getTime();
        }
        
        if (pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE
                .getTimeMode()) {
            where.append("and ( ( obstime >= '"
                    + dateFormat.format(upperChangeLowertime) + "' ");
            where.append(" and obstime <= '"
                    + dateFormat.format(upperChangeUppertime) + "' )");
            where.append(" or ( obstime >= '"
                    + dateFormat.format(lowerChangeLowertime) + "' ");
            where.append(" and obstime <= '"
                    + dateFormat.format(lowerChangeUppertime) + "' ) )");
            where.append(" and value != " + PDCConstants.MISSING_VALUE);
        } else {
            where.append(
                    " and obstime >= '" + dateFormat.format(minTime) + "' ");
            where.append(
                    " and  obstime <= '" + dateFormat.format(maxTime) + "' ");
            where.append(" and value != " + PDCConstants.MISSING_VALUE);
        }
        where.append(" order by lid asc, ts, obstime desc");

        return where.toString();
    }

    /**
     * The algorithm uses the duration equal to or less than the given duration
     * in hours; i.e.. it rounds down.
     */
    private static String durHoursToShefCode() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        String durcode = null;

        int dur = pcOptions.getDurHours();

        /* if stack for max codes */
        if (pcOptions.getTimeMode() == TimeModeType.MAXSELECT.getTimeMode()) {
            if (dur >= 24 * 7) {
                durcode = "W";
            } else if (dur >= 24) {
                durcode = "X";
            } else if (dur >= 18) {
                durcode = "S";
            } else if (dur >= 12) {
                durcode = "Y";
            } else if (dur >= 6) {
                durcode = "R";
            } else if (dur >= 3) {
                durcode = "E";
            } else if (dur >= 1) {
                durcode = "D";
            } else {
                durcode = "D";
            }
        } else {
            /* if stack for min codes */
            if (dur >= 24 * 7) {
                durcode = "M";
            } else if (dur >= 24) {
                durcode = "N";
            } else if (dur >= 18) {
                durcode = "I";
            } else if (dur >= 12) {
                durcode = "P";
            } else if (dur >= 6) {
                durcode = "H";
            } else if (dur >= 3) {
                durcode = "G";
            } else if (dur >= 1) {
                durcode = "F";
            } else {
                durcode = "F";
            }
        }

        return durcode;
    }

    public static String buildRiverStatusWhere(PDCOptions pcOptions) {
        StringBuilder where = new StringBuilder();
        int durHours = 0;
        SimpleDateFormat dateFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss");
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        Date now = SimulatedTime.getSystemTime().getTime();
        Date minTime = new Date(pcOptions.getValidTime().getTime());
        Date maxTime = new Date(pcOptions.getValidTime().getTime());

        if (pcOptions.getPrimary() == 1) {
            where.append("WHERE pe like '%%%%' ");
        } else {
            where.append("WHERE pe = '");
            where.append(pcOptions.getSelectedAdHocElementString());
            where.append("' ");
        }

        /* filter by typesource if requested. */
        if (pcOptions.getFilterByTypeSource() == 1) {
            where.append(buildTypeSourceWhereFilter());
        } else {
            where.append("AND ts NOT LIKE 'C%%' ");
        }

        if (pcOptions.getQueryMode() == QueryMode.TIME_STEP_MODE
                .getQueryMode()) {
            durHours = 24;
        } else {
            durHours = pcOptions.getDurHours();
        }

        /*
         * set the time window based on the time mode. if min/max given use
         * settime mode
         */

        if (pcOptions.getTimeMode() == TimeModeType.LATEST.getTimeMode()) {
            long millis = now.getTime()
                    - durHours * PDCConstants.MILLIS_PER_HOUR;
            minTime = SimulatedTime.getSystemTime().getTime();
            minTime.setTime(millis);
            maxTime = SimulatedTime.getSystemTime().getTime();
        } else if (pcOptions.getTimeMode() == TimeModeType.SETTIME
                .getTimeMode()) {
            Calendar min = new GregorianCalendar();
            min.setTimeInMillis(minTime.getTime());
            min.add(Calendar.HOUR_OF_DAY, (durHours * -1));
            minTime.setTime(min.getTimeInMillis());

            Calendar max = new GregorianCalendar();
            max.setTimeInMillis(maxTime.getTime());
            max.add(Calendar.HOUR_OF_DAY, durHours);
            maxTime.setTime(max.getTimeInMillis());
        }

        if (pcOptions.getTimeMode() == TimeModeType.LATEST.getTimeMode()) {
            where.append(" AND validtime >= '")
                    .append(dateFormat.format(minTime)).append("'");
            where.append(" AND value != ").append(PDCConstants.MISSING_VALUE);
            where.append(" ORDER BY lid ASC, pe, ts, validtime DESC");
        } else {
            where.append(" AND validtime >= '")
                    .append(dateFormat.format(minTime)).append("'");
            where.append(" AND validtime <= '")
                    .append(dateFormat.format(maxTime)).append("'");
            where.append(" AND value != ").append(PDCConstants.MISSING_VALUE);
            where.append(" ORDER BY lid ASC, pe, ts, validtime DESC");
        }
        return where.toString();
    }

    /**
     * This function creates a portion of a where clause that filters on the
     * type sources in pcOptions.getTypeSourceChosenCount
     * 
     * @return
     */
    public static String buildTypeSourceWhereFilter() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        StringBuilder buffer = new StringBuilder();
        String returnValue = null;

        for (int i = 0; i < pcOptions.getTypeSourceChosenList().size(); i++) {
            buffer.append(" '")
                    .append(pcOptions.getTypeSourceChosenList().get(i))
                    .append("' ");

            if (i != pcOptions.getTypeSourceChosenCount() - 1) {
                buffer.append(", ");
            }
        }

        if (pcOptions.getTypeSourceChosenCount() > 0) {
            returnValue = " and ts in (" + buffer.toString() + ") ";
        } else {
            returnValue = "";
        }

        return returnValue;
    }

    /**
     * Build where clause for generic retrievals of data from Observation type
     * tables. This where clause is also used for retrievals from the
     * LatestObsValue table which has a similar structure as the Observation
     * tables.
     * 
     * @return the Where clause
     */
    public static String buildSnowTempOtherWhere() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        StringBuilder where = new StringBuilder();
        String durCode = null;
        int durHours = pcOptions.getDurHours();

        Date minTime = SimulatedTime.getSystemTime().getTime();
        Date maxTime = SimulatedTime.getSystemTime().getTime();
        Date lowerChangeBasetime = SimulatedTime.getSystemTime().getTime();
        Date lowerChangeLowertime = SimulatedTime.getSystemTime().getTime();
        Date lowerChangeUppertime = SimulatedTime.getSystemTime().getTime();
        Date upperChangeUppertime = SimulatedTime.getSystemTime().getTime();
        Date upperChangeLowertime = SimulatedTime.getSystemTime().getTime();

        /* get the current time */
        Date now = SimulatedTime.getSystemTime().getTime();

        /* filter by physical element first */
        where.append("WHERE pe = '" + pcOptions.getSelectedAdHocElementString()
                + "' ");

        /* filter by type-source */
        if (pcOptions.getFilterByTypeSource() == 1) {
            where.append(buildTypeSourceWhereFilter() + " ");
        }

        /* set the time window */
        if ((pcOptions.getTimeMode() == TimeModeType.MAXSELECT.getTimeMode())
                || (pcOptions.getTimeMode() == TimeModeType.MINSELECT
                        .getTimeMode())) {
            Date validTime = pcOptions.getValidTime();

            Calendar cal = new GregorianCalendar();
            cal.setTimeInMillis((long) ((validTime.getTime() / 1000)
                    - durHours * 3600 * PDCConstants.MINMAX_DUR_MULTIPLIER));
            minTime.setTime(cal.getTimeInMillis());
            maxTime = validTime;

            durCode = durHoursToShefCode();

            where.append(" and extremum = '").append(durCode).append("' ");
        } else if (pcOptions.getTimeMode() == TimeModeType.LATEST
                .getTimeMode()) {
            long millis = now.getTime()
                    - durHours * PDCConstants.MILLIS_PER_MINUTE * 60;
            minTime.setTime(millis);
            maxTime = now;

            where.append("AND extremum = 'Z' ");
        } else if (pcOptions.getTimeMode() == TimeModeType.SETTIME
                .getTimeMode()) {
            Calendar min = new GregorianCalendar();
            min.setTimeInMillis(minTime.getTime());
            min.add(Calendar.HOUR, durHours * -1);
            minTime.setTime(min.getTimeInMillis());

            Calendar max = new GregorianCalendar();
            max.setTimeInMillis(maxTime.getTime());
            max.add(Calendar.HOUR, durHours);

            where.append("AND extremum = 'Z' ");
        } else if (pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE
                .getTimeMode()) {
            /*
             * Retrieve the number of hours that can be searched around the end
             * times of the change period.
             */
            int changeHourWindow = AppsDefaults.getInstance().getInt(
                    PDCConstants.HV_HOURS_IN_WINDOW,
                    PDCConstants.MISSING_VALUE);
            ;

            Calendar upperChangeBaseCal = new GregorianCalendar();
            Calendar lowerChangeBaseCal = new GregorianCalendar();

            upperChangeBaseCal
                    .setTimeInMillis(pcOptions.getValidTime().getTime());
            lowerChangeBaseCal.add(Calendar.HOUR, durHours * -1);
            lowerChangeBasetime.setTime(lowerChangeBaseCal.getTimeInMillis());

            upperChangeBaseCal.add(Calendar.HOUR, changeHourWindow);
            upperChangeUppertime = upperChangeBaseCal.getTime();
            upperChangeBaseCal.add(Calendar.HOUR, changeHourWindow * -1);
            upperChangeLowertime = upperChangeBaseCal.getTime();

            lowerChangeBaseCal.add(Calendar.HOUR, changeHourWindow);
            lowerChangeUppertime = lowerChangeBaseCal.getTime();
            lowerChangeBaseCal.add(Calendar.HOUR, changeHourWindow * -1);
            lowerChangeLowertime = lowerChangeBaseCal.getTime();
        }

        SimpleDateFormat sdf = new SimpleDateFormat(
                CommonHydroConstants.IHFS_DATE_FORMAT);
        if (pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE
                .getTimeMode()) {
            where.append("and ( ( obstime >= '")
                    .append(sdf.format(upperChangeLowertime)).append("' ");
            where.append(" and obstime <= '")
                    .append(sdf.format(upperChangeUppertime)).append("' )");
            where.append(" or ( obstime >= '")
                    .append(sdf.format(lowerChangeLowertime)).append("' ");
            where.append(" and obstime <= '")
                    .append(sdf.format(lowerChangeUppertime)).append("' ) )");
            where.append(" and value != " + PDCConstants.MISSING_VALUE);
            where.append(" and extremum = 'Z' ");
        } else {
            where.append(" and obstime >= '").append(sdf.format(minTime))
                    .append("' ");
            where.append(" and  obstime <= '").append(sdf.format(maxTime))
                    .append("' ");
            where.append(" and value != " + PDCConstants.MISSING_VALUE);
        }
        where.append(" ORDER BY lid ASC, ts, obstime DESC");

        return where.toString();
    }

    public static String buildSnowTempOtherWhere2() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        StringBuilder where = new StringBuilder();
        Calendar minTime = new GregorianCalendar();
        minTime.setTimeInMillis(0);
        Calendar maxTime = new GregorianCalendar();
        Calendar lowerChangeBaseTime = new GregorianCalendar();
        Calendar lowerChangeLowerTime = new GregorianCalendar();
        Calendar lowerChangeUpperTime = new GregorianCalendar();
        Calendar upperChangeBaseTime = new GregorianCalendar();
        Calendar upperChangeUpperTime = new GregorianCalendar();
        Calendar upperChangeLowerTime = new GregorianCalendar();
        String durCode = null;
        long lvalue;
        long durHours = pcOptions.getDurHours() * PDCConstants.MILLIS_PER_HOUR;
        int changeHourWindow;
        double changeSecondsWindow;

        /* get the current time */
        Calendar currentTime = new GregorianCalendar();
        currentTime.setTimeZone(TimeZone.getTimeZone("GMT"));
        long currentMillis = currentTime.getTimeInMillis();

        /* filter by physical element first */
        where.append("WHERE pe = '")
                .append(pcOptions.getSelectedAdHocElementString()).append("' ");

        /* filter by type-source */
        if (pcOptions.getFilterByTypeSource() == 1) {
            where.append(buildTypeSourceWhereFilter()).append(" ");
        }

        /* set the time window */
        if ((pcOptions.getTimeMode() == TimeModeType.MAXSELECT.getTimeMode())
                || (pcOptions.getTimeMode() == TimeModeType.MINSELECT
                        .getTimeMode())) {
            lvalue = (long) (currentMillis - durHours * MINMAX_DUR_MULTIPLIER);
            minTime.setTimeInMillis(lvalue);

            maxTime.setTime(pcOptions.getValidTime());

            durCode = durHoursToShefCode();

            where.append("AND extremum = '").append(durCode).append("' ");
        } else if (pcOptions.getTimeMode() == TimeModeType.LATEST
                .getTimeMode()) {
            lvalue = currentMillis - durHours;
            minTime.setTimeInMillis(lvalue);
            maxTime = currentTime;

            where.append("AND extremum = 'Z' ");
        } else if (pcOptions.getTimeMode() == TimeModeType.SETTIME
                .getTimeMode()) {
            lvalue = pcOptions.getValidTime().getTime() - durHours;
            minTime.setTimeInMillis(lvalue);
            lvalue = pcOptions.getValidTime().getTime() + durHours;
            maxTime.setTimeInMillis(lvalue);

            where.append("AND extremum = 'Z' ");
        } else if (pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE
                .getTimeMode()) {
            /*
             * Retrieve the number of hours that can be searched around the end
             * times of the change period.
             */
            changeHourWindow = AppsDefaults.getInstance().getInt(
                    PDCConstants.HV_HOURS_IN_WINDOW,
                    PDCConstants.MISSING_VALUE);
            changeSecondsWindow = (changeHourWindow / 2)
                    * PDCConstants.MILLIS_PER_HOUR;

            upperChangeBaseTime.setTime(pcOptions.getValidTime());

            lvalue = pcOptions.getValidTime().getTime() - durHours;
            lowerChangeBaseTime.setTimeInMillis(lvalue);

            lvalue = (long) (upperChangeBaseTime.getTimeInMillis()
                    + changeSecondsWindow);
            upperChangeUpperTime.setTimeInMillis(lvalue);

            lvalue = (long) (upperChangeBaseTime.getTimeInMillis()
                    - changeSecondsWindow);
            upperChangeLowerTime.setTimeInMillis(lvalue);

            lvalue = (long) (lowerChangeBaseTime.getTimeInMillis()
                    + changeSecondsWindow);
            lowerChangeUpperTime.setTimeInMillis(lvalue);

            lvalue = (long) (lowerChangeBaseTime.getTimeInMillis()
                    - changeSecondsWindow);
            lowerChangeLowerTime.setTimeInMillis(lvalue);
        }

        SimpleDateFormat sdf = new SimpleDateFormat(
                CommonHydroConstants.IHFS_DATE_FORMAT);

        if (pcOptions.getTimeMode() == TimeModeType.VALUE_CHANGE
                .getTimeMode()) {
            Date upperChangeLowerTimeDate = upperChangeLowerTime.getTime();
            Date upperChangeUpperTimeDate = upperChangeUpperTime.getTime();
            Date lowerChangeLowerTimeDate = lowerChangeLowerTime.getTime();
            Date lowerChangeUpperTimeDate = lowerChangeUpperTime.getTime();

            where.append(" AND ( ( obstime >= '")
                    .append(sdf.format(upperChangeLowerTimeDate)).append("' ");
            where.append(" AND obstime <= '")
                    .append(sdf.format(upperChangeUpperTimeDate))
                    .append("' ) ");
            where.append(" OR ( obstime >= '")
                    .append(sdf.format(lowerChangeLowerTimeDate)).append("' ");
            where.append(" AND obstime <= '")
                    .append(sdf.format(lowerChangeUpperTimeDate))
                    .append("' ) ) ");
            where.append(" AND value != ").append(PDCConstants.MISSING_VALUE)
                    .append(" ");
            where.append(" AND extremum = 'Z' ");
        } else {
            Date minTimeDate = minTime.getTime();
            Date maxTimeDate = maxTime.getTime();

            where.append(" AND obstime >= '").append(sdf.format(minTimeDate))
                    .append("' ");
            where.append(" AND obstime <= '").append(sdf.format(maxTimeDate))
                    .append("' ");
            where.append("AND value != ").append(PDCConstants.MISSING_VALUE)
                    .append(" ");
        }
        where.append(" ORDER BY lid ASC, ts, obstime DESC");

        return where.toString();
    }
}