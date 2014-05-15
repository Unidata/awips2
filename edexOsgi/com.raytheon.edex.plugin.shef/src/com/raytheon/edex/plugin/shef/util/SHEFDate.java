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
package com.raytheon.edex.plugin.shef.util;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.TimeZone;
import java.util.regex.Matcher;

import com.raytheon.uf.common.dataplugin.shef.util.SHEFErrorCodes;
import com.raytheon.uf.common.dataplugin.shef.util.SHEFTimezone;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Object to hold the Shef Date information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2009            jkorman     Initial creation
 * May 13, 2014    3088    mpduff      Cleanup, remove unused code, optimized
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class SHEFDate {

    public enum TIME_DIVISIONS {
        SECONDS, MINUTES, HOURS, DAYS, MONTHS, ENDOFMONTH, YEARS;
    }

    private static final String DATE_INC_CODES = "SNHDMEY";

    private static final String DATE_REL_CODES = "SNHDMEY";

    private static HashMap<String, TIME_DIVISIONS> DIVISIONS = new HashMap<String, TIME_DIVISIONS>();
    static {
        DIVISIONS.put("S", TIME_DIVISIONS.SECONDS);
        DIVISIONS.put("N", TIME_DIVISIONS.MINUTES);
        DIVISIONS.put("H", TIME_DIVISIONS.HOURS);
        DIVISIONS.put("D", TIME_DIVISIONS.DAYS);
        DIVISIONS.put("M", TIME_DIVISIONS.MONTHS);
        DIVISIONS.put("E", TIME_DIVISIONS.ENDOFMONTH);
        DIVISIONS.put("Y", TIME_DIVISIONS.YEARS);
    }

    /**
     * Number of days in each month.
     */
    public static final int[] DAYS_MONTH = { 0, 31, 29, 31, 30, 31, 30, 31, 31,
            30, 31, 30, 31, };

    // C Y M D H N S
    private static final String DT_FMT = "%02d%02d%02d%02d%02d%02d%02d";

    private static final String OUT_FMT = "%02d%02d%02d%02d%02d%02d%02d";

    private static final int[] NOD = { -1, 0, 31, 59, 90, 120, 151, 181, 212,
            243, 273, 304, 334, };

    private static final int[] MXD = { -1, 31, 28, 31, 30, 31, 30, 31, 31, 30,
            31, 30, 31, };

    private int error = 0;

    private int calSec;

    private int calMin;

    private int calHour;

    private int calDay;

    private int calMonth;

    private int calYear;

    private int calJul;

    private int calCC;

    private int calLY;

    private TimeZone timeZone = null;

    /**
     * 
     */
    public SHEFDate() {
        this(TimeUtil.newGmtCalendar());
    }

    /**
     * 
     * @param century
     * @param year
     * @param month
     * @param day
     * @param hour
     * @param minute
     * @param second
     */
    public SHEFDate(int century, int ly, int month, int day, int hour,
            int minute, int second) {
        calCC = century;
        calLY = ly;
        calMonth = month;
        calDay = day;
        calHour = hour;
        calMin = minute;
        calSec = second;
        forceYear();
    }

    /**
     * 
     * @param date
     * @param tz
     */
    public SHEFDate(Date date, TimeZone tz) {
        timeZone = tz;
        Calendar c = TimeUtil.newCalendar(tz);
        c.setTime(date);

        calYear = c.get(Calendar.YEAR);
        calCC = calYear / 100;
        calLY = calYear % 100;
        calMonth = c.get(Calendar.MONTH) + 1;
        calDay = c.get(Calendar.DAY_OF_MONTH);
        calHour = c.get(Calendar.HOUR_OF_DAY);
        calMin = c.get(Calendar.MINUTE);
        calSec = c.get(Calendar.SECOND);
        calJul = c.get(Calendar.DAY_OF_YEAR);
    }

    /**
     * @param c
     */
    public SHEFDate(Calendar c) {
        calYear = c.get(Calendar.YEAR);
        calCC = calYear / 100;
        calLY = calYear % 100;
        calMonth = c.get(Calendar.MONTH) + 1;
        calDay = c.get(Calendar.DAY_OF_MONTH);
        calHour = c.get(Calendar.HOUR_OF_DAY);
        calMin = c.get(Calendar.MINUTE);
        calSec = c.get(Calendar.SECOND);
        calJul = c.get(Calendar.DAY_OF_YEAR);
    }

    /**
     * Copy constructor for SHEFDate instances.
     * 
     * @param date
     */
    public SHEFDate(SHEFDate date) {
        copyFrom(date);
    }

    /**
     * 
     */
    private void doJulian1() {

        int julDay = NOD[calMonth] + calDay;
        if (calMonth > 2) {
            if (calYear == ((calYear / 4) * 4)) {
                julDay++;
                julDay += getInc(calYear);
            }
        }
        calJul = julDay;
    }

    /**
     * 
     * @param inc
     */
    private void doJulian2(int inc) {
        int tJul = calJul + inc;
        int tYear = calYear;

        int maxDay = getMaxDay(tYear);
        while ((tJul <= 0) || (tJul > maxDay)) {
            if (tJul > maxDay) {
                tJul -= maxDay;
                tYear++;
                if (tYear == 100) {
                    tYear = 0;
                }
            } else {
                tJul += maxDay;
                tYear--;
                if (tYear == -1) {
                    tYear = 99;
                }
            }
            maxDay = getMaxDay(tYear);
        }
        calYear = tYear;
        calJul = tJul;
    }

    /**
     * 
     * @param year
     * @return
     */
    private static int getMaxDay(int year) {
        int maxDay = (366 + (year / 4) - (year + 3) / 4);
        return (maxDay + getInc(year));
    }

    /**
     * 
     * @param year
     * @return
     */
    private static int getInc(int year) {
        int inc = 0;
        if ((year == 1900) || (year == 1800) || (year == 2100)) {
            inc = -1;
        }
        return inc;
    }

    /**
     * 
     * @param year
     * @return
     */
    private static boolean isLeapYear(int year) {
        boolean leapYear = false;
        if ((year % 4) == 0) {
            if ((year != 1900) && (year != 1800) && (year != 2100)) {
                leapYear = true;
            }
        }
        return leapYear;
    }

    /**
     * 
     * @param year
     * @return
     */
    public static boolean isLegalYear(int year) {
        return ((year >= 1753) && (year <= 2199));
    }

    /**
     * 
     * @return
     */
    public int getError() {
        return error;
    }

    /**
     * 
     */
    private void forceYear() {
        calYear = (calCC * 100) + calLY;
    }

    /**
     * @return the year
     */
    public int getYear() {
        return calYear;
    }

    /**
     * @param year
     *            the year to set
     */
    public void setYear(int year) {
        calYear = year;
        if (calYear > 0) {
            calCC = calYear / 100;
            calLY = calYear % 100;
        }
    }

    /**
     * @return the century
     */
    public int getCentury() {
        return calCC;
    }

    /**
     * @param century
     *            the century to set
     */
    public void setCentury(int century) {
        calCC = century;
        forceYear();
    }

    /**
     * Get the 2 digit year.
     * 
     * @return the ly
     */
    public int getLy() {
        return calLY;
    }

    /**
     * Set the 2 digit year.
     * 
     * @param ly
     *            the ly to set
     */
    public void setLy(int ly) {
        calLY = ly;
        forceYear();
    }

    /**
     * @return the month
     */
    public int getMonth() {
        return calMonth;
    }

    /**
     * @param month
     *            the month to set
     */
    public void setMonth(int month) {
        calMonth = month;
    }

    /**
     * @return the day
     */
    public int getDay() {
        return calDay;
    }

    /**
     * @param day
     *            the day to set
     */
    public void setDay(int day) {
        calDay = day;
    }

    // *******************************************

    /**
     * @return the hour
     */
    public int getHour() {
        return calHour;
    }

    /**
     * 
     * @param hour
     */
    public void setHour(int hour) {
        calHour = hour;
    }

    /**
     * @return the minute
     */
    public int getMinute() {
        return calMin;
    }

    /**
     * @param minute
     */
    public void setMinute(int minute) {
        calMin = minute;
    }

    /**
     * @return the second
     */
    public int getSecond() {
        return calSec;
    }

    /**
     * 
     * @param second
     *            the second to set
     */
    public void setSecond(int second) {
        calSec = second;
    }

    /**
     * @return the julian
     */
    public int getJulian() {
        return calJul;
    }

    /**
     * @param julian
     *            the julian to set
     */
    public void setJulian(int julian) {
        calJul = julian;
    }

    /**
     * @return the timeZone
     */
    public TimeZone getTimeZone() {
        return timeZone;
    }

    /**
     * @param timeZone
     *            the timeZone to set
     */
    public void setTimeZone(TimeZone timeZone) {
        this.timeZone = timeZone;
    }

    /**
     * 
     */
    public void adjustToTimezone() {
        if (timeZone != null) {
            doYearAdjust();
            if (calHour == -1) {
                calHour = (timeZone.getRawOffset() == 0) ? 12 : 24;
                calMin = 0;
                calSec = 0;
            }
        }
    }

    private void doYearAdjust() {
        Calendar cDate = TimeUtil.newGmtCalendar();
        SHEFDate sDate = new SHEFDate(cDate);

        int cyr = sDate.getYear();
        int ccc = sDate.getCentury();
        int cly = sDate.getLy();

        if (calCC == -1) {
            if (calLY == -1) {
                Calendar cSys = copy(cDate);

                cSys.set(Calendar.HOUR_OF_DAY, 0);
                cSys.set(Calendar.MINUTE, 0);
                cSys.set(Calendar.SECOND, 0);
                cSys.set(Calendar.MILLISECOND, 0);
                Calendar cObs = copy(cSys);
                cObs.set(Calendar.MONTH, calMonth - 1);
                cObs.set(Calendar.DAY_OF_MONTH, calDay);
                long diff = Math.abs(cObs.getTimeInMillis()
                        - cSys.getTimeInMillis());
                if (diff > ShefConstants.HALF_YEAR) {
                    if (calMonth > cSys.get(Calendar.MONTH) + 1) {
                        cObs.add(Calendar.YEAR, -1);
                    } else {
                        cObs.add(Calendar.YEAR, 1);
                    }
                    calYear = cObs.get(Calendar.YEAR);
                    calCC = calYear / 100;
                    calLY = calYear % 100;
                } else {
                    calYear = cyr;
                    calCC = ccc;
                    calLY = cly;
                }
            } else {
                // Only the century needs to be determined.
                // Get the wall clock yy
                int tyear = (ccc * 100) + calLY;
                if ((tyear - cyr) <= 10) {
                    calYear = tyear;
                    calCC = calYear / 100;
                    calLY = calYear % 100;
                } else {
                    calYear = tyear - 100;
                    calCC = calYear / 100;
                    calLY = calYear % 100;
                }
            }
        } else {
            calYear = (calCC * 100) + calLY;
        }
    }

    /**
     * 
     * @return
     */
    public boolean validate() {
        boolean valid = true;

        if ((calHour < 0) || (calHour > 24)) {
            valid = false;
            error = SHEFErrorCodes.LOG_016;
        } else if (calHour == 24) {
            if (calSec != 0) {
                valid = false;
                error = SHEFErrorCodes.LOG_016;
            } else if (calMin != 0) {
                valid = false;
                error = SHEFErrorCodes.LOG_016;
            }
        }
        if ((calMonth > 0) && (calMonth < 13)) {
            int daysPerMonth = DAYS_MONTH[calMonth];
            if (calMonth == 2) {
                if (!isLeapYear(calYear)) {
                    daysPerMonth--;
                }
            }
            if (calDay > daysPerMonth) {
                valid = false;
                error = SHEFErrorCodes.LOG_016;
            } else if (calDay < 1) {
                valid = false;
                error = SHEFErrorCodes.LOG_016;
            }
        } else {
            valid = false;
            error = SHEFErrorCodes.LOG_016;
        }

        return valid;
    }

    /**
     * 
     * @param token
     *            A ParserToken instance that should be applied to this date
     *            instance.
     * @return A new SHEFDate instance with the token's data applied.
     */
    public SHEFDate applyData(ParserToken token) {
        SHEFDate newDate = null;

        if (ParserToken.DATE_TYPES.containsKey(token.getType())) {
            newDate = new SHEFDate();
            newDate.copyFrom(this);

            SHEFDate d = token.getDateData();

            switch (token.getType()) {
            case DATE_HOUR: {
                // HH[NN[SS]]
                int t = d.getSecond();
                newDate.setSecond((t > -1) ? t : 0);
                t = d.getMinute();
                newDate.setMinute((t > -1) ? t : 0);
                newDate.setHour(d.getHour());
                break;
            }
            case DATE_MIN: {
                // NN[SS]
                int t = d.getSecond();
                newDate.setSecond((t > -1) ? t : 0);
                if (d.getMinute() > -1) {
                    newDate.setMinute(d.getMinute());
                }
                break;
            }
            case DATE_SEC: {
                // SS
                if (d.getSecond() > -1) {
                    newDate.setSecond(d.getSecond());
                }
                break;
            }
            case DATE_DATE: {
                if (d.getCentury() > -1) {
                    newDate.setCentury(d.getCentury());
                }
            }
            case DATE_YEAR: {
                if (d.getCentury() < 0) {
                    newDate.setCentury(-1);
                } else {
                    newDate.setCentury(d.getCentury());
                }
                newDate.setLy(d.getLy());
                newDate.doYearAdjust();
                // Fall through to pick up other fields.
            }
            case DATE_MON: {
                if (d.getMonth() > -1) {
                    newDate.setMonth(d.getMonth());
                }
                // Fall through to pick up other fields.
            }
            case DATE_DAY: {
                if (d.getDay() > -1) {
                    newDate.setDay(d.getDay());
                }
                if (d.getHour() > -1) {
                    newDate.setHour(d.getHour());
                }
                if (d.getMinute() > -1) {
                    newDate.setMinute(d.getMinute());
                }
                if (d.getSecond() > -1) {
                    newDate.setSecond(d.getSecond());
                }
                break;
            }
            case DATE_JUL: {
                newDate = doJulDate(token);
                break;
            }
            }
        } else if (TokenType.DATE_REL.equals(token.getType())) {
            newDate = relative(this, token.getToken());
        } else if (TokenType.INT_CODE.equals(token.getType())) {
            newDate = relative(this, token.getToken());
        }
        if (newDate != null) {
            newDate.validate();
        }

        return newDate;
    }

    /**
     * 
     * @param token
     * @return
     */
    private SHEFDate doJulDate(ParserToken token) {
        SHEFDate newDate = new SHEFDate();
        newDate.copyFrom(this);

        SHEFDate pDate = token.getDateData();
        Calendar c = newDate.toCalendar();
        c.set(Calendar.DAY_OF_YEAR, pDate.getJulian());
        newDate = new SHEFDate(c);
        return newDate;
    }

    //

    /**
     * Check if this date/time is set to the last day of the current month.
     * 
     * @return Is this instance at the last day of the month?
     */
    private boolean isLastDayOfMonth() {
        Calendar c = toCalendar();
        int lastDay = c.getActualMaximum(Calendar.DAY_OF_MONTH);
        return (lastDay == calDay);
    }

    /**
     * Increment this object.
     * 
     * @param value
     *            amount to increment
     * @param type
     *            the field to incrment
     */
    public void inc(int value, TIME_DIVISIONS type) {
        int seconds = 0;
        int minutes = 0;
        int hours = 0;
        int days = 0;

        // Note that SECONDS through HOURS fall through to the next case
        // to ensure all values are adjusted. for carry.
        switch (type) {
        case SECONDS: {
            minutes = value / 60;
            seconds = calSec + (value - (minutes * 60));
            if (seconds < 0) {
                seconds += 60;
                minutes--;
            } else if (seconds >= 60) {
                seconds -= 60;
                minutes++;
            }
            calSec = seconds;
            calMin = minutes + calMin;
            value = 0;
        }
        case MINUTES: {
            hours = value / 60;
            minutes = calMin + (value - (60 * hours));
            if (minutes < 0) {
                minutes += 60;
                hours--;
            } else if (minutes >= 60) {
                minutes -= 60;
                hours++;
            }
            calMin = minutes;
            calHour = hours + calHour;
            value = 0;
        }
        case HOURS: {
            days = value / 24;
            hours = calHour + (value - (24 * days));
            if (hours < 0) {
                hours += 24;
                days--;
            } else if (hours >= 23) {
                hours -= 24;
                days++;
            }
            calHour = hours;
            calDay = days + calDay;
            value = (calDay < 0) ? days : 0;
        }
        case DAYS: {
            doJulian1();
            doJulian2(value);

            int tJul = calJul;
            int ii = 1 + ((calYear + 3) / 4) - (calYear / 4);
            ii += getInc(calYear);
            if (calJul > (61 - ii)) {
                tJul += ii;
            }
            calMonth = ((tJul * 67) + 2012) / 2048;
            calDay = tJul - (calMonth * 489) / 16 + 30;
            break;
        }
        case MONTHS: {
            int month = calMonth + value;
            int year = calYear;
            while ((month < 1) || (month > 12)) {
                if (month > 12) {
                    month -= 12;
                    year++;
                    if (year == 100) {
                        year = 0;
                    }
                } else {
                    month += 12;
                    year--;
                    if (year == -1) {
                        year = 99;
                    }
                }
            } // while
            calMonth = month;
            calYear = year;
            break;
        }
        case YEARS: {
            int year = calYear + value;
            if ((year >= 0) && (year <= 99)) {
                year = year - (100 * (year / 100));
            } else {
                if ((year < 1753) || (year > 2199)) {
                    error = SHEFErrorCodes.LOG_039;
                }
            }
            calYear = year;
            break;
        }
        case ENDOFMONTH: {
            int year = calYear;
            int month = calMonth;
            int day = calDay;

            int dayMonth = MXD[month];
            // adjust Feb. for leap year if required.
            if ((month == 2) && isLeapYear(year)) {
                dayMonth++;
            }
            if (dayMonth != day) {
                error = SHEFErrorCodes.LOG_038;
            } else {
                month += value;
                while ((month < 1) || (month > 12)) {
                    if (month > 12) {
                        month -= 12;
                        year++;
                        if (year == 100) {
                            year = 0;
                        }
                    } else {
                        month += 12;
                        year--;
                        if (year == -1) {
                            year = 99;
                        }
                    }
                } // while
                day = MXD[month];
                if ((month == 2) && isLeapYear(year)) {
                    day++;
                }
                if (year >= 100) {
                    if (!isLegalYear(year)) {
                        error = SHEFErrorCodes.LOG_039;
                    }
                }
                calYear = year;
                calMonth = month;
                calDay = day;
            }
        }
        }
        calLY = calYear % 100;
        calCC = calYear / 100;
    }

    /**
     * Copy the data from a given SHEF date instance into this instance.
     * 
     * @param date
     *            A SHEFDate to copy from.
     */
    public void copyFrom(SHEFDate date) {
        if (date != null) {
            calYear = date.calYear;
            calCC = date.calCC;
            calLY = date.calLY;
            calMonth = date.calMonth;
            calDay = date.calDay;
            calHour = date.calHour;
            calMin = date.calMin;
            calSec = date.calSec;
            timeZone = date.timeZone;
        }
    }

    /**
     * Forces this SHEFDate instance to Zulu timezone.
     */
    public void toZuluDate() {
        // Check if we are already a Zulu date!
        if (calHour == 24) {
            calHour = 0;
            inc(1, TIME_DIVISIONS.DAYS);
        }
        if (!ShefConstants.Z.equals(timeZone.getID())) {
            TimeZone tzz = SHEFTimezone.getSysTimeZone(ShefConstants.Z);

            boolean retard = false;
            if (calHour == 1) {
                GregorianCalendar g = (GregorianCalendar) toCalendar().clone();
                if (!timeZone.inDaylightTime(g.getTime())) {
                    g.set(Calendar.HOUR_OF_DAY, 0);

                    retard = timeZone.inDaylightTime(g.getTime());
                }
            }

            // Actually if the following doesn't work we have a
            // real big problem!
            if (tzz != null) {
                Calendar c = toCalendar();
                Calendar cz = TimeUtil.newCalendar(tzz);
                cz.setTimeInMillis(c.getTimeInMillis());

                calYear = cz.get(Calendar.YEAR);
                calMonth = cz.get(Calendar.MONTH) + 1;
                calDay = cz.get(Calendar.DAY_OF_MONTH);
                calHour = cz.get(Calendar.HOUR_OF_DAY);
                if (retard) {
                    calHour--;
                }
                calMin = cz.get(Calendar.MINUTE);
                calSec = cz.get(Calendar.SECOND);
                timeZone = tzz;
            }
        }
    }

    /**
     * Return a calendar representation of this SHEF date.
     * 
     * @return
     */
    public Calendar toCalendar() {
        Calendar c = TimeUtil.newCalendar(timeZone);
        c.set(Calendar.YEAR, calYear);
        c.set(Calendar.MONTH, calMonth - 1);
        c.set(Calendar.DAY_OF_MONTH, calDay);
        c.set(Calendar.HOUR_OF_DAY, calHour);
        c.set(Calendar.MINUTE, calMin);
        c.set(Calendar.SECOND, calSec);
        c.set(Calendar.MILLISECOND, 0);

        return c;
    }

    /**
     * Check to see if the date/time is in the DST exclusion zone. 02:00:00.000
     * .. 02:59:59.000
     * 
     * @return
     */
    public boolean isDSTExclusion() {
        boolean isExcluded = false;

        Calendar c = TimeUtil.newCalendar(timeZone);
        c.set(Calendar.YEAR, calYear);
        c.set(Calendar.MONTH, calMonth - 1);
        c.set(Calendar.DAY_OF_MONTH, calDay);
        c.set(Calendar.HOUR_OF_DAY, calHour);
        c.set(Calendar.MINUTE, calMin);
        c.set(Calendar.SECOND, calSec);
        c.set(Calendar.MILLISECOND, 0);
        // Are we in DST?
        if (timeZone.inDaylightTime(c.getTime())) {
            if ((calHour == 2)) {
                if ((calMin >= 0) && (calMin < 60)) {
                    // We're in a possible exclusion zone
                    // Set the hour to 1.
                    // If that time is NOT in DST then we're
                    // in transition date.
                    c.set(Calendar.HOUR_OF_DAY, 1);
                    isExcluded = !timeZone.inDaylightTime(c.getTime());
                }
            }
        }
        return isExcluded;
    }

    /**
     * Returns this date formatted to the local timezone.
     * 
     * @return
     */
    public String toLocal() {
        return toString();
    }

    @Override
    public String toString() {
        return String.format(DT_FMT, calCC, calLY, calMonth, calDay, calHour,
                calMin, calSec);
    }

    public String toOutString() {
        return String.format(OUT_FMT, calCC, calLY, calMonth, calDay, calHour,
                calMin, calSec);
    }

    // *******************************************

    /**
     * 
     * @param mon
     * @param day
     * @param tz
     * @return
     */
    public static Calendar getDateMonDay(int mon, int day, TimeZone tz) {
        Calendar cSys = TimeUtil.newGmtCalendar();
        cSys.setTimeZone(tz);
        cSys.set(Calendar.HOUR_OF_DAY, 0);
        cSys.set(Calendar.MINUTE, 0);
        cSys.set(Calendar.SECOND, 0);
        cSys.set(Calendar.MILLISECOND, 0);

        Calendar cObs = copy(cSys);
        cObs.set(Calendar.MONTH, mon - 1);
        cObs.set(Calendar.DAY_OF_MONTH, day);

        long diff = Math.abs(cObs.getTimeInMillis() - cSys.getTimeInMillis());
        if (diff > ShefConstants.HALF_YEAR) {
            if (mon > cSys.get(Calendar.MONTH) + 1) {
                cObs.add(Calendar.YEAR, -1);
            } else {
                cObs.add(Calendar.YEAR, 1);
            }
        }
        return cObs;
    }

    /**
     * 
     * @param year
     * @param mon
     * @param day
     * @param tz
     * @return
     */
    public static Calendar getDateYearMon(int year, int mon, int day,
            TimeZone tz) {
        Calendar cObs = null;
        Calendar cSys = TimeUtil.newGmtCalendar();
        cSys.setTimeZone(tz);

        cSys.set(Calendar.DAY_OF_MONTH, day);
        cSys.set(Calendar.HOUR_OF_DAY, 0);
        cSys.set(Calendar.MINUTE, 0);
        cSys.set(Calendar.SECOND, 0);
        cSys.set(Calendar.MILLISECOND, 0);
        if (year < 100) {
            // Century of the current year.
            int cc = (cSys.get(Calendar.YEAR) / 100) * 100;

            cObs = copy(cSys);
            cObs.set(Calendar.MONTH, mon - 1);

            // check for up to 10 years in the future.
            int diff = (cc + year) - cSys.get(Calendar.YEAR);
            if (diff <= 10) {
                cObs.set(Calendar.YEAR, (cc + year));
            } else {
                cObs.set(Calendar.YEAR, ((cc - 100) + year));
            }
        } else {
            cObs = copy(cSys);
            cObs.set(Calendar.YEAR, year);
            cObs.set(Calendar.MONTH, mon - 1);
            cObs.set(Calendar.DAY_OF_MONTH, day);
        }

        return cObs;
    }

    /**
     * 
     * @param sDate
     * @return
     */
    private static final SHEFDate relative(SHEFDate sDate, String incCode) {
        SHEFDate newDate = new SHEFDate(sDate);

        Matcher m = TokenType.DATE_REL.getPattern().matcher(incCode);
        if (m.find()) {
            String code = m.group(2);

            int pos = DATE_REL_CODES.indexOf(code);
            int incVal = -999;
            if (pos >= 0) {
                incVal = Integer.parseInt(m.group(4));
                incVal *= ("-".equals(m.group(3))) ? -1 : 1;

                newDate.inc(incVal, DIVISIONS.get(code));
            }
        }
        return newDate;
    }

    /**
     * 
     * @param sDate
     * @param incCode
     * @param seriesId
     * @return the incremented SHEFDate
     */
    public static final SHEFDate increment(SHEFDate sDate, String incCode,
            int seriesId) {

        Matcher m = TokenType.INT_CODE.getPattern().matcher(incCode);
        if (m.find()) {
            SHEFDate newDate = new SHEFDate(sDate);
            String code = m.group(2);
            // If we are trying to increment "End-of-month" ensure that
            // the date "day" is at the end of the month.
            if ("E".equals(code) && !sDate.isLastDayOfMonth()) {
                return newDate;
            }
            if (seriesId >= 2) {
                int pos = DATE_INC_CODES.indexOf(code);
                if (pos >= 0) {
                    int incVal = Integer.parseInt(m.group(4));
                    incVal *= ("-".equals(m.group(3))) ? -1 : 1;
                    incVal *= (seriesId - 1);

                    newDate.inc(incVal, DIVISIONS.get(code));
                }
            }
            return newDate;
        }

        return sDate;
    }

    /**
     * Return a copy of the calendar passed in.
     * 
     * @param cal
     *            The Calendar to copy
     * @return The new Calendar
     */
    private static Calendar copy(Calendar cal) {
        if (cal == null) {
            return cal;
        }

        Calendar copy = TimeUtil.newGmtCalendar();
        copy.setTimeInMillis(cal.getTimeInMillis());
        return copy;
    }

    public static final void main(String[] args) {

        // List<ParserToken> tokens = new ArrayList<ParserToken>();
        // tokens.add(new ParserToken("DS30", TokenType.DATE_SEC));
        // tokens.add(new ParserToken("DN21", TokenType.DATE_MIN));
        // tokens.add(new ParserToken("DN2115", TokenType.DATE_MIN));
        // tokens.add(new ParserToken("DH15", TokenType.DATE_HOUR));
        // tokens.add(new ParserToken("DH1521", TokenType.DATE_HOUR));
        // tokens.add(new ParserToken("DH152115", TokenType.DATE_HOUR));
        //
        // tokens.add(new ParserToken("DM033115", TokenType.DATE_MON));
        // tokens.add(new ParserToken("DRE-1", TokenType.DATE_REL));
        //
        // SHEFDate d = new SHEFDate(20,8,1,31,14,21,15);
        // d.setTimeZone(SHEFTimezone.getSysTimeZone("C"));
        //
        // System.out.println(d);
        // System.out.println("*****************************************************");
        // for(ParserToken t : tokens) {
        // System.out.print(String.format("%-10s%-20s",t.getType().name(),t.getToken()));
        // System.out.println(d.applyData(t));
        // System.out.println("-----------------------------------------------------");
        // }

        // SHEFDate d = new SHEFDate(new Date(), TimeZone.getTimeZone("Z"));
        //
        // SHEFDate date = new SHEFDate(20,11,2,6,12,0,0);
        // TimeZone zone = SHEFTimezone.getSysTimeZone("Z");
        // date.setTimeZone(zone);
        // date.adjustToTimezone();
        // System.out.println(date);
        //
        // date = date.applyData(new ParserToken("DJ036",TokenType.DATE_JUL));
        // System.out.println(date);
        //
        // date = date.applyData(new ParserToken("DH12",TokenType.DATE_HOUR));
        // System.out.println(date);
        //
        // Calendar c = getDateYearMon(82, 8, 8, zone);
        // date = new SHEFDate(c);
        // System.out.println(date);
        //
        // zone = SHEFTimezone.getSysTimeZone("C");
        // date = new SHEFDate();
        // date.setCentury(-1);
        // date.setLy(82);
        // date.setMonth(4);
        // date.setDay(25);
        // date.setHour(1);
        // date.setMinute(59);
        // date.setSecond(59);
        //
        // date.setTimeZone(zone);
        // System.out.println(date);
        // date.adjustToTimezone();
        // System.out.println(date);
        // System.out.println("DST Exclusion " + date.isDSTExclusion());
        //
        // ParserToken t = new ParserToken("820229",TokenType.OBS_DATE_6);
        //
        // t.adjustToTimezone(zone);
        //
        // date = new SHEFDate();
        // date.setYear(2010);
        // date.setMonth(3);
        // date.setDay(14);
        // date.setHour(2);
        // date.setMinute(0);
        // date.setSecond(1);
        //
        // date.setTimeZone(zone);
        // System.out.println(date);
        // date.adjustToTimezone();
        //
        // date = SHEFDate.increment(date, "DIH1", 2);
        // System.out.println(date);
        // date = SHEFDate.increment(date, "DIN20", 2);
        // System.out.println(date);
        // date = SHEFDate.increment(date, "DIS10", 2);
        //
        // String s = date.toString();
        // date = SHEFDate.increment(date,"DIM3", 2);
        // date = SHEFDate.increment(date,"DIM-9", 2);
        // date = SHEFDate.increment(date,"DIM6", 2);
        // System.out.println("pass = " + (s.equals(date.toString())));

        // for(String s : new String[] { "ED", "CD", "MD", "PD", }) {
        // SHEFDate date = new SHEFDate(20, 11, 5, 23, 3, 0, 0);
        // TimeZone zone = SHEFTimezone.getSysTimeZone(s);
        // date.setTimeZone(zone);
        // date.toZuluDate();
        // System.out.println(date);
        // }

        // zone = SHEFTimezone.getSysTimeZone("PS");
        // date = new SHEFDate();
        // date.setYear(1982);
        // date.setMonth(2);
        // date.setDay(8);
        // date.setHour(4);
        // date.setMinute(0);
        // date.setSecond(0);
        //
        // date.setTimeZone(zone);
        //
        // date.toZuluDate();
        // System.out.println(date);
        //
        // zone = SHEFTimezone.getSysTimeZone("C");
        // date = new SHEFDate();
        // date.setYear(1930);
        // date.setMonth(04);
        // date.setDay(20);
        // date.setHour(7);
        // date.setMinute(0);
        // date.setSecond(0);
        //
        // date.setTimeZone(zone);
        //
        // date.toZuluDate();
        // System.out.println(date);
        //
        // zone = SHEFTimezone.getSysTimeZone("Z");
        // date = new SHEFDate();
        // date.setYear(2011);
        // date.setMonth(8);
        // date.setDay(13);
        // date.setHour(0);
        // date.setMinute(0);
        // date.setSecond(0);
        // date.setTimeZone(zone);
        //
        // date = date.applyData(new ParserToken("DT2001",TokenType.DATE_DATE));
        // System.out.println(date);

        TimeZone zone = SHEFTimezone.getSysTimeZone("Z");
        SHEFDate date = new SHEFDate();
        date.setYear(2011);
        date.setMonth(1);
        date.setDay(1);
        date.setHour(0);
        date.setMinute(0);
        date.setSecond(29);

        date.setTimeZone(zone);

        System.out.println(date);

        date.inc(-30, TIME_DIVISIONS.SECONDS);
        System.out.println(date);

        date.inc(1, TIME_DIVISIONS.MONTHS);
        System.out.println(date);

        date.inc(6, TIME_DIVISIONS.HOURS);
        System.out.println(date);

    }
}
