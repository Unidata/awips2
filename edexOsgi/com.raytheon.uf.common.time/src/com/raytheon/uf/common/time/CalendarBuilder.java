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
package com.raytheon.uf.common.time;

import java.util.Calendar;
import java.util.TimeZone;

import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Build {@link Calendar} objects. Note: Defaults to the GMT timezone since that
 * is most often used.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 11, 2013 1453       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class CalendarBuilder {

    private static final int UNSET = -1;

    private int year = UNSET;

    private int month = UNSET;

    private int dayOfMonth = UNSET;

    private int hourOfDay = UNSET;

    private int minute = UNSET;

    private int second = UNSET;

    private int millisecond = UNSET;

    private TimeZone timeZone = TimeZone.getTimeZone("GMT");

    /**
     * @param timeZone
     *            the timeZone to set
     * @return a {@link CalendarBuilder} to keep building with
     */
    public CalendarBuilder withTimeZone(TimeZone timeZone) {
        this.timeZone = timeZone;
        return this;
    }

    /**
     * @param year
     *            the year to set
     * @return a {@link CalendarBuilder} to keep building with
     */
    public CalendarBuilder withYear(int year) {
        this.year = year;
        return this;
    }

    /**
     * @param month
     *            the month to set
     * @return a {@link CalendarBuilder} to keep building with
     */
    public CalendarBuilder withMonth(int month) {
        this.month = month;
        return this;
    }

    /**
     * @param dayOfMonth
     *            the dayOfMonth to set
     * @return a {@link CalendarBuilder} to keep building with
     */
    public CalendarBuilder withDayOfMonth(int dayOfMonth) {
        this.dayOfMonth = dayOfMonth;
        return this;
    }

    /**
     * @param hourOfDay
     *            the hourOfDay to set
     * @return a {@link CalendarBuilder} to keep building with
     */
    public CalendarBuilder withHourOfDay(int hour) {
        this.hourOfDay = hour;
        return this;
    }

    /**
     * @param minute
     *            the minute to set
     * @return a {@link CalendarBuilder} to keep building with
     */
    public CalendarBuilder withMinute(int minute) {
        this.minute = minute;
        return this;
    }

    /**
     * @param second
     *            the second to set
     * @return a {@link CalendarBuilder} to keep building with
     */
    public CalendarBuilder withSecond(int second) {
        this.second = second;
        return this;
    }

    /**
     * @param millisecond
     *            the millisecond to set
     * @return a {@link CalendarBuilder} to keep building with
     */
    public CalendarBuilder withMillisecond(int millisecond) {
        this.millisecond = millisecond;
        return this;
    }

    public Calendar build() {
        Calendar calendar = TimeUtil.newCalendar(timeZone);

        setIfRequired(Calendar.YEAR, year, calendar);
        setIfRequired(Calendar.MONTH, month, calendar);
        setIfRequired(Calendar.DAY_OF_MONTH, dayOfMonth, calendar);
        setIfRequired(Calendar.HOUR_OF_DAY, hourOfDay, calendar);
        setIfRequired(Calendar.MINUTE, minute, calendar);
        setIfRequired(Calendar.SECOND, second, calendar);
        setIfRequired(Calendar.MILLISECOND, millisecond, calendar);

        return calendar;
    }

    /**
     * Set the {@link Calendar} field if required.
     * 
     * @param field
     *            the calendar field constant, example: {@link Calendar#YEAR}.
     * @param value
     *            the int value
     * @param calendar
     *            the calendar instance
     */
    private void setIfRequired(int field, int value, Calendar calendar) {
        if (value != UNSET) {
            calendar.set(field, value);
        }
    }

}
