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
package com.raytheon.uf.edex.plugin.mpe.biasmesgen;

import java.util.Calendar;

/**
 * POJO representation of the bias table date. Essentially wraps a
 * {@link Calendar} and provides access to the year, month, day, hour, minute,
 * and second in a way that is usable to the bias table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2016 5576       bkowal      Initial creation
 * Jun 13, 2016 5576       bkowal      Added {@link #toString()}, {@link #equals(Object)},
 *                                     and {@link #hashCode()}.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class BiasTableDate {

    public static final int NUM_BYTES = 12;

    private final Calendar calendar;

    public BiasTableDate(Calendar calendar) {
        if (calendar == null) {
            throw new IllegalArgumentException(
                    "Required argument 'calendar' cannot be NULL.");
        }
        this.calendar = calendar;
    }

    /**
     * Constructor
     * 
     * @param year
     *            the four-digit {@link Calendar} year.
     * @param month
     *            the {@link Calendar} month. Expected to be in the range: [1 -
     *            12].
     * @param day
     *            the {@link Calendar} date
     * @param hour
     *            the {@link Calendar} hour of day based on a 24-hour clock.
     * @param minute
     *            the {@link Calendar} minute. Expected to be in the range: [0 -
     *            59].
     * @param second
     *            the {@link Calendar} second. Expected to be in the range: [0 -
     *            59].
     */
    public BiasTableDate(int year, int month, int day, int hour, int minute,
            int second) {
        if (month < 1 || month > 12) {
            throw new IllegalArgumentException(
                    "An invalid month: "
                            + month
                            + " has been specified. The specified month must be >= 1 and <= 12.");
        }
        if (hour < 0 || hour > 23) {
            throw new IllegalArgumentException(
                    "An invalid hour: "
                            + hour
                            + " has been specified. The specified hour must be >= 0 and <= 23.");
        }
        if (minute < 0 || minute > 59) {
            throw new IllegalArgumentException(
                    "An invalid minute: "
                            + minute
                            + " has been specified. The specified minute must be >= 0 and <= 59.");
        }
        if (second < 0 || second > 59) {
            throw new IllegalArgumentException(
                    "An invalid second: "
                            + second
                            + " has been specified. The specified second must be >= 0 and <= 59.");
        }

        this.calendar = Calendar.getInstance();
        calendar.set(Calendar.YEAR, year);
        calendar.set(Calendar.MONTH, (month - 1));
        final int dayMin = calendar.getMinimum(Calendar.DATE);
        final int dayMax = calendar.getMaximum(Calendar.DATE);
        if (day < dayMin || day > dayMax) {
            throw new IllegalArgumentException("An invalid day: " + day
                    + " has been specified. The specified day must be >= "
                    + dayMin + " and <= " + dayMax + ".");
        }
        calendar.set(Calendar.DATE, day);
        calendar.set(Calendar.HOUR_OF_DAY, hour);
        calendar.set(Calendar.MINUTE, minute);
        calendar.set(Calendar.SECOND, second);
    }

    public short getYear() {
        return (short) calendar.get(Calendar.YEAR);
    }

    /**
     * Returns the {@link Calendar} month. The month returned will be in the
     * range [1 - 12] in which 1 is representative of January, the first month,
     * and 12 is representative of December, the last month.
     * 
     * @return a numerical value representative of the month
     */
    public short getMonth() {
        return (short) (calendar.get(Calendar.MONTH) + 1);
    }

    public short getDay() {
        return (short) calendar.get(Calendar.DATE);
    }

    public short getHour() {
        return (short) calendar.get(Calendar.HOUR_OF_DAY);
    }

    public short getMinute() {
        return (short) calendar.get(Calendar.MINUTE);
    }

    public short getSecond() {
        return (short) calendar.get(Calendar.SECOND);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("BiasTableDate [");
        sb.append("year=").append(getYear());
        sb.append(", month=").append(getMonth());
        sb.append(", day=").append(getDay());
        sb.append(", hour=").append(getHour());
        sb.append(", minute=").append(getMinute());
        sb.append(", second=").append(getSecond());
        sb.append("]");
        return sb.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((calendar == null) ? 0 : calendar.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        BiasTableDate other = (BiasTableDate) obj;
        if (calendar == null) {
            if (other.calendar != null) {
                return false;
            }
        } else if (other.calendar == null) {
            return false;
        }
        if (getYear() != other.getYear()) {
            return false;
        }
        if (getDay() != other.getDay()) {
            return false;
        }
        if (getMonth() != other.getMonth()) {
            return false;
        }
        if (getHour() != other.getHour()) {
            return false;
        }
        if (getMinute() != other.getMinute()) {
            return false;
        }
        if (getSecond() != other.getSecond()) {
            return false;
        }
        return true;
    }
}