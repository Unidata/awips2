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

package com.raytheon.uf.common.dataplugin.gfe.time;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Select Time Range
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------------
 * Jun 24, 2008           mnash     Initial creation
 * Aug 01, 2012  965      dgilling  Moved to dataplugin.gfe project.
 * Feb 28, 2018  7062     randerso  Fix toTimeRange for DST transition
 *
 * </pre>
 *
 * @author mnash
 */
public class SelectTimeRange implements Comparable<SelectTimeRange> {

    /**
     * Time Range Mode
     */
    public static enum Mode {
        /** Local Time */
        LT,

        /** GMT */
        ZULU,

        /** Local Standard Time */
        LST
    };

    private final String name;

    private final int start;

    private final int end;

    private final Mode mode;

    private final LocalizationLevel level;

    private final TimeZone timeZone;

    /**
     * Constructor
     *
     * @param name
     *            display name
     * @param start
     *            starting hour relative to midnight of the current day
     * @param end
     *            ending hour relative to midnight of the current day
     * @param mode
     *            Zulu, local daylight, or local standard see {@link Mode}
     * @param level
     *            the LocalizationLevel
     * @param timeZone
     *            the local time zone
     */
    public SelectTimeRange(String name, int start, int end, Mode mode,
            LocalizationLevel level, TimeZone timeZone) {
        this.name = name;
        this.start = start;
        this.end = end;
        this.mode = mode;
        this.level = level;
        this.timeZone = timeZone;
    }

    /**
     * Gets the name by which this SelectTimeRange is known.
     *
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * Gets the start time, in hours, of this SelectTimeRange relative to
     * midnight in the designated time mode.
     *
     * @return the start
     */
    public int getStart() {
        return start;
    }

    /**
     * Gets the end time, in hours, of this SelectTimeRange relative to midnight
     * in the designated time mode.
     *
     * @return the end
     */
    public int getEnd() {
        return end;
    }

    /**
     * Gets the time zone mode
     *
     * @return the mode
     */
    public Mode getMode() {
        return mode;
    }

    @Override
    public int compareTo(SelectTimeRange other) {
        if (start != other.start) {
            return start - other.start;
        } else {
            return end - other.end;
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + end;
        result = prime * result + start;
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
        SelectTimeRange other = (SelectTimeRange) obj;
        if (end != other.end) {
            return false;
        }
        if (start != other.start) {
            return false;
        }
        return true;
    }

    /**
     * Convert this SelectTimeRange to an actual TimeRange based on the current
     * time.
     *
     * @return the actual TimeRange
     */
    public TimeRange toTimeRange() {
        return toTimeRange(SimulatedTime.getSystemTime().getTime());
    }

    /**
     * Convert this SelectTimeRange to an actual TimeRange based on the supplied
     * time now.
     *
     * @param now
     * @return the actual TimeRange
     */
    public TimeRange toTimeRange(Date now) {
        TimeRange result;

        int offset = timeZone.getOffset(now.getTime());
        if (mode.equals(Mode.LST) && timeZone.inDaylightTime(now)) {
            offset -= timeZone.getDSTSavings();
        }

        // Find most recent midnight in mode.
        Calendar cal = GregorianCalendar.getInstance();
        cal.setTimeZone(timeZone);
        cal.setTime(now);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        int year = cal.get(Calendar.YEAR);
        int month = cal.get(Calendar.MONTH);
        int date = cal.get(Calendar.DAY_OF_MONTH);

        cal.setTimeZone(TimeZone.getTimeZone("GMT"));

        cal.set(year, month, date, 0, 0, 0);
        cal.add(Calendar.HOUR, start);
        long trStart = cal.getTimeInMillis() - offset;

        cal.set(year, month, date, 0, 0, 0);
        cal.add(Calendar.HOUR, end);
        long trEnd = cal.getTimeInMillis() - offset;

        result = new TimeRange(trStart, trEnd);

        return result;
    }

    /**
     * @return the localization level for this SelectTimeRange
     */
    public LocalizationLevel getLevel() {
        return this.level;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getClass().getSimpleName()).append('{');
        sb.append(name);
        sb.append(" (").append(start).append(',').append(end).append(") ");
        sb.append(mode.name());
        sb.append('}');

        return sb.toString();
    }
}
