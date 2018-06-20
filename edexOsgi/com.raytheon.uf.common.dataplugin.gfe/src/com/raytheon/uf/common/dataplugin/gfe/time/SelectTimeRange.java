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
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jun 24, 2008             mnash       Initial creation
 * Aug 01, 2012   #965      dgilling    Moved to dataplugin.gfe project.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class SelectTimeRange implements Comparable<SelectTimeRange> {

    /**
     * Time Range Mode: LT = Local Time, ZULU = GMT, LST = Local Standard Time
     */
    public static enum Mode {
        LT, ZULU, LST
    };

    private static final long MILLI_PER_HOUR = 3600000;

    private final String name;

    private final int start;

    private final int end;

    private final Mode mode;

    private final LocalizationLevel level;

    private final TimeZone timeZone;

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

    /**
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(SelectTimeRange other) {
        if (start != other.start) {
            return start - other.start;
        } else {
            return end - other.end;
        }
    }

    public TimeRange toTimeRange() {
        return toTimeRange(SimulatedTime.getSystemTime().getTime());
    }

    public TimeRange toTimeRange(Date now) {
        TimeRange result;
        // Find most recent midnight in mode.
        Calendar cal = GregorianCalendar.getInstance();
        cal.setTimeZone(timeZone);
        cal.setTime(now);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        if (mode.equals(Mode.LST) && timeZone.inDaylightTime(now)) {
            cal.add(Calendar.MILLISECOND, -timeZone.getDSTSavings());
        }

        long midnight = cal.getTimeInMillis();
        // Add start and end hours to midnight
        long start = midnight + getStart() * MILLI_PER_HOUR;
        long end = midnight + getEnd() * MILLI_PER_HOUR;
        result = new TimeRange(start, end);

        return result;
    }

    public LocalizationLevel getLevel() {
        return this.level;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
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
