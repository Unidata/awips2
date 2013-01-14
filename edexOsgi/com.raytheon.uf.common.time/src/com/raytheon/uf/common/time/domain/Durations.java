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
package com.raytheon.uf.common.time.domain;

import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.time.domain.api.IDuration;
import com.raytheon.uf.common.time.domain.api.ITimePoint;

/**
 * Retrieve {@link IDuration}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 11, 2013 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class Durations {

    private static final Pattern DURATION_PATTERN = Pattern
            .compile("\\s*(\\d+)\\s+([^\\s]+)\\s*");

    public static final IDuration ZERO = Durations
            .of(0L, TimeUnit.MILLISECONDS);

    /**
     * Retrieve a {@link IDuration} of the specified value and unit.
     * 
     * @param value
     * @param unit
     * @return the duration
     */
    public static IDuration of(long value, TimeUnit unit) {
        return new Duration(value, unit);
    }

    /**
     * Retrieve the duration between two {@link TimePoint}s.
     * 
     * @param start
     *            the starting time point
     * @param end
     *            the ending time point
     * @return the duration between the two points
     */
    public static IDuration between(ITimePoint start, ITimePoint end) {
        final long millis = end.asMilliseconds() - start.asMilliseconds();
        return of(millis, TimeUnit.MILLISECONDS);
    }

    /**
     * Parse a string representation of a {@link Duration}.
     * 
     * @param asString
     *            the string representation of the duration
     * 
     * @return the duration
     * @throws IllegalArgumentException
     *             if the argument cannot be parsed into a duration
     */
    public static IDuration fromString(String asString) {
        final Matcher m = DURATION_PATTERN.matcher(asString);
        if (m.matches()) {
            return of(Long.parseLong(m.group(1)), TimeUnit.valueOf(m.group(2)));
        }

        throw new IllegalArgumentException("The argument [" + asString
                + "] does not match a duration!");
    }

    /**
     * Convert the {@link IDuration} to a string representation. This method
     * figures out the "optimal" unit to display the time in, e.g. 120 minutes
     * would display as "2 HOURS", but 128 minutes will be displayed as "128
     * MINUTES".
     * 
     * @param duration
     *            the duration instance
     * @return the string representation
     */
    public static String toString(IDuration duration) {
        TimeUnit timeUnitForDisplay = TimeUnit.DAYS;

        final TimeUnit[] values = TimeUnit.values();
        final long nanos = duration.getNanos();
        for (int unitIdx = 1; unitIdx < values.length; unitIdx++) {
            final TimeUnit timeUnitToTry = values[unitIdx];

            // If we would lose precision, then use the time unit before this
            // one
            final long valueInNewTimeUnit = timeUnitToTry.convert(nanos,
                    TimeUnit.NANOSECONDS);
            final boolean wouldLosePrecision = timeUnitToTry
                    .toNanos(valueInNewTimeUnit) != nanos;
            if (wouldLosePrecision) {
                timeUnitForDisplay = values[unitIdx - 1];
                break;
            }
        }

        // Now construct and return the pretty version of the String
        final long convertedValue = timeUnitForDisplay.convert(nanos,
                TimeUnit.NANOSECONDS);
        return convertedValue + " " + timeUnitForDisplay.toString();
    }

    /**
     * Disabled constructor.
     */
    private Durations() {
    }
}
