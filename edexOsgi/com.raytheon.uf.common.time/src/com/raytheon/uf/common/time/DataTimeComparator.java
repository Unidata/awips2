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

import java.util.Comparator;

/**
 * Provides configurable comparisons of DataTimes. This can which
 * characteristics of a DataTime object, reference time, valid time, or forecast
 * time, affect how relational operators >, <, >=, and <= behave.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 08, 2013  2245     bsteffen    Initial creation
 * Oct 14, 2013  2468     bsteffen    Use Date for validTime comparisons.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DataTimeComparator implements Comparator<DataTime> {

    /** Defines possible time sort keys */
    public static enum SortKey {
        INITIAL_TIME, FORECAST_TIME, VALID_TIME
    };

    /** The major sort key */
    protected final SortKey majorKey;

    /** The minor sort key */
    protected final SortKey minorKey;

    /** Is data to be sorted using match mode */
    protected final boolean matchMode;

    /**
     * This routine determines which characteristics of a DataTime object,
     * reference time, valid time, or forecast time, affect how relational
     * operators >, <, >=, and <= behave.
     * 
     * @param majorKey
     *            the major sort key
     * @param minorKey
     *            the minor sort key
     * @param matchMode
     *            the match mode flag
     */
    public DataTimeComparator(SortKey majorKey, SortKey minorKey,
            boolean matchMode) {
        this.majorKey = majorKey;
        this.minorKey = minorKey;
        this.matchMode = matchMode;
    }

    @Override
    public int compare(DataTime time1, DataTime time2) {
        int result = compare(majorKey, time1, time2);
        if (result != 0) {
            return result;
        }
        result = compare(minorKey, time1, time2);
        if (result != 0) {
            return result;
        }

        result = compareLevel(time1, time2);
        if (result != 0) {
            return result;
        }
        return compareValidPeriod(time1, time2);
    }

    private int compare(SortKey sortKey, DataTime time1, DataTime time2) {
        if (matchMode) {
            return compareMatch(sortKey, time1, time2);
        } else {
            return compareNoMatch(sortKey, time1, time2);
        }
    }

    private int compareNoMatch(SortKey sortKey, DataTime time1, DataTime time2) {
        switch (sortKey) {
        case INITIAL_TIME:
            return longCompare(time1.getRefTime().getTime(), time2
                    .getRefTime().getTime());
        case FORECAST_TIME:
            return integerCompare(time1.getFcstTime(), time2.getFcstTime());
        case VALID_TIME:
            return longCompare(time1.getValidTimeAsDate().getTime(), time2
                    .getValidTimeAsDate().getTime());
        default:
            throw new IllegalArgumentException(String.valueOf(sortKey)
                    + " is not a recognized SortKey.");
        }
    }

    private int compareMatch(SortKey sortKey, DataTime time1, DataTime time2) {
        switch (sortKey) {
        case INITIAL_TIME:
            return longCompare(time1.getMatchRef(), time2.getMatchRef());
        case FORECAST_TIME:
            return longCompare(time1.getMatchFcst(), time2.getMatchFcst());
        case VALID_TIME:
            return longCompare(time1.getMatchValid(), time2.getMatchValid());
        default:
            throw new IllegalArgumentException(String.valueOf(sortKey)
                    + " is not a recognized SortKey.");
        }
    }

    private int compareLevel(DataTime time1, DataTime time2) {
        return Double.compare(time1.getLevelValue(), time2.getLevelValue());
    }

    /**
     * For valid period the rules arbitrary but we need some rules to make the
     * comparator consistent.
     * 
     * 1. Not null periods are greater than null periods. <br />
     * 2. Longer periods are greater than shorter periods <br />
     * 3. Periods that start later are greater than periods that start earlier.
     */
    private int compareValidPeriod(DataTime time1, DataTime time2) {
        TimeRange p1 = time1.getValidPeriod();
        TimeRange p2 = time2.getValidPeriod();

        if (p1 == p2) {
            return 0;
        } else if (p1 == null) {
            return -1;
        } else if (p2 == null) {
            return 1;
        }

        int result = longCompare(p1.getDuration(), p2.getDuration());
        if (result != 0) {
            return result;
        }
        return longCompare(p1.getStart().getTime(), p2.getStart().getTime());
    }

    /**
     * For compatibility with Java 6 use this instead of Integer.compare.
     */
    private int integerCompare(int x, int y) {
        return (x < y) ? -1 : ((x == y) ? 0 : 1);
    }

    /**
     * For compatibility with Java 6 use this instead of Long.compare.
     */
    private int longCompare(long x, long y) {
        return (x < y) ? -1 : ((x == y) ? 0 : 1);
    }


}
