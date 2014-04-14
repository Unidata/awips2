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
package com.raytheon.uf.common.inventory;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.time.DataTime;

/**
 * The primary function of this class is two find the intersection between two
 * collections of TimeAndSpace objects. This will compare all the TimeAndSpace
 * objects in each collection and find "matches". The simplest matches are when
 * both time and space objects are equal. More complex matches can be created
 * when times are equal but one is time agnostic or spaces are equal and one is
 * space agnostic. Finally if for one of the Collections contains a time and
 * space agnostic TimeAndSpace than it will match everything.
 * 
 * Time matching is a little more complex than space because you can configure
 * ignoreRange and validMatch so that times can be considered matching even if
 * they aren't identical, as long as they are referencing a similar time, for
 * more information see the setters for those flags.
 * 
 * When determining matches this will choose the most specific match. For
 * example if a TimeAndSpace object can match an object that matches space but
 * only matches validTime or it can match an object that matches space but is
 * time agnostic, then it will choose the valid time match since that is more
 * specific than the agnostic. This order is defined by the natural order of the
 * MatchType enum.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 08, 2012           bsteffen    Initial creation
 * Apr 11, 2014  2947     bsteffen    Switch spatial matching to use
 *                                    IGridGeometryProvider
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class TimeAndSpaceMatcher {

    private boolean ignoreRange = false;

    private boolean matchValid = false;

    /**
     * When ignore range is true then times with different ranges are considered
     * matching if they are the same in all other ways, when false times are
     * considered nonmatching if the range is not identical.
     * 
     * @param ignoreRange
     */
    public void setIgnoreRange(boolean ignoreRange) {
        this.ignoreRange = ignoreRange;
    }

    /**
     * When match valid is true then dataTimes are compared based off valid
     * time, when it is false the refTime and forecast time are considered
     * sperately and both must match to create a match. Even when matchValid is
     * true, if there is a time where forecast and refTime amtch it will be
     * prefered to a time where only validTime matches
     * 
     * @param matchValid
     */
    public void setMatchValid(boolean matchValid) {
        this.matchValid = matchValid;
    }

    /**
     * Perform the match operation. The result is returned as a map, where the
     * key is the intersected TimeAndSpace and the values are a MatchResult that
     * contains the SpaceAndTime needed from each collection to represent that
     * time.
     * 
     * @param times1
     * @param times2
     * @return
     */
    public Map<TimeAndSpace, MatchResult> match(
            Collection<TimeAndSpace> times1, Collection<TimeAndSpace> times2) {
        Map<TimeAndSpace, MatchResult> result = new HashMap<TimeAndSpace, MatchResult>();
        for (TimeAndSpace t1 : times1) {
            for (TimeAndSpace t2 : times2) {
                MatchResult res = createMatchResult(t1, t2);
                if (res != null) {
                    MatchResult prev = result.get(res.getMerge());
                    if (prev == null || prev.compareTo(res) > 0) {
                        result.put(res.getMerge(), res);
                    }
                }
            }
        }
        return result;
    }

    /**
     * Given two TimeAndSpace objects attempts to determine what they have in
     * common, if anything and return a MatchResult or null if there is no
     * match.
     * 
     * @param t1
     * @param t2
     * @return
     */
    private MatchResult createMatchResult(TimeAndSpace t1, TimeAndSpace t2) {
        TimeMatchType timeMatchType;
        DataTime time = null;

        /* Determine how well the times match */
        if (t1.isTimeAgnostic()) {
            /* When one is agnostic it will match anything. */
            time = t2.getTime();
            timeMatchType = TimeMatchType.AGNOSTIC;
        } else if (t2.isTimeAgnostic()) {
            /* again one is agnostic */
            time = t1.getTime();
            timeMatchType = TimeMatchType.AGNOSTIC;
        } else if (t1.getTime().equals(t2.getTime())) {
            /* A perfect time match is always best. */
            time = t1.getTime();
            timeMatchType = TimeMatchType.MATCH;
        } else if (ignoreRange
                && t1.getTime().getMatchRef() == t2.getTime().getMatchRef()
                && t1.getTime().getMatchFcst() == t2.getTime().getMatchFcst()) {
            /*
             * If the ignoreRanfe flag is set then it is still considered a
             * match even if the ranges are different.
             */
            time = new DataTime(t1.getTime().getRefTime(), t1.getTime()
                    .getFcstTime());
            timeMatchType = TimeMatchType.IGNORE_RANGE;
        } else if (matchValid
                && t1.getTime().getMatchValid() == t2.getTime().getMatchValid()) {
            /*
             * finally last valid allows us to mix different
             * refTime/forecastTimes as long as valid matches.
             */
            if (t1.getTime().getMatchRef() > t2.getTime().getMatchRef()) {
                time = new DataTime(t1.getTime().getRefTime(), t1.getTime()
                        .getFcstTime());
            } else {
                time = new DataTime(t2.getTime().getRefTime(), t2.getTime()
                        .getFcstTime());
            }
            timeMatchType = TimeMatchType.VALID_TIME;
        } else {
            /* no time match, means no match at all. */
            return null;
        }

        SpaceMatchType spaceMatchType = null;
        IGridGeometryProvider space = null;
        
        /* Determine how well the spaces match */
        if (t1.isSpaceAgnostic()) {
            /* When one is agnostic it will match anything. */
            space = t2.getSpace();
            spaceMatchType = SpaceMatchType.AGNOSTIC;
        } else if (t2.isSpaceAgnostic()) {
            /* again one is agnostic */
            space = t1.getSpace();
            spaceMatchType = SpaceMatchType.AGNOSTIC;
        } else if (t1.getSpace().equals(t2.getSpace())) {
            /* A perfect space match is always best. */
            space = t1.getSpace();
            spaceMatchType = SpaceMatchType.MATCH;
        } else {
            if(t1.getSpace() instanceof IGridGeometryProviderComparable){
                space = ((IGridGeometryProviderComparable) t1.getSpace())
                        .compare(t2.getSpace());
            }
            if (space == null
                    && (t2.getSpace() instanceof IGridGeometryProviderComparable)) {
                space = ((IGridGeometryProviderComparable) t2.getSpace())
                        .compare(t1.getSpace());
            }
            if(space != null){
                spaceMatchType = SpaceMatchType.COMPARABLE;
            }else{
                return null;
            }
        }
        return new MatchResult(t1, t2, new TimeAndSpace(time, space),
                timeMatchType, spaceMatchType);
    }

    /**
     * 
     * Class to contain the result of a match operation for 2 matching
     * TimeAndSpace objects
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Sep 27, 2012            bsteffen     Initial creation
     * 
     * </pre>
     * 
     * @author bsteffen
     * @version 1.0
     */
    public static class MatchResult implements Comparable<MatchResult> {

        /** The TimeAndSpace from the first collection that matches */
        private final TimeAndSpace t1;

        /** The TimeAndSpace from the second collection that matches */
        private final TimeAndSpace t2;

        /** The TimeAndSpace from the first collection that matches */
        private final TimeAndSpace merge;

        /** How good of a time match is this. */
        private final TimeMatchType timeMatchType;

        /** How good of a space match is this. */
        private final SpaceMatchType spaceMatchType;

        private MatchResult(TimeAndSpace t1, TimeAndSpace t2,
                TimeAndSpace merge, TimeMatchType timeMatchType,
                SpaceMatchType spaceMatchType) {
            super();
            this.t1 = t1;
            this.t2 = t2;
            this.merge = merge;
            this.timeMatchType = timeMatchType;
            this.spaceMatchType = spaceMatchType;
        }

        public TimeAndSpace get1() {
            return t1;
        }

        public TimeAndSpace get2() {
            return t2;
        }

        public TimeAndSpace getMerge() {
            return merge;
        }

        public TimeMatchType getTimeMatchType() {
            return timeMatchType;
        }

        public SpaceMatchType getSpaceMatchType() {
            return spaceMatchType;
        }

        @Override
        public int compareTo(MatchResult o) {
            int result = timeMatchType.compareTo(o.getTimeMatchType());
            if (result == 0) {
                result = spaceMatchType.compareTo(o.getSpaceMatchType());
            }
            return result;
        }

    }

    public static enum TimeMatchType {
        /* Perfect match */
        MATCH,

        /* Reftime and forecast time match but ranges do not */
        IGNORE_RANGE,

        /* Only valid time matches, not ref/forectast time. */
        VALID_TIME,

        /* One is time or both are time agnostic so it matches everything. */
        AGNOSTIC
    }

    public static enum SpaceMatchType {
        /* Perfect match */
        MATCH,

        /*
         * One or both implements IGridGeometryProviderComparable and found a
         * suitable intersection
         */
        COMPARABLE,

        /* One is time or both are space agnostic so it matches everything. */
        AGNOSTIC
    }

    /**
     * Given the result of a match this will pull out the unique SpaceAndTime
     * objects that matched from the second Collection passed to match.
     * 
     * @param matchResults
     * @return
     */
    public static Set<TimeAndSpace> getAll2(
            Map<TimeAndSpace, MatchResult> matchResults) {
        Set<TimeAndSpace> result = new HashSet<TimeAndSpace>(
                matchResults.size());
        for (MatchResult mr : matchResults.values()) {
            result.add(mr.get2());
        }
        return result;
    }

    /**
     * Given the result of a match this will pull out the unique SpaceAndTime
     * objects that matched from the first Collection passed to match.
     * 
     * @param matchResults
     * @return
     */
    public static Set<TimeAndSpace> getAll1(
            Map<TimeAndSpace, MatchResult> matchResults) {
        Set<TimeAndSpace> result = new HashSet<TimeAndSpace>(
                matchResults.size());
        for (MatchResult mr : matchResults.values()) {
            result.add(mr.get1());
        }
        return result;
    }
}
