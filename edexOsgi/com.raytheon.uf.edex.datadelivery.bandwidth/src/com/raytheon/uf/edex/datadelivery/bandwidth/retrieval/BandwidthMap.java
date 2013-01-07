package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import java.io.File;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Class generates a profile of available bandwidth for edex to manage using
 * {@link BandwidthRoute} entries. Each BandwidthRoute describes the network
 * resource availability for a particular "route", i.e. network connection.
 * {@link RetrievalManager} creates a {@link RetrievalPlan} using the
 * BandwidthMap to schedule tasks that require network resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 726        jspinks     Initial release.
 * Oct 10, 2012 0726       djohnson    Overload the load method to support files.
 * Oct 23, 2012 1286       djohnson    Add ability to save changes to the map.
 * 
 * </pre>
 * 
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class BandwidthMap implements ISerializableObject {

    private static final int DEFAULT_BUCKET_SIZE = 3;

    private static final int DEFAULT_PLAN_DAYS = 2;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BandwidthMap.class);

    /**
     * Load an XML serialized BandwidthMap from the specified file.
     * 
     * @param file
     *            the file
     * @return the map
     */
    public static BandwidthMap load(File file) {
        try {
            BandwidthMap map = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    BandwidthMap.class, file);
            map.initialize();
            map.setFile(file);
            return map;
        } catch (SerializationException e) {
            throw new IllegalArgumentException(e);
        }
    }

    // Map to track days of the year (int) to bandwidthBucket id (long)
    // to available bandwidth (int).
    private final SortedMap<Integer, NavigableMap<Long, Integer>> bandwidth = new TreeMap<Integer, NavigableMap<Long, Integer>>();

    private final AtomicBoolean initialized = new AtomicBoolean(false);

    @XmlElement(name = "route")
    private List<BandwidthRoute> routes;

    private File file;

    // A matcher for a seed and step value pair separated by a colon ':'.
    private final Pattern seedStepPattern = Pattern
            .compile("(\\d{1,2})(?::)(\\d{1,2})");

    // A matcher for a series of comma separated numbers
    private final Pattern seriesPattern = Pattern.compile("(?:(\\d{1,2}),?)");

    /**
     * The the available bandwidth (in kilobytes/second) for the specified time.
     * 
     * @param routeName
     *            The route name to use.
     * 
     * @param time
     *            The time to use.
     * 
     * @return The available bandwidth for the time specified.
     */
    public int getBandwidth(Network routeName, Calendar time) {

        // TODO: Need to figure something out for dates outside
        // the current year (which is what the map loads by default)
        // some type of checking of time year against year of last
        // day or something...

        // If this instance has not been initialized
        if (this.initialized.compareAndSet(false, true)) {
            initialize();
        }

        int bw = -1;
        boolean found = false;

        for (BandwidthRoute route : routes) {
            if (route.getNetwork().equals(routeName)) {
                found = true;
                bw = route.getDefaultBandwidth();
                NavigableMap<Long, Integer> limitedBandwidth = bandwidth
                        .get(time.get(Calendar.DAY_OF_YEAR));
                if (limitedBandwidth != null) {
                    Entry<Long, Integer> t = limitedBandwidth.floorEntry(time
                            .getTimeInMillis());
                    if (t != null) {
                        bw = t.getValue().intValue();
                    }
                }
            }
        }

        if (!found) {
            statusHandler
                    .error("BandwidthMap does not contain bandwidth configurations for route ["
                            + routeName + "]");
        }
        return bw;
    }

    private BitSet getDaysBitSet(Calendar calendar) {
        // Since BitSet is zero based and Calendar.DAY_OF_YEAR is 1 based,
        // create a BitSet that has an extra bit so that the indexes of the
        // two types line up.
        return new BitSet(calendar.getActualMaximum(Calendar.DAY_OF_YEAR) + 1);
    }

    /**
     * Return the route with the specified network.
     * 
     * @param network
     *            The network to retrieve a BandwidthRoute for.
     * 
     * @return The BandwidthRoute with for the specified network.
     */
    public BandwidthRoute getRoute(Network network) {
        BandwidthRoute route = null;
        for (BandwidthRoute r : routes) {
            if (r.getNetwork().equals(network)) {
                route = r;
                break;
            }
        }
        return route;
    }

    /**
     * Convenience method for parsing a seed/step pair.
     * 
     * <pre>
     * Example: if a seed/step of 1:3 is provided with a maximum
     * value of 10.  A List containing 1,4,7,10 will be returned.
     * 
     * Example: if a seed/step of 2:2 is provided with a maximum
     * value of 10.  A List containing 2,4,6,8,10 will be returned.
     * 
     * </pre>
     * 
     * @param m
     *            The Matcher that has matched on the value to be evaluated.
     * 
     * @param maximumValue
     *            The maximum value in the seed/step series.
     * 
     * @return A List of Integers that contain all the values between the seed
     *         value and the maximum value, inclusive, with a stepping obtained
     *         from the Matcher.
     */
    private List<Integer> getSeedStep(Matcher m, int maximumValue) {
        List<Integer> series = new ArrayList<Integer>();
        if (m.end(1) > -1) {
            int seed = Integer.parseInt(m.group(1));
            int step = Integer.parseInt(m.group(2));
            for (int value = seed; value <= maximumValue; value += step) {
                series.add(value);
            }
        } else {
            // A single month value, mark the days in the month specified.
            series.add(Integer.parseInt(m.group(0)));
        }
        return series;
    }

    /**
     * Convenience method for parsing a comma separated series of integers.
     * 
     * @param matcher
     *            The Matcher that has matched on the value to be evaluated.
     * 
     * @return A List of Integers contained in the series, sorted in ascending
     *         order.
     */
    private List<Integer> getSeries(Matcher matcher) {
        List<Integer> series = new ArrayList<Integer>();
        series.add(Integer.parseInt(matcher.group(1)));
        while (matcher.find(matcher.end())) {
            series.add(Integer.parseInt(matcher.group(1)));
        }
        Collections.sort(series);
        return series;
    }

    private void initialize() {

        List<BitSet> criteria = new ArrayList<BitSet>();

        if (this.routes != null) {
            for (BandwidthRoute route : routes) {
                // If there are allocations set, modify the bandwidth buckets
                // according to the modifications in the map
                List<AvailableBandwidth> modifications = route
                        .getModifications();

                if (modifications != null) {

                    // While planDays and bucketSizeMinutes can be defaulted,
                    // defaultBandwidth has much
                    // more of an impact so it MUST be specified.
                    int temp = route.getDefaultBandwidth();
                    if (temp == 0) {
                        throw new IllegalStateException(
                                "BandwidthMap does not contain the required attribute defaultBandwidth.");
                    }

                    temp = route.getPlanDays();
                    if (temp == 0) {
                        statusHandler
                                .warn("BandwidthMap["
                                        + route.getNetwork()
                                        + "] does not contain the required attribute planDays."
                                        + "The default value of ["
                                        + DEFAULT_PLAN_DAYS + "] will be used.");
                        route.setPlanDays(DEFAULT_PLAN_DAYS);
                    }

                    temp = route.getBucketSizeMinutes();
                    if (temp == 0) {
                        statusHandler
                                .warn("BandwidthMap["
                                        + route.getNetwork()
                                        + "] does not contain the required attribute bucketSizeMinutes."
                                        + "The default value of ["
                                        + DEFAULT_BUCKET_SIZE
                                        + "] will be used.");
                        route.setBucketSizeMinutes(DEFAULT_BUCKET_SIZE);
                    }

                    int index = 0;
                    for (AvailableBandwidth modification : modifications) {
                        index++;
                        criteria.clear();
                        int duration = 0;
                        int minuteOfDay = 0;

                        if (modification.getMinuteOfDay() == null) {
                            statusHandler
                                    .warn("BandwidthMap["
                                            + route.getNetwork()
                                            + "] Modification ["
                                            + index
                                            + "/"
                                            + modifications.size()
                                            + "] does not contain the required minuteOfDay attribute."
                                            + "This modification will be ignored.");
                            continue;
                        } else {
                            minuteOfDay = Integer.parseInt(modification
                                    .getMinuteOfDay());
                        }

                        if (modification.getDuration() == null) {
                            statusHandler
                                    .warn("BandwidthMap["
                                            + route.getNetwork()
                                            + "] Modification ["
                                            + index
                                            + "/"
                                            + modifications.size()
                                            + "] does not contain the required duration attribute."
                                            + "This modification will be ignored.");
                            continue;
                        } else {
                            duration = Integer.parseInt(modification
                                    .getDuration());
                        }

                        if (modification.getMonth() != null) {
                            BitSet month = parseMonth(modification.getMonth());
                            // If the BitSet is empty, the value provided was
                            // invalid, move
                            // to the next allocation.
                            if (month.isEmpty()) {
                                statusHandler
                                        .warn("BandwidthMap["
                                                + route.getNetwork()
                                                + "] contains an invalid value for attribute month, "
                                                + "modification ["
                                                + index
                                                + "/"
                                                + modifications.size()
                                                + "]. This modification will be ignored.");
                                continue;
                            }
                            criteria.add(month);
                        }

                        if (modification.getDayOfMonth() != null) {
                            BitSet dayOfMonth = parseDayOfMonth(modification
                                    .getDayOfMonth());
                            // If the BitSet is empty, the value provided was
                            // invalid, move
                            // to the next allocation.
                            if (dayOfMonth.isEmpty()) {
                                statusHandler
                                        .warn("BandwidthMap["
                                                + route.getNetwork()
                                                + "] contains an invalid value for attribute dayOfMonth, "
                                                + "modification ["
                                                + index
                                                + "/"
                                                + modifications.size()
                                                + "]. This modification will be ignored.");
                                continue;
                            }
                            criteria.add(dayOfMonth);
                        }

                        if (modification.getWeekOfMonth() != null) {
                            BitSet weekOfMonth = parseWeekOfMonth(modification
                                    .getWeekOfMonth());
                            // If the BitSet is empty, the value provided was
                            // invalid, move
                            // to the next allocation.
                            if (weekOfMonth.isEmpty()) {
                                statusHandler
                                        .warn("BandwidthMap["
                                                + route.getNetwork()
                                                + "] contains an invalid value for attribute weekOfMonth, "
                                                + "modification ["
                                                + index
                                                + "/"
                                                + modifications.size()
                                                + "]. This modification will be ignored.");
                                continue;
                            }
                            criteria.add(weekOfMonth);
                        }

                        if (modification.getWeekOfYear() != null) {
                            BitSet weekOfYear = parseWeekOfYear(modification
                                    .getWeekOfYear());
                            // If the BitSet is empty, the value provided was
                            // invalid, move
                            // to the next allocation.
                            if (weekOfYear.isEmpty()) {
                                statusHandler
                                        .warn("BandwidthMap["
                                                + route.getNetwork()
                                                + "] contains an invalid value for attribute weekOfYear, "
                                                + "modification ["
                                                + index
                                                + "/"
                                                + modifications.size()
                                                + "]. This modification will be ignored.");
                                continue;
                            }
                            criteria.add(weekOfYear);
                        }

                        if (modification.getDayOfWeek() != null) {
                            BitSet daysOfWeek = parseDayOfWeek(modification
                                    .getDayOfWeek());
                            // If the BitSet is empty, the value provided was
                            // invalid, move
                            // to the next allocation.
                            if (daysOfWeek.isEmpty()) {
                                statusHandler
                                        .warn("BandwidthMap["
                                                + route.getNetwork()
                                                + "] contains an invalid value for attribute dayOfWeek, "
                                                + "modification ["
                                                + index
                                                + "/"
                                                + modifications.size()
                                                + "]. This modification will be ignored.");
                                continue;
                            }
                            criteria.add(daysOfWeek);
                        }

                        if (criteria.size() > 0) {
                            // Combine all the criteria together to get the
                            // final
                            // bitmapping...
                            Calendar now = TimeUtil.newCalendar();
                            BitSet composite = getDaysBitSet(now);
                            composite.set(0, composite.size());

                            for (BitSet c : criteria) {
                                composite.and(c);
                            }

                            now.set(Calendar.SECOND, 0);
                            now.set(Calendar.MILLISECOND, 0);

                            // For each day with criteria, build the
                            // "bucketSize" indexes that
                            // will mark each modification to the default
                            // bandwidth.
                            for (int dayOfYear = composite.nextSetBit(0); dayOfYear >= 0; dayOfYear = composite
                                    .nextSetBit(dayOfYear + 1)) {

                                now.set(Calendar.DAY_OF_YEAR, dayOfYear);
                                now.set(Calendar.HOUR_OF_DAY, minuteOfDay / 60);
                                now.set(Calendar.MINUTE, minuteOfDay % 60);

                                NavigableMap<Long, Integer> bucket = bandwidth
                                        .get(dayOfYear);
                                if (bucket == null) {
                                    bucket = new TreeMap<Long, Integer>();
                                    bandwidth.put(dayOfYear, bucket);
                                }

                                for (int i = 0; i < duration; i += route
                                        .getBucketSizeMinutes()) {
                                    bucket.put(now.getTimeInMillis(),
                                            modification.getBandwidth());
                                    now.add(Calendar.MINUTE,
                                            route.getBucketSizeMinutes());
                                }
                                // Mark the end of a modification with a bucket
                                // with default bandwidth.
                                // Since available bandwidth is determined by
                                // looking up a start time,
                                // this approach will allow lookups with time
                                // past the duration time of
                                // this modification to receive an answer of
                                // default bandwidth, without
                                // having to do any calculations on the
                                // difference between time and
                                // key values.
                                bucket.put(now.getTimeInMillis(),
                                        route.getDefaultBandwidth());
                            }
                        } else {
                            statusHandler
                                    .warn("BandwidthMap["
                                            + route.getNetwork()
                                            + "] modification ["
                                            + index
                                            + "/"
                                            + modifications.size()
                                            + "] did not contain valid"
                                            + " time specifications. This modification will be ignored.");
                        }

                        // Now we have generated the keys for the starting and
                        // ending BandwidthBuckets. Get the portion of map that
                        // is built, and adjust the available bandwidth.

                    }
                }
            }
        }
    }

    /**
     * Parse the value of dayOfMonth, which can be a single value or a comma
     * separated series of days of the month.
     * 
     * @param dayOfMonth
     *            A single day value, or a comma separated series of day values.
     * 
     * @return A BitSet with bits corresponding to the days of the year set that
     *         match with the days of the month value specified.
     */
    private BitSet parseDayOfMonth(String dayOfMonth) {
        Calendar now = TimeUtil.newCalendar();
        BitSet daysOfYear = getDaysBitSet(now);

        // Check series matcher..
        Matcher s = seriesPattern.matcher(dayOfMonth);
        List<Integer> daysOfMonth = new ArrayList<Integer>();
        if (s.lookingAt()) {
            daysOfMonth.add(Integer.parseInt(s.group(1)));
            while (s.find(s.end())) {
                daysOfMonth.add(Integer.parseInt(s.group(1)));
            }
        }

        if (daysOfMonth.size() > 0) {
            for (int month = now.getActualMinimum(Calendar.MONTH); month <= now
                    .getActualMaximum(Calendar.MONTH); month++) {
                now.set(Calendar.MONTH, month);
                // If the day of the month selected is in the month
                // add that day to the daysOfYear set.
                for (int day : daysOfMonth) {
                    if (day <= now.getActualMaximum(Calendar.DAY_OF_MONTH)) {
                        now.set(Calendar.DAY_OF_MONTH, day);
                        daysOfYear.set(now.get(Calendar.DAY_OF_YEAR));
                    }
                }
            }
        }

        return daysOfYear;
    }

    /**
     * Parse the value of dayOfWeek, which can be a single value or a comma
     * separated series of days of the week.
     * 
     * @param dayOfWeek
     *            A single day value, or a comma separated series of day values.
     * 
     * @return A BitSet with bits corresponding to the days of the year set that
     *         match with the days of the week value specified.
     */
    private BitSet parseDayOfWeek(String dayOfWeek) {

        Calendar now = TimeUtil.newCalendar();
        BitSet daysOfYear = getDaysBitSet(now);

        Matcher s = seriesPattern.matcher(dayOfWeek);
        List<Integer> daysOfWeek = new ArrayList<Integer>();
        if (s.lookingAt()) {
            daysOfWeek = getSeries(s);
            Set<Integer> days = new HashSet<Integer>();
            days.addAll(daysOfWeek);
            Set<Integer> exclude = new HashSet<Integer>();

            // Iterate over the weeks of the year and set the days of each week.
            // The first and last weeks need a little extra attention

            // Exclude the days of the week that are before the start of the
            // year.
            // i.e. if the first day of the year is a Wednesday, mask of Sunday
            // thru Tuesday.
            now.set(Calendar.DAY_OF_YEAR, 1);
            int first = now.get(Calendar.DAY_OF_WEEK);
            for (int day = 1; day <= first; day++) {
                exclude.add(day);
            }

            Set<Integer> firstDays = new HashSet<Integer>(days);
            firstDays.removeAll(exclude);
            for (int d : firstDays) {
                now.set(Calendar.DAY_OF_WEEK, d);
                daysOfYear.set(now.get(Calendar.DAY_OF_YEAR));
            }

            // Do all the weeks in between the first and last...
            for (int week = 2; week < now
                    .getActualMaximum(Calendar.WEEK_OF_YEAR); week++) {
                now.set(Calendar.WEEK_OF_YEAR, week);
                for (int day : days) {
                    now.set(Calendar.DAY_OF_WEEK, day);
                    daysOfYear.set(now.get(Calendar.DAY_OF_YEAR));
                }
            }

            // Exclude the days of the week that are past the end of the year.
            // i.e. if the last day of the year is a Thursday, mask of Friday
            // thru Sunday.
            exclude.clear();
            now.set(Calendar.DAY_OF_YEAR,
                    now.getActualMaximum(Calendar.DAY_OF_YEAR));
            int day = now.get(Calendar.DAY_OF_WEEK);
            for (day++; day <= 7; day++) {
                exclude.add(day);
            }
            Set<Integer> lastDays = new HashSet<Integer>(days);
            lastDays.removeAll(exclude);
            for (int l : lastDays) {
                now.set(Calendar.DAY_OF_WEEK, l);
                daysOfYear.set(now.get(Calendar.DAY_OF_YEAR));
            }
        }

        return daysOfYear;
    }

    /**
     * Parse the value of month, which can be a single value or a comma
     * separated series of months.
     * 
     * @param month
     *            A single month value, or a comma separated series of months.
     * 
     * @return A BitSet with bits corresponding to the days of the year set that
     *         match with the days contained in the month value specified.
     */
    private BitSet parseMonth(String month) {
        Calendar now = TimeUtil.newCalendar();
        BitSet daysOfYear = getDaysBitSet(now);

        Matcher m = seriesPattern.matcher(month);
        if (m.lookingAt()) {
            List<Integer> months = getSeries(m);
            for (int i_month : months) {
                now.set(Calendar.MONTH, i_month);
                now.set(Calendar.DAY_OF_MONTH, 1);
                // Get the day of the year for the first day in the month.
                int day = now.get(Calendar.DAY_OF_YEAR);
                // Set the day to the last day of the current month.
                now.set(Calendar.DAY_OF_MONTH,
                        now.getActualMaximum(Calendar.DAY_OF_MONTH));
                // Set the bits for all the days in the currently selected
                // month.
                daysOfYear.set(day, now.get(Calendar.DAY_OF_YEAR) + 1);
            }
        }
        return daysOfYear;
    }

    private BitSet parseWeekOfMonth(String weekOfMonth) {

        Calendar now = TimeUtil.newCalendar();
        BitSet daysOfYear = getDaysBitSet(now);

        Matcher s = seriesPattern.matcher(weekOfMonth);
        List<Integer> weeksOfMonth = new ArrayList<Integer>();
        if (s.lookingAt()) {
            weeksOfMonth = getSeries(s);

            Set<Integer> days = new HashSet<Integer>();
            days.addAll(weeksOfMonth);
            Set<Integer> exclude = new HashSet<Integer>();

            // Iterate over the weeks of the year and set the days of each week.
            // The first and last weeks need a little extra attention

            // Exclude the days of the week that are before the start of the
            // year.
            // i.e. if the first day of the year is a Wednesday, mask of Sunday
            // thru Tuesday.
            now.set(Calendar.DAY_OF_YEAR, 1);
            int first = now.get(Calendar.DAY_OF_WEEK);
            for (int day = 1; day <= first; day++) {
                exclude.add(day);
            }

            Set<Integer> firstDays = new HashSet<Integer>(days);
            firstDays.removeAll(exclude);
            for (int d : firstDays) {
                now.set(Calendar.DAY_OF_WEEK, d);
                daysOfYear.set(now.get(Calendar.DAY_OF_YEAR));
            }

            // Do all the weeks in between the first and last...
            for (int week = 2; week < now
                    .getActualMaximum(Calendar.WEEK_OF_YEAR); week++) {
                now.set(Calendar.WEEK_OF_YEAR, week);
                for (int day : days) {
                    now.set(Calendar.DAY_OF_WEEK, day);
                    daysOfYear.set(now.get(Calendar.DAY_OF_YEAR));
                }
            }

            // Exclude the days of the week that are past the end of the year.
            // i.e. if the last day of the year is a Thursday, mask of Friday
            // thru Sunday.
            exclude.clear();
            now.set(Calendar.DAY_OF_YEAR,
                    now.getActualMaximum(Calendar.DAY_OF_YEAR));
            int day = now.get(Calendar.DAY_OF_WEEK);
            for (day++; day <= 7; day++) {
                exclude.add(day);
            }
            Set<Integer> lastDays = new HashSet<Integer>(days);
            lastDays.removeAll(exclude);
            for (int l : lastDays) {
                now.set(Calendar.DAY_OF_WEEK, l);
                daysOfYear.set(now.get(Calendar.DAY_OF_YEAR));
            }
        }

        return daysOfYear;
    }

    /**
     * Parse the value of weekOfYear, which can be one of three possible values.
     * A single value, a comma separated series or a seed/step value pair.
     * 
     * @param weekOfYear
     *            The weekOfYear value to parse.
     * 
     * @return A BitSet with bits corresponding to the days of the year set that
     *         match with the days contained in the weeks of the year value
     *         specified.
     */
    private BitSet parseWeekOfYear(String weekOfYear) {
        Calendar now = TimeUtil.newCalendar();
        BitSet daysOfYear = getDaysBitSet(now);

        // Check series matcher..
        Matcher s = seriesPattern.matcher(weekOfYear);
        List<Integer> weeksOfYear = new ArrayList<Integer>();
        if (s.lookingAt()) {
            weeksOfYear = getSeries(s);
        } else {
            // Check the seed/step matcher
            Matcher m = seedStepPattern.matcher(weekOfYear);
            if (m.matches()) {
                weeksOfYear = getSeedStep(m,
                        now.getActualMaximum(Calendar.WEEK_OF_YEAR));
            }
        }

        if (weeksOfYear.size() > 0) {
            // Special processing for the first and last week of the year.

            // If the first week of the year was selected..
            if (weeksOfYear.get(0) == 1) {
                now.set(Calendar.DAY_OF_YEAR, 1);
                int day = now.get(Calendar.DAY_OF_WEEK);
                for (int j = 1, i = day; i < 7; i++, j++) {
                    daysOfYear.set(j);
                }
            }
            int lastWeek = now.getActualMaximum(Calendar.WEEK_OF_YEAR);
            // If the last value equals the last week of the year.
            if (weeksOfYear.get(weeksOfYear.size() - 1) == lastWeek) {
                int lastDay = now.getActualMaximum(Calendar.DAY_OF_YEAR);
                now.set(Calendar.DAY_OF_YEAR, lastDay);
                int day = now.get(Calendar.DAY_OF_WEEK);
                // Loop until we reach the last day of the year.
                for (int i = 1; i < day; i++) {
                    now.set(Calendar.DAY_OF_WEEK, i);
                    daysOfYear.set(now.get(Calendar.DAY_OF_YEAR));
                }
            }

            for (int i = 1; i < weeksOfYear.size() - 1; i++) {
                now.set(Calendar.WEEK_OF_YEAR, i);
                now.set(Calendar.DAY_OF_WEEK, 1);
                // The the day of year of the first day of the week
                int day = now.get(Calendar.DAY_OF_YEAR);
                // Set the day to the last day of the week
                now.set(Calendar.DAY_OF_WEEK, 7);
                // Set all the bits between the first and last day of the week
                daysOfYear.set(day, now.get(Calendar.DAY_OF_YEAR) + 1);
            }
        }

        return daysOfYear;
    }

    /**
     * Set the route attribute.
     * 
     * @param routes
     *            the routes to set
     */
    public void setRoutes(List<BandwidthRoute> routes) {
        this.routes = routes;
    }

    /**
     * Persists the changes to this instance to its configuration file.
     * 
     * @throws SerializationException
     *             on error serializing changes
     */
    public void save() throws SerializationException {
        save(file);
    }

    /**
     * Persists the changes to this instance to its configuration file.
     * 
     * @param file
     *            the file to save changes to
     * @throws SerializationException
     *             on error serializing changes
     */
    public void save(File file) throws SerializationException {
        SerializationUtil.jaxbMarshalToXmlFile(this, file.getAbsolutePath());
    }

    /**
     * @param file
     */
    private void setFile(File file) {
        this.file = file;
    }
}
