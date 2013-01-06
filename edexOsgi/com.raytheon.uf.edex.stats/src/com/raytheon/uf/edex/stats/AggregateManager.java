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
package com.raytheon.uf.edex.stats;

import java.lang.reflect.Method;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.StatsRecord;
import com.raytheon.uf.common.stats.xml.StatisticsAggregate;
import com.raytheon.uf.common.stats.xml.StatisticsEvent;
import com.raytheon.uf.common.stats.xml.StatisticsGroup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.stats.dao.AggregateRecordDao;
import com.raytheon.uf.edex.stats.dao.StatsDao;
import com.raytheon.uf.edex.stats.util.ConfigLoader;

/**
 * Aggregates stat records based on the statsConfig files and stores them after
 * a configured period.
 * 
 * *
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez    Stored the aggregate buckets in the db.
 * Nov 07, 2012   1317     mpduff      Updated Configuration Files.
 * Nov 28, 2012   1350     rjpeter     Simplied aggregation and added aggregation with current db aggregate records.
 * </pre>
 * 
 * @author jsanchez
 * 
 */
public class AggregateManager {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AggregateManager.class);

    /** In minutes */
    private int bucketInterval;

    /** default value */
    private static final int defaultBucketInterval = 5;

    /** default value */
    private static final int defaultScanInterval = 15;

    public AggregateManager(String bucketInterval) {
        validateIntervals(bucketInterval);
    }

    /**
     * Aggregates the grouped events and then aggregates them with any matching
     * range in DB and stores the updated aggregate.
     * 
     * @param dao
     * @param statsEvent
     * @param timeRange
     * @param groupedEvents
     */
    private void aggregate(AggregateRecordDao dao, StatisticsEvent statsEvent,
            TimeRange timeRange, Multimap<String, Event> groupedEvents) {
        Calendar start = TimeUtil.newCalendar(TimeZone.getTimeZone("GMT"));
        start.setTime(timeRange.getStart());

        Calendar end = TimeUtil.newCalendar(TimeZone.getTimeZone("GMT"));
        end.setTime(timeRange.getEnd());

        // perform aggregate functions on the grouped data
        for (String groupKey : groupedEvents.keySet()) {
            Collection<Event> groupData = groupedEvents.get(groupKey);
            Iterator<Method> aggrMethodIter = statsEvent.getAggregateMethods()
                    .iterator();
            Iterator<StatisticsAggregate> statAggrIter = statsEvent
                    .getAggregateList().iterator();
            int count = groupData.size();

            while (aggrMethodIter.hasNext() && statAggrIter.hasNext()) {
                String field = statAggrIter.next().getField();
                Method m = aggrMethodIter.next();

                try {
                    double max = -Double.MAX_VALUE;
                    double min = Double.MAX_VALUE;
                    double sum = 0;

                    for (Event event : groupData) {
                        Number number = (Number) m.invoke(event, new Object[0]);
                        double value = number.doubleValue();
                        sum += value;
                        if (value > max) {
                            max = value;
                        }

                        if (value < min) {
                            min = value;
                        }
                    }

                    AggregateRecord record = new AggregateRecord(
                            statsEvent.getType(), start, end, groupKey, field);
                    record.setSum(sum);
                    record.setMin(min);
                    record.setMax(max);
                    record.setCount(count);
                    dao.mergeRecord(record);
                } catch (Exception e) {
                    statusHandler.error("Unable to aggregate '" + field + "'",
                            e);
                }
            }
        }
    }

    /**
     * Creates a time range from a date and the bucket interval. The time range
     * start time that will be the date rounded to the next bucket interval. The
     * time range end time will be the start time plus the bucket interval.
     * 
     * @param date
     * @return
     */
    private TimeRange createTimeRange(Calendar date) {
        Calendar start = getBucketStartTime(date);
        Calendar end = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        end.setTimeInMillis(start.getTimeInMillis());
        end.add(Calendar.MINUTE, bucketInterval);

        return new TimeRange(start, end);
    }

    /**
     * Calculates the start time that will be the date rounded to the next
     * bucket interval
     * 
     * @param date
     * @return
     */
    private Calendar getBucketStartTime(Calendar date) {
        int currentMinutes = date.get(Calendar.MINUTE);

        int incrementsWithinHour = bucketInterval;
        // checks if period is larger than 60 minutes
        if (bucketInterval > 60) {
            incrementsWithinHour = bucketInterval % 60;
        }

        int mod = currentMinutes % incrementsWithinHour;

        Calendar start = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        start.setTimeInMillis(date.getTimeInMillis());
        start.add(Calendar.MINUTE, -mod);
        start.set(Calendar.SECOND, 0);
        start.set(Calendar.MILLISECOND, 0);

        return start;
    }

    /**
     * Scans the stats table to be stored in buckets and aggregate if necessary.
     */
    public void scan() throws Exception {
        long t0 = System.currentTimeMillis();
        ConfigLoader configLoader = ConfigLoader.getInstance();
        StatsDao statsRecordDao = new StatsDao();
        AggregateRecordDao aggrRecordDao = new AggregateRecordDao();

        Map<String, StatisticsEvent> statsMap = configLoader.getTypeView();

        // latest time to pull
        Calendar timeToProcess = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        int count = 0;

        // process the events by type
        for (Map.Entry<String, StatisticsEvent> entry : statsMap.entrySet()) {
            String type = entry.getKey();
            StatisticsEvent event = entry.getValue();
            List<StatsRecord> records = null;

            do {
                // retrieve stats in blocks of 1000
                records = statsRecordDao.retrieveRecords(timeToProcess, type,
                        2000);

                if (!CollectionUtil.isNullOrEmpty(records)) {
                    // sort events into time buckets
                    Map<TimeRange, Multimap<String, Event>> timeMap = sort(
                            event, records);

                    for (Map.Entry<TimeRange, Multimap<String, Event>> timeMapEntry : timeMap
                            .entrySet()) {
                        aggregate(aggrRecordDao, event, timeMapEntry.getKey(),
                                timeMapEntry.getValue());
                    }

                    try {
                        statsRecordDao.deleteAll(records);
                    } catch (Exception e) {
                        statusHandler.error("Error deleting stat records", e);
                    }

                    count += records.size();
                }
            } while (!CollectionUtil.isNullOrEmpty(records));
        }

        long t1 = System.currentTimeMillis();
        statusHandler.info("Aggregated " + count + " stat events in "
                + (t1 - t0) + " ms");
    }

    /**
     * Sorts the records into time buckets and groups by the underlying Event.
     * 
     * @param records
     * @return
     */
    private Map<TimeRange, Multimap<String, Event>> sort(
            StatisticsEvent statEvent, List<StatsRecord> records) {
        Map<TimeRange, Multimap<String, Event>> rval = new HashMap<TimeRange, Multimap<String, Event>>();
        TimeRange timeRange = null;
        Multimap<String, Event> eventsByGroup = null;
        final Object[] EMPTY_OBJ_ARR = new Object[0];
        StringBuilder group = new StringBuilder();

        for (StatsRecord record : records) {
            if ((timeRange == null)
                    || !timeRange.contains(record.getDate().getTime())) {
                // Create bucket based on stats record date
                timeRange = createTimeRange(record.getDate());
                eventsByGroup = rval.get(timeRange);
                if (eventsByGroup == null) {
                    eventsByGroup = ArrayListMultimap.create();
                    rval.put(timeRange, eventsByGroup);
                }
            }

            try {
                // get underlying event
                Event event = SerializationUtil.transformFromThrift(
                        Event.class, record.getEvent());

                // determine group
                boolean addDelim = false;
                Iterator<Method> gMethodIter = statEvent.getGroupByMethods()
                        .iterator();
                Iterator<StatisticsGroup> gFieldNameIter = statEvent
                        .getGroupList().iterator();
                group.setLength(0);

                while (gMethodIter.hasNext() && gFieldNameIter.hasNext()) {
                    Method m = gMethodIter.next();
                    String field = gFieldNameIter.next().getName();
                    String gVal = String
                            .valueOf(m.invoke(event, EMPTY_OBJ_ARR));

                    if (addDelim) {
                        group.append('-');
                    } else {
                        addDelim = true;
                    }

                    group.append(field).append(':').append(gVal);
                }

                eventsByGroup.put(group.toString(), event);
            } catch (Exception e) {
                statusHandler
                        .error("Error processing event. Aggregation may be inaccurate. ",
                                e);
            }
        }

        return rval;
    }

    /**
     * Tests if the bucket interval is a valid value. If value is invalid then
     * value will be set to default value.
     * 
     * @param bucketInt
     * @return
     */
    private void validateIntervals(String bucketInt) {
        try {
            bucketInterval = Integer.parseInt(bucketInt);
        } catch (NumberFormatException e) {
            bucketInterval = defaultBucketInterval;
            statusHandler.info("'" + bucketInt
                    + "' is not a valid bucket interval value. Setting to '"
                    + defaultBucketInterval + "'");
        }

        int incrementsWithinHour = bucketInterval;
        // checks if period is larger than 60 minutes
        if (bucketInterval > 60) {
            incrementsWithinHour = bucketInterval % 60;
        }
        if (60 % incrementsWithinHour != 0) {
            bucketInterval = defaultBucketInterval;
            statusHandler
                    .info("The bucket interval must go into an hour evenly. Setting bucket interval to '"
                            + bucketInterval + "'");
        }
    }
}
