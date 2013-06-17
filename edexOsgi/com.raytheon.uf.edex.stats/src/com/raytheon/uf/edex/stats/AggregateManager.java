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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.xml.bind.JAXBException;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.StatisticsEvent;
import com.raytheon.uf.common.stats.StatsGrouping;
import com.raytheon.uf.common.stats.StatsGroupingColumn;
import com.raytheon.uf.common.stats.StatsRecord;
import com.raytheon.uf.common.stats.xml.StatisticsAggregate;
import com.raytheon.uf.common.stats.xml.StatisticsEventConfig;
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
 * Nov 07, 2012 1317       mpduff      Updated Configuration Files.
 * Nov 28, 2012 1350       rjpeter     Simplied aggregation and added aggregation with current db aggregate records.
 * Jan 07, 2013 1451       djohnson    Use newGmtCalendar().
 * Jan 15, 2013 1487       djohnson    Use xml for the grouping information on an {@link AggregateRecord}.
 * Mar 13, 2013 1802       bphillip    Updated to use spring injection of dao
 * Mar 27, 2013 1802       bphillip    Made jaxb manager static and changed visibility of a method
 * May 22, 2013 1917       rjpeter     Added ability to save raw and aggregate stats, to reclaimSpace every scan call,
 *                                     and to not pretty print xml grouping information.
 * </pre>
 * 
 * @author jsanchez
 * 
 */
@Component
@Transactional
public class AggregateManager {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AggregateManager.class);

    private AggregateRecordDao aggregateDao;

    private StatsDao statsRecordDao;

    private static final Object[] EMPTY_OBJ_ARR = new Object[0];

    private static JAXBManager jaxbManager;

    /** In minutes */
    private int bucketInterval;

    /** default value */
    private static final int defaultBucketInterval = 5;

    public AggregateManager() {

    }

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
    private void aggregate(StatisticsEventConfig statsEvent,
            TimeRange timeRange,
            Multimap<StatsGroupingColumn, StatisticsEvent> groupedEvents)
            throws JAXBException {
        Calendar start = TimeUtil.newGmtCalendar();
        start.setTime(timeRange.getStart());

        Calendar end = TimeUtil.newGmtCalendar();
        end.setTime(timeRange.getEnd());

        // perform aggregate functions on the grouped data
        for (StatsGroupingColumn group : groupedEvents.keySet()) {
            Collection<StatisticsEvent> groupData = groupedEvents.get(group);
            String groupKey = jaxbManager.marshalToXml(group, false);

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

                    for (StatisticsEvent event : groupData) {
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
                    aggregateDao.mergeRecord(record);
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
        OfflineStatsManager offline = new OfflineStatsManager();
        Map<String, StatisticsEventConfig> statsMap = configLoader
                .getTypeView();

        // latest time to pull
        Calendar timeToProcess = Calendar.getInstance(TimeZone
                .getTimeZone("GMT"));
        int count = 0;

        // process the events by type
        for (Map.Entry<String, StatisticsEventConfig> entry : statsMap
                .entrySet()) {
            String type = entry.getKey();
            StatisticsEventConfig event = entry.getValue();
            List<StatsRecord> records = null;

            do {
                // retrieve stats in blocks of 1000
                records = statsRecordDao.retrieveRecords(timeToProcess, type,
                        2000);

                if (!CollectionUtil.isNullOrEmpty(records)) {
                    // sort events into time buckets
                    Map<TimeRange, Multimap<StatsGroupingColumn, StatisticsEvent>> timeMap = sort(
                            event, records);

                    for (Map.Entry<TimeRange, Multimap<StatsGroupingColumn, StatisticsEvent>> timeMapEntry : timeMap
                            .entrySet()) {
                        aggregate(event, timeMapEntry.getKey(),
                                timeMapEntry.getValue());
                    }

                    try {
                        statsRecordDao.deleteAll(records);
                    } catch (Exception e) {
                        statusHandler.error("Error deleting stat records", e);
                    }

                    count += records.size();
                    if (event.getRawOfflineRetentionDays() >= 0) {
                        offline.writeStatsToDisk(event, timeMap);
                    }
                }
            } while (!CollectionUtil.isNullOrEmpty(records));
        }

        statsRecordDao.reclaimSpace();
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
    private Map<TimeRange, Multimap<StatsGroupingColumn, StatisticsEvent>> sort(
            StatisticsEventConfig statEvent, List<StatsRecord> records) {
        Map<TimeRange, Multimap<StatsGroupingColumn, StatisticsEvent>> rval = new HashMap<TimeRange, Multimap<StatsGroupingColumn, StatisticsEvent>>();
        TimeRange timeRange = null;
        Multimap<StatsGroupingColumn, StatisticsEvent> eventsByGroup = null;

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
                StatisticsEvent event = SerializationUtil.transformFromThrift(
                        StatisticsEvent.class, record.getEvent());

                StatsGroupingColumn group = determineGroupRepresentationForEvent(
                        statEvent, event);
                if (group != null) {
                    eventsByGroup.put(group, event);
                }
            } catch (Exception e) {
                statusHandler
                        .error("Error processing event. Aggregation may be inaccurate. ",
                                e);
            }
        }

        return rval;
    }

    @VisibleForTesting
    static StatsGroupingColumn determineGroupRepresentationForEvent(
            StatisticsEventConfig statEvent, StatisticsEvent event)
            throws IllegalAccessException, InvocationTargetException {
        Iterator<Method> gMethodIter = statEvent.getGroupByMethods().iterator();
        Iterator<StatisticsGroup> gFieldNameIter = statEvent.getGroupList()
                .iterator();
        List<StatsGrouping> groupings = new ArrayList<StatsGrouping>();

        while (gMethodIter.hasNext() && gFieldNameIter.hasNext()) {
            Method m = gMethodIter.next();
            String field = gFieldNameIter.next().getName();
            String gVal = String.valueOf(m.invoke(event, EMPTY_OBJ_ARR));
            groupings.add(new StatsGrouping(field, gVal));
        }

        StatsGroupingColumn column = new StatsGroupingColumn();
        column.setGroup(groupings);

        return column;
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
        if ((60 % incrementsWithinHour) != 0) {
            bucketInterval = defaultBucketInterval;
            statusHandler
                    .info("The bucket interval must go into an hour evenly. Setting bucket interval to '"
                            + bucketInterval + "'");
        }
    }

    /**
     * Scans the aggregate table for aggregate statistics to offline. It doesn't
     * process any aggregate from within the 12 hours.
     */
    public void offlineAggregates() {
        ConfigLoader configLoader = ConfigLoader.getInstance();
        OfflineStatsManager offline = new OfflineStatsManager();
        Map<String, StatisticsEventConfig> statsMap = configLoader
                .getTypeView();

        // offline aggregate data older than 6 hours
        long maxTime = ((System.currentTimeMillis() / TimeUtil.MILLIS_PER_HOUR) - 6)
                * TimeUtil.MILLIS_PER_HOUR;

        for (StatisticsEventConfig conf : statsMap.values()) {
            if (conf.getAggregateOfflineRetentionDays() >= 0) {
                String eventType = conf.getType();

                try {
                    Date oldestAggregateDate = aggregateDao
                            .getOldestAggregateDate(eventType);
                    if (oldestAggregateDate != null) {
                        Date mostRecentOfflineDate = offline
                                .getMostRecentOfflinedAggregate(conf);

                        long startHour = oldestAggregateDate.getTime()
                                / TimeUtil.MILLIS_PER_HOUR;

                        if (mostRecentOfflineDate != null) {
                            // move ahead one hour from most recent time on disk
                            long offlineHour = (mostRecentOfflineDate.getTime() / TimeUtil.MILLIS_PER_HOUR) + 1;
                            if (offlineHour > startHour) {
                                startHour = offlineHour;
                            }
                        }

                        Date startDate = new Date(startHour
                                * TimeUtil.MILLIS_PER_HOUR);
                        // process an hour at a time
                        Date endDate = new Date(startDate.getTime()
                                + TimeUtil.MILLIS_PER_HOUR);
                        while (endDate.getTime() <= maxTime) {
                            List<AggregateRecord> records = aggregateDao
                                    .getAggregates(eventType, startDate,
                                            endDate);
                            offline.writeAggregatesToDisk(conf, records);
                            startDate = endDate;
                            endDate = new Date(startDate.getTime()
                                    + TimeUtil.MILLIS_PER_HOUR);
                        }
                    }
                } catch (Exception e) {
                    statusHandler.error(
                            "Error occured generating offline aggregates for event "
                                    + conf.getType(), e);
                }
            }
        }

        // zip up old data?
    }

    public void setJaxbManager(JAXBManager jaxbManager) {
        AggregateManager.jaxbManager = jaxbManager;
    }

    public void setAggregateDao(AggregateRecordDao aggregateDao) {
        this.aggregateDao = aggregateDao;
    }

    public void setStatsRecordDao(StatsDao statsRecordDao) {
        this.statsRecordDao = statsRecordDao;
    }
}
