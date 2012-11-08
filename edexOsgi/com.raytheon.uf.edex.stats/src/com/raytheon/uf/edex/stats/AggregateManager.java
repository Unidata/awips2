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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.StatsRecord;
import com.raytheon.uf.common.stats.xml.StatisticsAggregate;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;
import com.raytheon.uf.common.stats.xml.StatisticsEvent;
import com.raytheon.uf.common.stats.xml.StatisticsGroup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.stats.dao.StatsDao;
import com.raytheon.uf.edex.stats.handler.StatsHandler;
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
 *
 * </pre>
 *
 * @author jsanchez
 *
 */
public class AggregateManager {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AggregateManager.class);

    private class TimeRangeKey extends TimeRange {
        public TimeRangeKey(Calendar cal1, Calendar cal2) {
            super(cal1, cal2);
        }

        @Override
        public boolean equals(Object o) {
            if (o != null && o instanceof TimeRange) {
                TimeRange other = (TimeRange) o;

                return (this.getStart().equals(other.getStart()) && this
                        .getEnd().equals(other.getEnd()));
            }

            return false;
        }

        @Override
        public int hashCode() {
            return 1;
        }

        @Override
        public String toString() {
            return super.toString();
        }
    }

    /** In minutes */
    private int bucketInterval;

    /** In minutes */
    private int scanInterval;

    /** default value */
    private static final int defaultBucketInterval = 5;

    /** default value */
    private static final int defaultScanInterval = 15;

    /** loads localized copies of the statsConfig */
    private final ConfigLoader configLoader;

    private final CoreDao aggregateRecordDao = new CoreDao(DaoConfig.forClass(
            "metadata", AggregateRecord.class));

    public AggregateManager(String bucketInterval, String scanInterval)
            throws Exception {
        this.configLoader = new ConfigLoader();
        validateIntervals(bucketInterval, scanInterval);
        configLoader.load();
        StatsHandler.setValidEventTypes(configLoader.getConfigurations());
    }

    /**
     * Tests if the bucket interval and the scan interval are valid values. If
     * values are invalid then values will be set to default values.
     *
     * @param bucketInt
     * @param scanInt
     * @return
     */
    private void validateIntervals(String bucketInt, String scanInt) {
        try {
            bucketInterval = Integer.parseInt(bucketInt);
        } catch (NumberFormatException e) {
            bucketInterval = defaultBucketInterval;
            statusHandler.info("'" + bucketInt
                    + "' is not a valid bucket interval value. Setting to '"
                    + defaultBucketInterval + "'");
        }

        try {
            scanInterval = Integer.parseInt(scanInt);
        } catch (NumberFormatException e) {
            scanInterval = defaultScanInterval;
            statusHandler.info("'" + scanInt
                    + "' is not a valid scan interval value. Setting to '"
                    + defaultScanInterval + "'");
        }

        if (scanInterval < bucketInterval) {
            scanInterval = defaultBucketInterval;
            bucketInterval = defaultBucketInterval;
            statusHandler
                    .info("The bucket interval can not be greater than the scan interval. Setting scan interval to '"
                            + defaultBucketInterval
                            + "' and bucket interval to '"
                            + bucketInterval
                            + "'");
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

    /**
     * Scans the stats table to be stored in buckets and aggregate if necessary.
     */
    public void scan() throws Exception {
        StatsDao statsRecordDao = new StatsDao();
        Map<String, Map<TimeRangeKey, List<StatsRecord>>> aggregateBuckets = new HashMap<String, Map<TimeRangeKey, List<StatsRecord>>>();

        // retrieves records and sorts in buckets
        retrieveStatRecords(statsRecordDao, aggregateBuckets);

        // loops through map to aggregate buckets
        for (StatisticsConfig statsConfig : configLoader.getConfigurations()) {
            for (StatisticsEvent event : statsConfig.getEvents()) {
                String eventType = event.getType();

                Map<TimeRangeKey, List<StatsRecord>> map = aggregateBuckets
                        .get(eventType);
                // map should never be null, since it will be set in the 'sort'
                // method.
                for (Iterator<Map.Entry<TimeRangeKey, List<StatsRecord>>> iter = map
                        .entrySet().iterator(); iter.hasNext();) {
                    Entry<TimeRangeKey, List<StatsRecord>> element = iter
                            .next();
                    TimeRangeKey tr = element.getKey();
                    List<StatsRecord> records = element.getValue();
                    if (!records.isEmpty()) {
                        List<Event> data = extractEvents(records);
                        aggregate(event, tr, data);
                        try {
                            statsRecordDao.deleteAll(records);
                        } catch (Exception e) {
                            statusHandler.error("Error deleting stat records",
                                    e);
                        }
                    }
                    iter.remove();
                }
            }
        }
    }

    /**
     * Retrieve StatRecords from the metadata.event.stats table. This method
     * does not retrieve records of the current bucket.
     */
    private void retrieveStatRecords(StatsDao statsRecordDao,
            Map<String, Map<TimeRangeKey, List<StatsRecord>>> aggregateBuckets)
            throws Exception {
        Calendar current = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        for (StatisticsConfig statsConfig : configLoader.getConfigurations()) {
            for (StatisticsEvent event : statsConfig.getEvents()) {
                String eventType = event.getType();
                // Does not retrieve stat records of current bucket.
                // this method should always return a valid array.
                StatsRecord[] records = statsRecordDao.retrieveRecords(
                        getBucketStartTime(current), eventType);
                sort(eventType, records, aggregateBuckets);
            }
        }
    }

    /**
     * Stores the results into proper aggregate buckets. This method assumes
     * that the records are in date order.
     *
     * @param events
     */
    private void sort(String eventType, StatsRecord[] records,
            Map<String, Map<TimeRangeKey, List<StatsRecord>>> aggregateBuckets)
            throws Exception {
        Map<TimeRangeKey, List<StatsRecord>> map = aggregateBuckets
                .get(eventType);
        if (map == null) {
            map = new HashMap<TimeRangeKey, List<StatsRecord>>();
            aggregateBuckets.put(eventType, map);
        }

        TimeRangeKey timeRange = null;
        for (StatsRecord record : records) {
            if (timeRange == null
                    || !timeRange.contains(record.getDate().getTime())) {
                // Create bucket based on stats record date
                timeRange = createTimeRangeKey(record.getDate());
            }

            List<StatsRecord> bucketList = map.get(timeRange);
            if (bucketList == null) {
                bucketList = new ArrayList<StatsRecord>();
                map.put(timeRange, bucketList);
            }
            bucketList.add(record);
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
    private TimeRangeKey createTimeRangeKey(Calendar date) {
        Calendar start = getBucketStartTime(date);
        Calendar end = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        end.setTimeInMillis(start.getTimeInMillis());
        end.add(Calendar.MINUTE, bucketInterval);

        TimeRangeKey timeRangeKey = new TimeRangeKey(start, end);

        return timeRangeKey;
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
     * Extracts the events from the stats records.
     *
     * @param records
     * @return
     */
    private List<Event> extractEvents(List<StatsRecord> records) {
        List<Event> eventsList = new ArrayList<Event>(records.size());

        for (StatsRecord record : records) {
            try {
                Event event = (Event) SerializationUtil
                        .transformFromThrift(record.getEvent());
                eventsList.add(event);
            } catch (SerializationException e) {
                statusHandler
                        .error("Error trying to transform event. Aggregation may be inaccurate. ",
                                e);
            }
        }

        return eventsList;
    }

    /**
     * Performs the aggregation based on the statsConfig file.
     *
     * @param key
     * @param data
     */
    private void aggregate(StatisticsEvent statsEvent, TimeRange timeRange,
            List<Event> data) {
        Calendar start = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        start.setTime(timeRange.getStart());

        Calendar end = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        end.setTime(timeRange.getEnd());

        // collect grouping names from stats config
        List<String> groupByColumns = new ArrayList<String>();
        for (StatisticsGroup groupBy : statsEvent.getGroupList()) {
            String column = groupBy.getName();
            groupByColumns.add(column);
        }

        // breaks data into groups
        Map<String, List<Event>> map = divideIntoGroups(data, groupByColumns);

        // perform aggregate functions on the grouped data
        for (String groupKey : map.keySet()) {

            List<Event> groupData = map.get(groupKey);
            for (StatisticsAggregate aggregate : statsEvent.getAggregateList()) {
                String field = aggregate.getField();
                try {
                    double[] values = new double[groupData.size()];
                    String methodName = getterMethodName(field);
                    for (int i = 0; i < groupData.size(); i++) {
                        Object obj = groupData.get(i);
                        Class<?> clazz = obj.getClass();
                        Method m = clazz.getMethod(methodName, new Class<?>[0]);
                        Number number = (Number) m.invoke(obj, new Object[0]);
                        values[i] = number.doubleValue();
                    }

                    double count = values.length;
                    double max = 0;
                    double min = Double.MAX_VALUE;
                    double sum = 0;

                    for (int i = 0; i < values.length; i++) {
                        sum += values[i];
                        if (values[i] > max) {
                            max = values[i];
                        }

                        if (values[i] < min) {
                            min = values[i];
                        }
                    }

                    AggregateRecord record = new AggregateRecord(
                            statsEvent.getType(), start, end, groupKey, field);
                    record.setSum(sum);
                    record.setMin(min);
                    record.setMax(max);
                    record.setCount(count);
                    aggregateRecordDao.persist(record);
                } catch (Exception e) {
                    statusHandler.error("Unable to aggregate '" + field + "'",
                            e);
                }
            }
        }
    }

    /**
     * Breaks the list of data into groups based on groupByColumns. The key is a
     * concatenation of the column values (i.e. datatype.username). This method
     * can group data to n-number of levels.
     *
     * @param data
     * @param groupByColumns
     * @return
     */
    private Map<String, List<Event>> divideIntoGroups(List<Event> data,
            List<String> groupByColumns) {
        Map<String, List<Event>> map = new HashMap<String, List<Event>>();
        map.put("", data);
        for (String column : groupByColumns) {

            List<Map<String, List<Event>>> listOfMaps = new ArrayList<Map<String, List<Event>>>();
            for (String parent : map.keySet()) {
                List<Event> list = map.get(parent);
                listOfMaps.add(group(list, column, parent));
            }

            map.clear();

            // replace map with grouped data
            for (Map<String, List<Event>> m : listOfMaps) {
                for (String k : m.keySet()) {
                    map.put(k, m.get(k));
                }
            }
        }

        return map;
    }

    /**
     * Helper method to group data to one level.
     *
     * @param data
     * @param column
     * @param parent
     * @return
     */
    private Map<String, List<Event>> group(List<Event> data, String column,
            String parent) {
        Map<String, List<Event>> map = new HashMap<String, List<Event>>();
        String methodName = getterMethodName(column);
        for (Event rec : data) {
            try {
                Class<?> clazz = rec.getClass();
                Method m = clazz.getMethod(methodName, new Class<?>[0]);
                String value = column + ":"
                        + String.valueOf(m.invoke(rec, new Object[0]));
                if (parent.length() > 0) {
                    value = parent + "-" + value;
                }
                List<Event> list = map.get(value);
                if (list == null) {
                    list = new ArrayList<Event>();
                }
                list.add(rec);
                map.put(value, list);
            } catch (Exception e) {
                statusHandler.error("Error creating groups", e);
            }
        }

        return map;
    }

    /**
     * Returns the name of the getter method for the parameter
     *
     * @param parameter
     * @return
     */
    private String getterMethodName(String parameter) {
        return "get" + parameter.substring(0, 1).toUpperCase()
                + parameter.substring(1);
    }

}
