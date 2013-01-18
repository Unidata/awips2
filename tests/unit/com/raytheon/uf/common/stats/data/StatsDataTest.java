package com.raytheon.uf.common.stats.data;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;

import org.junit.Test;

import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.util.UnitUtils;
import com.raytheon.uf.common.time.util.TimeUtil;

public class StatsDataTest {
    private final String eventType = "com.raytheon.uf.common.stats.ProcessEvent";

    private final String field = "processingTime";

    private final String grouping = "pluginName:obs";

    final Map<Long, StatsBin> bins = new TreeMap<Long, StatsBin>();

    @Test
    public void testAccumulateCreatesPoints() {
        List<AggregateRecord> records = getTestRecords();

        long startMillis = records.get(0).getStartDate().getTimeInMillis();

        long timeStep = 5;
        long duration = 60 * TimeUtil.MILLIS_PER_MINUTE;
        long numBins = duration / (timeStep * TimeUtil.MILLIS_PER_MINUTE);
        for (long i = 0; i < numBins; i++) {
            StatsBin sb = new StatsBin();
            sb.setBinMillis(startMillis
                    + (i * timeStep * TimeUtil.MILLIS_PER_MINUTE));
            bins.put(i, sb);
        }

        UnitUtils unitUtils = new UnitUtils(eventType, field);
        unitUtils.setDisplayUnit("ms");

        StatsData statsData = new StatsData("key", TimeUtil.MILLIS_PER_MINUTE,
                null);
        statsData.setBins(bins);
        statsData.addRecord(records.get(0));
        statsData.addRecord(records.get(1));
        statsData.accumulate();

        List<DataPoint> pointList = statsData.getData();

        int expectedPointCount = 2;
        assertEquals("Point Counts differ", expectedPointCount,
                pointList.size(), 0);
    }

    // Build the Aggregate records
    private List<AggregateRecord> getTestRecords() {
        List<AggregateRecord> records = new ArrayList<AggregateRecord>();
        Calendar start = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        start.set(Calendar.MILLISECOND, 0);
        start.set(Calendar.SECOND, 0);
        start.set(Calendar.MINUTE, 0);

        Calendar end = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        end.set(Calendar.MILLISECOND, 0);
        end.set(Calendar.SECOND, 0);
        end.set(Calendar.MINUTE, 5);

        AggregateRecord r = new AggregateRecord();
        r.setCount(2);
        r.setEndDate(end);
        r.setEventType(eventType);
        r.setField(field);
        r.setGrouping(grouping);
        r.setId(1);
        r.setMax(150);
        r.setMin(5);
        r.setStartDate(start);
        r.setSum(155);

        records.add(r);

        start = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        start.set(Calendar.MILLISECOND, 0);
        start.set(Calendar.SECOND, 0);
        start.set(Calendar.MINUTE, 30);

        end = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        end.set(Calendar.MILLISECOND, 0);
        end.set(Calendar.SECOND, 0);
        end.set(Calendar.MINUTE, 35);

        AggregateRecord rr = new AggregateRecord();
        rr.setCount(2);
        rr.setEndDate(end);
        rr.setEventType(eventType);
        rr.setField(field);
        rr.setGrouping(grouping);
        rr.setId(1);
        rr.setMax(200);
        rr.setMin(10);
        rr.setStartDate(start);
        rr.setSum(210);

        records.add(rr);

        return records;
    }
}
