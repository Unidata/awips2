package com.raytheon.uf.common.stats.data;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

import org.junit.Test;

import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.util.DataView;

public class DataPointTest {
    private final String eventType = "com.raytheon.uf.common.stats.ProcessEvent";

    private final String field = "processingTime";

    private final String grouping = "pluginName:obs";

    @Test
    public void testObjectCreationReturnsCorrectCount() {
        List<AggregateRecord> records = getTestRecords();

        DataPoint point = new DataPoint();

        for (AggregateRecord rec : records) {
            // Check for an existing point object
            point.setMax(rec.getMax());
            point.setMin(rec.getMin());
            point.setSum(rec.getSum());
            point.addToCount(rec.getCount());
        }

        double expectedCount = 4; // sum of both test object's count value

        assertEquals("Count does not match", expectedCount, point.getCount(), 0);

        assertEquals("Count does not match", expectedCount,
                point.getValue(DataView.COUNT), 0);
    }

    @Test
    public void testObjectCreationReturnsCorrectSum() {
        List<AggregateRecord> records = getTestRecords();

        DataPoint point = new DataPoint();

        for (AggregateRecord rec : records) {
            // Check for an existing point object
            point.setMax(rec.getMax());
            point.setMin(rec.getMin());
            point.setSum(rec.getSum());
            point.addToCount(rec.getCount());
        }

        double expectedSum = 365; // sum of both test object's sum value

        assertEquals("Sum does not match", expectedSum, point.getSum(), 0);

        assertEquals("Sum does not match", expectedSum,
                point.getValue(DataView.SUM), 0);
    }

    @Test
    public void testObjectCreationReturnsCorrectMinValue() {
        List<AggregateRecord> records = getTestRecords();

        DataPoint point = new DataPoint();

        for (AggregateRecord rec : records) {
            // Check for an existing point object
            point.setMax(rec.getMax());
            point.setMin(rec.getMin());
            point.setSum(rec.getSum());
            point.addToCount(rec.getCount());
        }

        double expectedMin = 5; // smallest min value of both test objects

        assertEquals("Min does not match", expectedMin, point.getMin(), 0);

        assertEquals("Min does not match", expectedMin,
                point.getValue(DataView.MIN), 0);
    }

    @Test
    public void testObjectCreationReturnsCorrectMaxValue() {
        List<AggregateRecord> records = getTestRecords();

        DataPoint point = new DataPoint();

        for (AggregateRecord rec : records) {
            // Check for an existing point object
            point.setMax(rec.getMax());
            point.setMin(rec.getMin());
            point.setSum(rec.getSum());
            point.addToCount(rec.getCount());
        }

        double expectedMax = 200; // largest max value of both test objects

        assertEquals("Max does not match", expectedMax, point.getMax(), 0);

        assertEquals("Max does not match", expectedMax,
                point.getValue(DataView.MAX), 0);
    }

    @Test
    public void testObjectCreationReturnsCorrectAverage() {
        List<AggregateRecord> records = getTestRecords();

        DataPoint point = new DataPoint();

        for (AggregateRecord rec : records) {
            // Check for an existing point object
            point.setMax(rec.getMax());
            point.setMin(rec.getMin());
            point.setSum(rec.getSum());
            point.addToCount(rec.getCount());
        }

        double expectedCount = 4; // sum of both test object's count value
        double expectedSum = 365; // sum of both test object's sum value
        double expectedAvg = expectedSum / expectedCount;

        assertEquals("Avg does not match", expectedAvg, point.getAvg(), 0.25);

        assertEquals("Avg does not match", expectedAvg,
                point.getValue(DataView.AVG), 0.25);
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
        start.set(Calendar.MINUTE, 5);

        end = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        end.set(Calendar.MILLISECOND, 0);
        end.set(Calendar.SECOND, 0);
        end.set(Calendar.MINUTE, 10);

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
