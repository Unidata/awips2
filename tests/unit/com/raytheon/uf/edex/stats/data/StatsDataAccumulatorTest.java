package com.raytheon.uf.edex.stats.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import org.junit.Test;

import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.util.UnitUtils;
import com.raytheon.uf.common.time.TimeRange;

public class StatsDataAccumulatorTest {

    @Test
    public void testCalculateBinsCalculatesCorrectly() {
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.set(Calendar.MILLISECOND, 0);
        c.set(Calendar.SECOND, 0);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.HOUR_OF_DAY, 0);
        c.set(Calendar.DAY_OF_MONTH, 1);
        c.set(Calendar.MONTH, 0);
        long start = c.getTimeInMillis();

        c.add(Calendar.DAY_OF_MONTH, 1);
        long end = c.getTimeInMillis();

        TimeRange tr = new TimeRange(start, end);
        StatsDataAccumulator acc = new StatsDataAccumulator();
        acc.setTimeRange(tr);
        acc.setTimeStep(5);

        acc.calculateBins();

        int expectedBinCount = 288;  // 5 minute bins 12 per hour, * 24
        int actualBinCount = acc.bins.keySet().size();
        assertEquals("Bin Counts do not match", expectedBinCount, actualBinCount);

        int count = 0;
        for (long bin : acc.bins.keySet()) {
            assertEquals("Bin times do not match", count, bin);
            count++;
        }
    }

    @Test
    public void testSetupGroupings() {
        List<AggregateRecord> recordList = getTestRecords();
        StatsDataAccumulator acc = new StatsDataAccumulator();
        acc.setRecords(recordList.toArray(new AggregateRecord[recordList.size()]));
        acc.setupGroupings();

        List<String> expectedGroups = new ArrayList<String>();
        expectedGroups.add("owner");
        expectedGroups.add("provider");
        expectedGroups.add("plugin");

        List<String> expectedGroupMembers = new ArrayList<String>();
        expectedGroupMembers.add("nomads");
        expectedGroupMembers.add("madis");
        expectedGroupMembers.add("owner0");
        expectedGroupMembers.add("owner1");
        expectedGroupMembers.add("owner2");
        expectedGroupMembers.add("owner3");
        expectedGroupMembers.add("owner4");
        expectedGroupMembers.add("grid");


        // Check the groups
        for (String group : acc.groups) {
            assertTrue(expectedGroups.contains(group));
        }

        // Check the group members
        for (String key: acc.groupMemberMap.keySet()) {
            for (String member: acc.groupMemberMap.get(key)) {
                assertTrue(expectedGroupMembers.contains(member));
            }
        }
    }

    @Test
    public void testCreateStatsDataMapCreation() {
        String eventType = "com.raytheon.uf.common.datadelivery.event.retrieval.DataRetrievalEvent";
        String dataType = "bytes";
        String displayUnit = "MB";

        UnitUtils unitUtils = new UnitUtils(eventType, dataType);
        unitUtils.setDisplayUnit(displayUnit);

        List<AggregateRecord> recordList = getTestRecords();
        StatsDataAccumulator acc = new StatsDataAccumulator();
        acc.setRecords(recordList.toArray(new AggregateRecord[recordList.size()]));
        acc.setEventType(eventType);
        acc.setDataType(dataType);
        acc.setupGroupings();

        acc.createStatsDataMap(unitUtils, acc.groups);

        Set<String> expectedSet = new HashSet<String>();
        expectedSet.add("owner0:nomads");
        expectedSet.add("owner1:nomads");
        expectedSet.add("owner2:nomads");
        expectedSet.add("owner3:nomads");
        expectedSet.add("owner4:nomads");
        expectedSet.add("owner0:madis");
        expectedSet.add("owner1:madis");
        expectedSet.add("owner2:madis");
        expectedSet.add("owner3:madis");
        expectedSet.add("owner4:madis");

        for (String key : acc.statsDataMap.keySet()) {
            assertTrue(expectedSet.contains(key));
        }

    }

    // Build the Aggregate records
    private List<AggregateRecord> getTestRecords() {
        String plugin = "plugin";
        String provider = "provider";
        String nomads = "nomads";
        String madis = "madis";
        String owner = "owner";
        String grid = "grid";
        String dash = "-";
        String colon = ":";

        List<String> groupings = new ArrayList<String>();
        for (int i = 0; i < 5; i++) {
            groupings.add(plugin + colon + grid + dash + owner + colon + owner + i);
        }
        for (int i = 0; i < 5; i++) {
            groupings.add(plugin + colon + grid + dash + owner + colon + owner + i);
        }
        for (int i = 0; i < 5; i++) {
            groupings.add(plugin + colon + grid + dash + owner + colon + owner + i);
        }

        for (int i = 0; i < 5; i++) {
            groupings.add(provider + colon + nomads + dash + owner + colon + owner + i);
        }
        for (int i = 0; i < 5; i++) {
            groupings.add(provider + colon + madis + dash + owner + colon + owner + i);
        }

        List<AggregateRecord> records = new ArrayList<AggregateRecord>();

        for (String group : groupings) {
            AggregateRecord r = new AggregateRecord();

            if (group.contains("provider")) {
                r.setEventType("com.raytheon.uf.common.datadelivery.event.retrieval.DataRetrievalEvent");
                r.setField("bytes");
            } else {
                r.setEventType("com.raytheon.uf.common.datadelivery.event.retrieval.SubscriptionRetrievalEvent");
                r.setField("numRecords");
            }
            r.setGrouping(group);
            records.add(r);
        }

        return records;
    }
}
