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
package com.raytheon.uf.edex.stats.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.xml.bind.JAXBException;

import org.junit.Test;

import com.google.common.collect.Maps;
import com.raytheon.uf.common.datadelivery.event.retrieval.DataRetrievalEvent;
import com.raytheon.uf.common.datadelivery.event.retrieval.SubscriptionRetrievalEvent;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.StatsGrouping;
import com.raytheon.uf.common.stats.StatsGroupingColumn;
import com.raytheon.uf.common.stats.data.StatsData;
import com.raytheon.uf.common.stats.util.UnitUtils;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * 
 * Test {@link StatsDataAccumulator}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2013 1487       djohnson     Use XML for grouping column.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class StatsDataAccumulatorTest {

    private static final JAXBManager JAXB_MANAGER;
    static {
        try {
            JAXB_MANAGER = new JAXBManager(StatsGroupingColumn.class);
        } catch (JAXBException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    @Test
    public void testCalculateBinsCalculatesCorrectly() {
        TimeRange tr = new TimeRange(0L, TimeUtil.MILLIS_PER_DAY);
        StatsDataAccumulator acc = new StatsDataAccumulator();
        acc.setTimeRange(tr);
        acc.setTimeStep(5);

        acc.calculateBins();

        int expectedBinCount = 288; // 5 minute bins 12 per hour, * 24
        int actualBinCount = acc.bins.keySet().size();
        assertEquals("Bin Counts do not match", expectedBinCount,
                actualBinCount);

        int count = 0;
        for (long bin : acc.bins.keySet()) {
            assertEquals("Bin times do not match", count, bin);
            count++;
        }
    }

    @Test
    public void testSetupGroupings() throws JAXBException {
        List<AggregateRecord> recordList = getTestRecords();
        StatsDataAccumulator acc = new StatsDataAccumulator();
        acc.setRecords(recordList.toArray(new AggregateRecord[recordList.size()]));
        acc.setupGroupings();

        List<String> expectedGroups = new ArrayList<String>();
        expectedGroups.add("owner");
        expectedGroups.add("provider");
        expectedGroups.add("plugin");

        List<String> expectedPlugins = Arrays.asList("grid");
        List<String> expectedProviders = Arrays.asList("nomads", "madis");
        List<String> expectedOwners = Arrays.asList("owner0", "owner1",
                "owner2", "owner3", "owner4");

        Map<String, List<String>> expectedGroupsToValues = Maps.newHashMap();
        expectedGroupsToValues.put("provider", expectedProviders);
        expectedGroupsToValues.put("plugin", expectedPlugins);
        expectedGroupsToValues.put("owner", expectedOwners);

        // Check the groups
        for (String expected : expectedGroups) {
            assertTrue("Did not find group [" + expected
                    + "] in the group collection!",
                    acc.groups.contains(expected));
        }

        // Check the group members
        final Map<String, Set<String>> groupMemberMap = acc.groupMemberMap;
        for (Entry<String, List<String>> entry : expectedGroupsToValues
                .entrySet()) {
            final String groupName = entry.getKey();
            final Set<String> setToCheck = groupMemberMap.get(groupName);
            for (String member : entry.getValue()) {
                assertTrue("Did not find entry [" + member + "] for group ["
                        + groupName + "]!", setToCheck.contains(member));
            }
        }
    }

    @Test
    public void testCreateStatsDataMapCreation() throws JAXBException {
        String eventType = DataRetrievalEvent.class.getName();
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

        acc.createStatsDataMap(acc.groups);

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

        final Map<String, StatsData> statsDataMap = acc.statsDataMap;
        for (String expected : expectedSet) {
            assertTrue("Did not find expected value (" + expected
                    + "] as key in the statsDataMap!",
                    statsDataMap.containsKey(expected));
        }

    }

    // Build the Aggregate records
    private List<AggregateRecord> getTestRecords() throws JAXBException {
        String plugin = "plugin";
        String provider = "provider";
        String nomads = "nomads";
        String madis = "madis";
        String owner = "owner";
        String grid = "grid";

        List<StatsGroupingColumn> groupingColumns = new ArrayList<StatsGroupingColumn>();
        for (int i = 0; i < 15; i++) {
            groupingColumns.add(StatsGroupingColumn.withGroupings(
                    new StatsGrouping(plugin, grid), new StatsGrouping(owner,
                            owner + i)));
        }
        for (int i = 0; i < 5; i++) {
            groupingColumns.add(StatsGroupingColumn.withGroupings(
                    new StatsGrouping(provider, nomads), new StatsGrouping(
                            owner, owner + i)));
        }
        for (int i = 0; i < 5; i++) {
            groupingColumns.add(StatsGroupingColumn.withGroupings(
                    new StatsGrouping(provider, madis), new StatsGrouping(
                            owner, owner + i)));
        }

        List<AggregateRecord> records = new ArrayList<AggregateRecord>();

        for (StatsGroupingColumn group : groupingColumns) {
            AggregateRecord r = new AggregateRecord();

            if ("provider".equals(group.getGroup().iterator().next().getName())) {
                r.setEventType(DataRetrievalEvent.class.getName());
                r.setField("bytes");
            } else {
                r.setEventType(SubscriptionRetrievalEvent.class.getName());
                r.setField("numRecords");
            }
            r.setGrouping(JAXB_MANAGER.marshalToXml(group));
            records.add(r);
        }

        return records;
    }
}
