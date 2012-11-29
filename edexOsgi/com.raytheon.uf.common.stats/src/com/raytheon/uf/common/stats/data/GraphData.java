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
package com.raytheon.uf.common.stats.data;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.StatisticsEvent;
import com.raytheon.uf.common.stats.util.UnitUtils;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.ReflectionException;
import com.raytheon.uf.common.util.ReflectionUtil;

/**
 * Data object for the statistics graph.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012     728     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class GraphData {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GraphData.class);

    /** Map of milliseconds -> StatsBin object */
    @DynamicSerializeElement
    private final Map<Long, StatsBin> bins = new TreeMap<Long, StatsBin>();

    /** List of member keys */
    @DynamicSerializeElement
    private List<String> keys;

    /** AggregateRecord list */
    @DynamicSerializeElement
    private List<AggregateRecord> recordList = new ArrayList<AggregateRecord>();

    /**
     * key -> stats data object
     */
    @DynamicSerializeElement
    private Map<String, StatsData> statsDataMap = new HashMap<String, StatsData>();

    /** Time range */
    @DynamicSerializeElement
    private TimeRange timeRange;

    /** Map of group -> key */
    @DynamicSerializeElement
    private final Map<String, String> keySequenceMap = new LinkedHashMap<String, String>();

    /** StatsLabelData object */
    @DynamicSerializeElement
    private StatsLabelData statsLabelData;

    /** Display unit */
    @DynamicSerializeElement
    private String displayUnit;

    /** UnitUtils object */
    private UnitUtils unitUtils;

    /**
     * Constructor.
     */
    public GraphData() {

    }

    /**
     * @return the bins
     */
    public Map<Long, StatsBin> getStatsBins() {
        return bins;
    }

    /**
     * @param recordList
     *            the recordList to set
     */
    public void setRecordList(List<AggregateRecord> recordList) {
        this.recordList = recordList;
    }

    /**
     * @return the recordList
     */
    public List<AggregateRecord> getRecordList() {
        return recordList;
    }

    /**
     * Add an AggregateRecord.
     *
     * @param record
     */
    public void addRecord(AggregateRecord record) {
        if (!recordList.contains(record)) {
            recordList.add(record);
        }
    }

    /**
     * @return the statsData
     */
    public StatsData getStatsData(String groupMemberName) {
        return this.statsDataMap.get(groupMemberName);
    }

    /**
     * Get a list of group memebers.
     *
     * @return
     */
    public List<String> getGroupMembers() {
        List<String> memberList = new ArrayList<String>(statsDataMap.keySet());
        Collections.sort(memberList);

        return memberList;
    }

    /**
     * Get the smallest value in the data set.
     *
     * @return
     */
    public double getMinValue() {
        double min = Double.MAX_VALUE;
        for (String key : statsDataMap.keySet()) {
            double minVal = statsDataMap.get(key).getMinValue();
            if (minVal < min) {
                min = minVal;
            }
        }

        return min;
    }

    /**
     * Get the largest value in the data set.
     *
     * @return
     */
    public double getMaxValue() {
        double max = Double.MIN_VALUE;
        for (String key : statsDataMap.keySet()) {
            double maxVal = statsDataMap.get(key).getMaxValue();
            if (maxVal > max) {
                max = maxVal;
            }
        }

        return max;
    }

    /**
     * Get the smallest value in the data set.
     *
     * @return
     */
    public double getMinValue(Set<String> visibleDataSet, String view) {
        if (visibleDataSet.isEmpty()) {
            return 0;
        }

        double min = Double.MAX_VALUE;
        for (String key : statsDataMap.keySet()) {
            if (visibleDataSet.contains(key)) {
                double minVal = statsDataMap.get(key).getMinValue(view);
                if (minVal < min) {
                    min = minVal;
                }
            }
        }

        return min;
    }

    /**
     * Get the largest value in the data set.
     *
     * @return
     */
    public double getMaxValue(Set<String> visibleDataSet, String view) {
        if (visibleDataSet.isEmpty()) {
            return 1;
        }
        double max = Double.MIN_VALUE;
        for (String key : statsDataMap.keySet()) {
            if (visibleDataSet.contains(key)) {
                double maxVal = statsDataMap.get(key).getMaxValue(view);
                if (maxVal > max) {
                    max = maxVal;
                }
            }
        }

        return max;
    }

    /**
     * Get the time range for this object.
     *
     * @return
     */
    public TimeRange getTimeRange() {
        return this.timeRange;
    }

    /**
     * Set the TimeRange
     *
     * @param timeRange
     */
    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    /**
     * Set the key sequence
     *
     * @param keySequence
     */
    public void setKeySequence(List<String> keySequence) {
        for (String key : keySequence) {
            keySequenceMap.put(key, "");
        }

        keys = keySequence;
    }

    /**
     * Get the key sequence.
     *
     * @return
     */
    public List<String> getKeySequence() {
        return keys;
    }

    /**
     * Get a list of all keys.
     *
     * @return the keys
     */
    public List<String> getKeys() {
        return keys;
    }

    /**
     * Get a list of keys that contain data.
     *
     * @return the keys with data
     */
    public List<String> getKeysWithData() {
        List<String> keysWithData = new ArrayList<String>();
        for (String key : keys) {
            StatsData sd = this.getStatsData(key);
            if (sd != null) {
                keysWithData.add(key);
            }
        }

        return keysWithData;
    }

    /**
     * Set the StatsLabelData object
     *
     * @param statsLabelData
     */
    public void setStatsLabelData(StatsLabelData statsLabelData) {
        this.statsLabelData = statsLabelData;
        this.keys = statsLabelData.makeKeys();
    }

    /**
     * Get the StatsLabelData
     *
     * @return
     */
    public StatsLabelData getStatsLabelData() {
        return this.statsLabelData;
    }

    /**
     * Get the group and names map.
     *
     * @return
     */
    public Map<String, List<String>> getGroupAndNamesMap() {
        return statsLabelData.getGroupAndNamesMap();
    }

    /**
     * Get the units from the event object
     *
     * @param eventId
     *            Event id
     * @param field
     *            data field
     *
     * @return The units
     */
    @VisibleForTesting
    static String getUnitsFromEventObject(String eventId, String field) {
        // Get the units from the event class
        String units = "Count";

        try {
            StatisticsEvent obj = ReflectionUtil.newInstanceOfAssignableType(
                    StatisticsEvent.class, eventId);
            units = obj.getStorageUnit(field);
        } catch (ReflectionException e) {
            statusHandler.handle(Priority.ERROR, "Error Instantiating "
                    + eventId, e);
        }

        return units;
    }

    /**
     * @return the statsDataMap
     */
    public Map<String, StatsData> getStatsDataMap() {
        return statsDataMap;
    }

    /**
     * @return the keySequenceMap
     */
    public Map<String, String> getKeySequenceMap() {
        return keySequenceMap;
    }

    /**
     * @return the displayUnit
     */
    public String getDisplayUnit() {
        return displayUnit;
    }

    /**
     * @param displayUnit
     *            the displayUnit to set
     */
    public void setDisplayUnit(String displayUnit) {
        this.displayUnit = displayUnit;
    }

    /**
     * @param keys
     *            the keys to set
     */
    public void setKeys(List<String> keys) {
        this.keys = keys;
    }

    /**
     * Set the StatsData map.
     *
     * @param statsDataMap
     */
    public void setStatsDataMap(Map<String, StatsData> statsDataMap) {
        this.statsDataMap = statsDataMap;
    }

    /**
     * @return the unitUtils
     */
    public UnitUtils getUnitUtils() {
        return unitUtils;
    }

    /**
     * @param unitUtils
     *            the unitUtils to set
     */
    public void setUnitUtils(UnitUtils unitUtils) {
        this.unitUtils = unitUtils;
        this.unitUtils.setConversion(this.getMaxValue());
    }
}
