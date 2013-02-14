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
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.stats.AggregateRecord;
import com.raytheon.uf.common.stats.util.DataView;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Statistical data object holding data to be graphed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 07, 2012    728     mpduff      Initial creation.
 * Jan 17, 2013   1357     mpduff      Store data in raw units, not converted.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class StatsData {
    /** Key to this object */
    @DynamicSerializeElement
    private String key;

    /** List of AggregateRecords */
    @DynamicSerializeElement
    private final List<AggregateRecord> recordList = new ArrayList<AggregateRecord>();

    /**
     * Frequency (timestep) of the data 5 min, hourly, daily
     */
    @DynamicSerializeElement
    private int dataFrequency;

    /**
     * Time Range of the data in this object
     */
    @DynamicSerializeElement
    private TimeRange timeRange = new TimeRange();

    /** List of DataPoint objects */
    @DynamicSerializeElement
    private List<DataPoint> pointList = new ArrayList<DataPoint>();

    /** Map of millis -> StatsBin objects */
    @DynamicSerializeElement
    private Map<Long, StatsBin> bins;

    /** Constructor */
    public StatsData() {

    }

    /**
     * Constructor
     * 
     * @param key
     *            Key to the object
     * @param tsMillis
     *            timestep in ms
     * @param timeRange
     *            TimeRange object
     * @param unitUtils
     *            UnitUtils object
     */
    public StatsData(String key, long tsMillis, TimeRange timeRange) {
        this.key = key;
        this.dataFrequency = (int) tsMillis;
        this.timeRange = timeRange;
    }

    /**
     * @return the minValue
     */
    public Double getMinValue() {
        double minValue = Integer.MAX_VALUE;

        for (DataPoint point : pointList) {
            if (point.getMin() < minValue) {
                minValue = point.getMin();
            }
        }

        return minValue;
    }

    /**
     * @return the minValue
     */
    public Double getMinValue(DataView view) {
        double minValue = Integer.MAX_VALUE;

        for (DataPoint point : pointList) {
            if (point.getValue(view) < minValue) {
                minValue = point.getValue(view);
            }
        }

        return minValue;
    }

    /**
     * @return the minValue
     */
    public Double getMaxValue(DataView view) {
        double maxValue = Integer.MIN_VALUE;

        for (DataPoint point : pointList) {
            if (point.getValue(view) > maxValue) {
                maxValue = point.getValue(view);
            }
        }

        return maxValue;
    }

    /**
     * @return the maxValue
     */
    public Double getMaxValue() {
        double maxValue = Integer.MIN_VALUE;

        for (DataPoint point : pointList) {
            if (point.getAvg() > maxValue) {
                maxValue = point.getAvg();
            }
        }

        return maxValue;
    }

    /**
     * @return the dataFrequency
     */
    public int getDataFrequency() {
        return dataFrequency;
    }

    /**
     * @param dataFrequency
     *            the dataFrequency to set
     */
    public void setDataFrequency(int dataFrequency) {
        this.dataFrequency = dataFrequency;
    }

    /**
     * Get the list of DataPoint objects for the key.
     * 
     * @param key
     *            The key
     * @return List of DataPoint objects
     */
    public List<DataPoint> getData() {
        Collections.sort(pointList);
        return pointList;
    }

    /**
     * @return the timeRange
     */
    public TimeRange getTimeRange() {
        return timeRange;
    }

    /**
     * @param pointList
     *            the pointList to set
     */
    public void setPointList(List<DataPoint> pointList) {
        this.pointList = pointList;
    }

    /**
     * Add an AggregateRecord.
     * 
     * @param rec
     *            the record to add
     */
    public void addRecord(AggregateRecord rec) {
        this.recordList.add(rec);
    }

    /**
     * Get the key
     * 
     * @return
     */
    public String getKey() {
        return key;
    }

    /**
     * Set the key
     * 
     * @param key
     *            the key to set
     */
    public void setKey(String key) {
        this.key = key;
    }

    /**
     * Set the StatsBin object map.
     * 
     * @param bins
     */
    public void setBins(Map<Long, StatsBin> bins) {
        this.bins = bins;
    }

    /**
     * Accumulates the AggregateRecord objects into the correct bins.
     */
    public void accumulate() {
        pointList.clear();
        for (AggregateRecord record : recordList) {
            Date startDate = record.getStartDate().getTime();

            long start = startDate.getTime();
            long bin = getBinKey(start);
            if (bins.get(bin) != null) {
                bins.get(bin).addData(record);
            }
        }

        createPoints();
    }

    /**
     * Create the points for this key.
     * 
     * @param dataKey
     */
    private void createPoints() {
        // Bins are created, now make the graph group member and point objects
        for (long key : bins.keySet()) {
            StatsBin sb = bins.get(key);
            List<AggregateRecord> dataList = sb.getData();
            if (!dataList.isEmpty()) {
                DataPoint point = new DataPoint();
                point.setX(sb.getBinMillis());

                for (AggregateRecord rec : dataList) {
                    // Check for an existing point object
                    point.setMax(rec.getMax());
                    point.setMin(rec.getMin());
                    point.setSum(rec.getSum());
                    point.addToCount(rec.getCount());
                }

                pointList.add(point);
            }
        }
    }

    /**
     * Get the bin key for the given millisecond value.
     * 
     * @param millis
     *            The millisecond value
     * @return The bin that should hold this millisecond value
     */
    private long getBinKey(long millis) {
        for (long bin : this.bins.keySet()) {
            if (millis <= bins.get(bin).getBinMillis()) {
                return bin;
            }
        }

        return 0;
    }

    /**
     * @return the recordList
     */
    public List<AggregateRecord> getRecordList() {
        return recordList;
    }

    /**
     * @return the pointList
     */
    public List<DataPoint> getPointList() {
        return pointList;
    }

    /**
     * @return the bins
     */
    public Map<Long, StatsBin> getBins() {
        return bins;
    }

    /**
     * @param timeRange
     *            the timeRange to set
     */
    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }
}
