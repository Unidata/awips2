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
package com.raytheon.uf.viz.derivparam.tree;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AggregateRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * 
 * A Node which build AggregateRecords containing all the records for a given
 * time range in seconds.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class TimeRangeLevelNode extends AbstractAliasLevelNode {

    private Integer startTime;

    private Integer endTime;

    private int dt;

    public TimeRangeLevelNode(TimeRangeLevelNode that) {
        super(that);
        this.startTime = that.startTime;
        this.endTime = that.endTime;
        this.dt = that.dt;
    }

    public TimeRangeLevelNode(AbstractRequestableLevelNode sourceNode,
            DerivParamDesc desc, DerivParamMethod method, String modelName,
            int startTime, int endTime, int dt, Level level) {
        super(sourceNode, desc, method, modelName, level);
        this.startTime = startTime;
        this.endTime = endTime;
        this.dt = dt;
    }

    public List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        Set<DataTime> allTime = sourceNode.timeQuery(false);
        Map<DataTime, List<DataTime>> goodTimes = new HashMap<DataTime, List<DataTime>>();
        Set<DataTime> timesToRequest = new HashSet<DataTime>();
        for (DataTime time : allTime) {
            List<DataTime> neededTimes = calculateNeededTimes(time);
            if (allTime.containsAll(neededTimes)) {
                goodTimes.put(time, neededTimes);
                timesToRequest.addAll(neededTimes);
            }

        }
        property.setSelectedEntryTimes(timesToRequest
                .toArray(new DataTime[timesToRequest.size()]));
        Map<DataTime, List<AbstractRequestableData>> timeBins = new HashMap<DataTime, List<AbstractRequestableData>>();
        for (AbstractRequestableData record : sourceNode.getData(property,
                timeOut, cache)) {
            for (Entry<DataTime, List<DataTime>> entry : goodTimes.entrySet()) {
                if (entry.getValue().contains(record.getDataTime())) {
                    List<AbstractRequestableData> records = timeBins.get(entry
                            .getKey());
                    if (records == null) {
                        records = new ArrayList<AbstractRequestableData>(entry
                                .getValue().size());
                        timeBins.put(entry.getKey(), records);
                    }
                    records.add(record);
                }
            }
        }
        List<AbstractRequestableData> records = new ArrayList<AbstractRequestableData>();
        for (Entry<DataTime, List<AbstractRequestableData>> entry : timeBins
                .entrySet()) {
            if (entry.getValue().size() == goodTimes.get(entry.getKey()).size()) {
                AggregateRequestableData newRecord = new AggregateRequestableData(
                        entry.getValue());
                newRecord.setDataTime(entry.getKey());
                modifyRequest(newRecord);
                records.add(newRecord);
            }
        }
        return records;
    }

    @Override
    public Set<DataTime> timeQueryInternal(boolean latestOnly,
            Map<AbstractRequestableLevelNode, Set<DataTime>> cache,
            Map<AbstractRequestableLevelNode, Set<DataTime>> latestOnlyCache)
            throws VizException {
        Set<DataTime> allTime = sourceNode.timeQuery(false, cache,
                latestOnlyCache);
        Set<DataTime> goodTimes = new HashSet<DataTime>();
        for (DataTime time : allTime) {
            if (allTime.containsAll(calculateNeededTimes(time))) {
                goodTimes.add(time);
            }

        }
        return goodTimes;
    }

    private List<DataTime> calculateNeededTimes(DataTime time) {
        List<DataTime> times = new ArrayList<DataTime>();
        for (int i = time.getFcstTime() + this.startTime; i <= time
                .getFcstTime() + this.endTime; i += dt) {
            times.add(new DataTime(time.getRefTime(), i));

        }
        return times;
    }

    @Override
    public TimeRangeLevelNode clone() {
        return new TimeRangeLevelNode(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((endTime == null) ? 0 : endTime.hashCode());
        result = prime * result
                + ((startTime == null) ? 0 : startTime.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        TimeRangeLevelNode other = (TimeRangeLevelNode) obj;
        if (endTime == null) {
            if (other.endTime != null)
                return false;
        } else if (!endTime.equals(other.endTime))
            return false;
        if (startTime == null) {
            if (other.startTime != null)
                return false;
        } else if (!startTime.equals(other.startTime))
            return false;
        return true;
    }

}
