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
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AggregateRequestableData;
import com.raytheon.uf.viz.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpaceMatcher;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpaceMatcher.MatchResult;
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

    public TimeRangeLevelNode(AbstractRequestableNode sourceNode,
            DerivParamDesc desc, DerivParamMethod method, String modelName,
            Integer startTime, int endTime, int dt, Level level) {
        super(sourceNode, desc, method, modelName, level);
        this.startTime = startTime;
        this.endTime = endTime;
        this.dt = dt;
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        TimeAndSpaceMatcher matcher = new TimeAndSpaceMatcher();
        matcher.setIgnoreRange(true);
        Map<TimeAndSpace, AbstractRequestableData> dataMap = new HashMap<TimeAndSpace, AbstractRequestableData>();
        for (AbstractRequestableData data : dependencyData.get(sourceNode)) {
            dataMap.put(data.getTimeAndSpace(), data);
        }
        Set<AbstractRequestableData> records = new HashSet<AbstractRequestableData>();
        for (TimeAndSpace ast : availability) {
            Set<TimeAndSpace> needed = calculateNeededAvailability(ast);
            Map<TimeAndSpace, MatchResult> matched = matcher.match(needed,
                    dataMap.keySet());
            if (TimeAndSpaceMatcher.getAll1(matched).containsAll(needed)) {
                List<AbstractRequestableData> dataList = new ArrayList<AbstractRequestableData>();
                for (TimeAndSpace dataTime : TimeAndSpaceMatcher
                        .getAll2(matched)) {
                    dataList.add(dataMap.get(dataTime));

                }
                AggregateRequestableData newRecord = new AggregateRequestableData(
                        dataList);
                newRecord.setDataTime(ast.getTime());
                newRecord.setSpace(ast.getSpace());
                modifyRequest(newRecord);
                records.add(newRecord);
            }
        }
        return records;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws VizException {
        TimeAndSpaceMatcher matcher = new TimeAndSpaceMatcher();
        matcher.setIgnoreRange(true);
        Set<TimeAndSpace> allAvail = availability.get(sourceNode);
        Set<TimeAndSpace> goodAvail = new HashSet<TimeAndSpace>();
        for (TimeAndSpace ast : allAvail) {
            Set<TimeAndSpace> needed = calculateNeededAvailability(ast);
            Set<TimeAndSpace> matchedNeeded = TimeAndSpaceMatcher
                    .getAll1(matcher.match(needed, allAvail));
            if (matchedNeeded.containsAll(needed)) {
                goodAvail.add(ast);
            }

        }
        return goodAvail;
    }

    @Override
    public Map<AbstractRequestableNode, Set<TimeAndSpace>> getDataDependency(
            Set<TimeAndSpace> availability,
            AvailabilityContainer availabilityContainer) {
        TimeAndSpaceMatcher matcher = new TimeAndSpaceMatcher();
        matcher.setIgnoreRange(true);
        Set<TimeAndSpace> sourceAvailability = new HashSet<TimeAndSpace>(
                availability);
        for (TimeAndSpace ast : availability) {
            Set<TimeAndSpace> needed = calculateNeededAvailability(ast);
            Set<TimeAndSpace> matchedAvail = TimeAndSpaceMatcher
                    .getAll1(matcher.match(needed, availability));
            sourceAvailability.addAll(matchedAvail);

        }
        Map<AbstractRequestableNode, Set<TimeAndSpace>> result = new HashMap<AbstractRequestableNode, Set<TimeAndSpace>>();
        result.put(sourceNode, sourceAvailability);
        return result;
    }

    @Override
    public boolean isConstant() {
        return super.isConstant();
    }

    private Set<TimeAndSpace> calculateNeededAvailability(TimeAndSpace ast) {
        Set<TimeAndSpace> result = new HashSet<TimeAndSpace>();
        int start = dt;
        if (startTime != null) {
            start = ast.getTime().getFcstTime() + this.startTime;
        }
        for (int i = start; i <= ast.getTime().getFcstTime() + this.endTime; i += dt) {
            DataTime time = new DataTime(ast.getTime().getRefTime(), i);
            result.add(new TimeAndSpace(time, ast.getSpace()));
        }
        return result;
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
