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
package com.raytheon.uf.viz.npp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;

/**
 * Abstract resource data for all NPP data. Groups frame times close together
 * (within {@link #groupTimeRangeMinutes} minutes).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2013             mschenke    Initial creation
 * Aug 2, 2013        2190 mschenke    Moved time grouping functions to utility class
 * Aug 27, 2013       2190 mschenke    Fixed groupRecordTimes function
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractNppResourceData extends
        AbstractRequestableResourceData {

    @XmlElement
    private int groupTimeRangeMinutes = 15;

    /**
     * @return the groupTimeRangeMinutes
     */
    public int getGroupTimeRangeMinutes() {
        return groupTimeRangeMinutes;
    }

    /**
     * @param groupTimeRangeMinutes
     *            the groupTimeRangeMinutes to set
     */
    public void setGroupTimeRangeMinutes(int groupTimeRangeMinutes) {
        this.groupTimeRangeMinutes = groupTimeRangeMinutes;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * requestPluginDataObjects(java.util.Collection)
     */
    @Override
    protected PluginDataObject[] requestPluginDataObjects(
            Collection<DataTime> loadSet) throws VizException {
        List<DataTime> timesToLoad = new ArrayList<DataTime>(loadSet);
        Collections.sort(timesToLoad);
        DataTime first = timesToLoad.get(0);
        DataTime last = timesToLoad.get(timesToLoad.size() - 1);
        Map<String, RequestConstraint> requestMap = new HashMap<String, RequestConstraint>(
                getMetadataMap());
        requestMap.put(
                "dataTime.refTime",
                new RequestConstraint(TimeUtil.formatToSqlTimestamp(first
                        .getValidPeriod().getStart()), TimeUtil
                        .formatToSqlTimestamp(last.getValidPeriod().getEnd())));

        PluginDataObject[] pdos = DataCubeContainer.getData(requestMap);
        List<PluginDataObject> finalList = new ArrayList<PluginDataObject>(
                pdos != null ? pdos.length : 0);

        if (pdos != null) {
            for (Object obj : pdos) {
                PluginDataObject pdo = (PluginDataObject) obj;
                for (DataTime dt : loadSet) {
                    if (withinRange(dt.getValidPeriod(), pdo.getDataTime())) {
                        finalList.add(pdo);
                        break;
                    }
                }
            }
        }
        Collections.sort(finalList, layerComparator);
        return finalList.toArray(new PluginDataObject[finalList.size()]);
    }

    public static boolean withinRange(TimeRange range, DataTime time) {
        long refTime = time.getMatchRef();
        return range.getStart().getTime() <= refTime
                && range.getEnd().getTime() >= refTime;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#
     * getAvailableTimes()
     */
    @Override
    public DataTime[] getAvailableTimes() throws VizException {
        Collection<DataTime> dts = groupTimes(Arrays.asList(super
                .getAvailableTimes()));
        return dts.toArray(new DataTime[dts.size()]);
    }

    /**
     * Group times for a {@link PluginDataObject} collection based on
     * {@link #groupTimeRangeMinutes}
     * 
     * @param records
     * @return
     */
    public <T extends PluginDataObject> Map<DataTime, Collection<T>> groupRecordTimes(
            Collection<T> records) {
        Set<DataTime> times = new HashSet<DataTime>();
        for (T record : records) {
            times.add(record.getDataTime());
        }

        Collection<DataTime> groupedTimes = groupTimes(times);
        Map<DataTime, List<T>> grouped = new HashMap<DataTime, List<T>>();
        for (T record : records) {
            for (DataTime time : groupedTimes) {
                if (withinRange(time.getValidPeriod(), record.getDataTime())) {
                    List<T> group = grouped.get(time);
                    if (group == null) {
                        group = new ArrayList<T>();
                        grouped.put(time, group);
                    }
                    group.add(record);
                }
            }
        }
        return new HashMap<DataTime, Collection<T>>(grouped);
    }

    /**
     * Group the {@link DataTime} collection based on
     * {@link #groupTimeRangeMinutes}
     * 
     * @param dataTimes
     * @return
     */
    public Collection<DataTime> groupTimes(Collection<DataTime> dataTimes) {
        return NPPTimeUtility.groupTimes(dataTimes,
                groupTimeRangeMinutes * 60 * 1000);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#update(java
     * .lang.Object)
     */
    @Override
    public void update(Object updateData) {
        super.update(updateData);
        invalidateAvailableTimesCache();
    }
}
