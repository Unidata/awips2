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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
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
 * Jan 8, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractNppResourceData extends
        AbstractRequestableResourceData {

    private static class DataTimeIterator<T extends PluginDataObject>
            implements Iterator<DataTime> {

        private Iterator<T> pdoIter;

        private T lastAccesed;

        private DataTimeIterator(Iterator<T> pdoIter) {
            this.pdoIter = pdoIter;
        }

        @Override
        public boolean hasNext() {
            return pdoIter.hasNext();
        }

        @Override
        public DataTime next() {
            lastAccesed = pdoIter.next();
            return lastAccesed.getDataTime();
        }

        @Override
        public void remove() {
            pdoIter.remove();
        }
    }

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
        RequestConstraint timeConst = new RequestConstraint();
        timeConst.setConstraintType(ConstraintType.BETWEEN);
        timeConst.setBetweenValueList(new String[] {
                new DataTime(first.getValidPeriod().getStart()).toString(),
                new DataTime(last.getValidPeriod().getEnd()).toString() });
        requestMap.put("dataTime.refTime", timeConst);

        LayerProperty property = new LayerProperty();
        property.setEntryQueryParameters(requestMap, false);
        property.setNumberOfImages(9999);

        List<Object> pdos = DataCubeContainer.getData(property, 60000);
        List<PluginDataObject> finalList = new ArrayList<PluginDataObject>(
                pdos != null ? pdos.size() : 0);

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
            Collections.sort(finalList, layerComparator);
        }
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
        long groupTimeInMillis = groupTimeRangeMinutes * 60 * 1000;
        Map<DataTime, Collection<T>> grouped = new HashMap<DataTime, Collection<T>>();
        Queue<T> objects = new ArrayDeque<T>(records);
        while (objects.size() > 0) {
            T record = objects.remove();
            DataTime current = record.getDataTime();
            TimeRange prev, curr;
            prev = curr = current.getValidPeriod();

            List<T> group = new ArrayList<T>();
            while (curr != null) {
                prev = curr;
                DataTimeIterator<T> iter = new DataTimeIterator<T>(
                        objects.iterator());
                curr = match(iter, prev, groupTimeInMillis);
                if (curr != null) {
                    group.add(iter.lastAccesed);
                }
            }

            grouped.put(new DataTime(prev.getStart().getTime(), prev), group);
        }
        return grouped;
    }

    /**
     * Group the {@link DataTime} collection based on
     * {@link #groupTimeRangeMinutes}
     * 
     * @param dataTimes
     * @return
     */
    public Collection<DataTime> groupTimes(Collection<DataTime> dataTimes) {
        long groupTimeInMillis = groupTimeRangeMinutes * 60 * 1000;
        List<DataTime> grouped = new ArrayList<DataTime>(dataTimes.size());
        Queue<DataTime> objects = new ArrayDeque<DataTime>(dataTimes);
        while (objects.size() > 0) {
            DataTime current = objects.remove();
            TimeRange prev, curr;
            prev = curr = current.getValidPeriod();
            while (curr != null) {
                prev = curr;
                curr = match(objects.iterator(), prev, groupTimeInMillis);
            }

            grouped.add(new DataTime(prev.getStart().getTime(), prev));
        }
        return grouped;
    }

    private TimeRange match(Iterator<DataTime> iter, TimeRange time,
            long groupTimeInMillis) {
        long startT = time.getStart().getTime();
        long endT = time.getEnd().getTime();
        while (iter.hasNext()) {
            DataTime dt = iter.next();
            TimeRange dtRange = dt.getValidPeriod();
            long s = dtRange.getStart().getTime();
            long e = dtRange.getEnd().getTime();
            long startCheck = s - groupTimeInMillis;
            long endCheck = e + groupTimeInMillis;
            if ((startT <= startCheck && endT >= startCheck)
                    || (startCheck <= startT && endCheck >= startT)) {
                iter.remove();
                return new TimeRange(Math.min(s, startT), Math.max(e, endT));
            }
        }
        return null;
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
