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
package com.raytheon.uf.viz.d2d.xy.adapters.varheight;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.measure.unit.Unit;

import org.geotools.geometry.DirectPosition2D;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.xy.InterpUtils;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.grid.inv.GridInventory;

/**
 * Adapter for Grid data that returns the value of a variable at multiple
 * heights.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridVarHeightAdapter extends AbstractVarHeightAdapter<GridRecord> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridVarHeightAdapter.class);

    protected HashMap<DataTime, Set<GridRecord>> yRecordMap = new HashMap<DataTime, Set<GridRecord>>();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#getParamterName
     * ()
     */
    @Override
    public String getParameterName() {
        synchronized (records) {
            String name = ((GridRecord) records.iterator().next())
                    .getParameter().getName();
            if (name == null || name.isEmpty()) {
                name = ((GridRecord) records.iterator().next()).getParameter()
                        .getAbbreviation();
            }
            return name;

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#getXUnits()
     */
    @Override
    public Unit<?> getXUnit() {
        synchronized (records) {
            if (records == null || records.size() == 0) {
                return null;
            }
            return ((GridRecord) records.iterator().next()).getParameter()
                    .getUnit();

        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#getYUnits()
     */
    @Override
    public Unit<?> getYUnit() {
        if (yRecordMap.isEmpty()) {
            return null;
        }

        Set<DataTime> keys = yRecordMap.keySet();
        for (DataTime key : keys) {
            Set<GridRecord> aRecord = yRecordMap.get(key);
            if (!aRecord.isEmpty()) {
                return ((GridRecord) aRecord.iterator().next()).getParameter()
                        .getUnit();
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#loadData(
     * com.raytheon.uf.common.time.DataTime)
     */
    @Override
    public List<XYData> loadData(DataTime currentTime) throws VizException {
        populateYRecords();

        ArrayList<GridRecord> hasData = new ArrayList<GridRecord>(
                records.size());

        Map<Level, GridRecord> xMap = new HashMap<Level, GridRecord>();
        synchronized (records) {

            for (GridRecord rec : records) {
                if (rec.getDataTime().equals(currentTime)) {
                    xMap.put(rec.getLevel(), rec);
                    if (rec.getMessageData() != null) {
                        hasData.add(rec);
                    }
                }
            }
        }
        Map<Level, GridRecord> yMap = new HashMap<Level, GridRecord>();

        synchronized (yRecordMap) {

            Set<GridRecord> yRecords = yRecordMap.get(currentTime);
            if (yRecords != null) {
                for (GridRecord rec : yRecords) {
                    yMap.put(rec.getLevel(), rec);
                    if (rec.getMessageData() != null) {
                        hasData.add(rec);
                    }
                }
            }

        }

        xMap.keySet().retainAll(yMap.keySet());
        yMap.keySet().retainAll(xMap.keySet());

        ArrayList<XYData> dataList = new ArrayList<XYData>(xMap.size());

        if (xMap.isEmpty()) {
            return dataList;
        }

        Map<GridCoverage, List<PluginDataObject>> recordsByLocation = new HashMap<GridCoverage, List<PluginDataObject>>(
                4);
        for (GridRecord record : xMap.values()) {
            if (hasData.contains(record)) {
                continue;
            }
            List<PluginDataObject> list = recordsByLocation.get(record
                    .getLocation());
            if (list == null) {
                list = new ArrayList<PluginDataObject>(xMap.size() * 2);
                recordsByLocation.put(record.getLocation(), list);
            }
            list.add(record);
        }

        for (GridRecord record : yMap.values()) {
            if (hasData.contains(record)) {
                continue;
            }
            List<PluginDataObject> list = recordsByLocation.get(record
                    .getLocation());
            if (list == null) {
                list = new ArrayList<PluginDataObject>(yMap.size());
                recordsByLocation.put(record.getLocation(), list);
            }
            list.add(record);
        }

        for (Entry<GridCoverage, List<PluginDataObject>> entry : recordsByLocation
                .entrySet()) {
            Request request = getRequest(entry.getKey());
            if (request == null) {
                continue;
            }
            DataCubeContainer.getDataRecords(entry.getValue(), request, null);

        }

        for (Level level : xMap.keySet()) {
            GridRecord yRecord = yMap.get(level);

            FloatDataRecord yRec = (FloatDataRecord) ((IDataRecord[]) yRecord
                    .getMessageData())[0];
            DirectPosition2D yPoint = getPoint(yRecord.getLocation());
            Rectangle yRect = getRectangle(yRecord.getLocation());
            float yVal = InterpUtils.getInterpolatedData(yRect, yPoint.x,
                    yPoint.y, yRec.getFloatData());
            if (yVal <= -9999) {
                continue;
            }
            GridRecord xRecord = xMap.get(level);
            DirectPosition2D xPoint = getPoint(xRecord.getLocation());
            Rectangle xRect = getRectangle(xRecord.getLocation());
            IDataRecord[] results = ((IDataRecord[]) xRecord.getMessageData());
            if (results == null) {
                continue;
            } else if (results.length == 4) {
                FloatDataRecord speedRec = (FloatDataRecord) results[0];
                FloatDataRecord dirRec = (FloatDataRecord) results[1];
                float speed = InterpUtils.getInterpolatedData(xRect, xPoint.x,
                        xPoint.y, speedRec.getFloatData());
                float dir = InterpUtils.getInterpolatedData(xRect, xPoint.x,
                        xPoint.y, dirRec.getFloatData());
                dataList.add(new XYWindImageData(speed, yVal, speed, dir));
            } else {
                FloatDataRecord xRec = (FloatDataRecord) results[0];
                float xVal = InterpUtils.getInterpolatedData(xRect, xPoint.x,
                        xPoint.y, xRec.getFloatData());
                if (xVal <= -9999) {
                    continue;
                }
                dataList.add(new XYData(xVal, yVal));
            }
        }

        if (dataList.isEmpty()) {
            statusHandler.handle(Priority.INFO, "No data found for point "
                    + resourceData.getPoint() + ", at time " + currentTime
                    + ", please verify point is within bounds of the data.");
            return Collections.emptyList();
        }

        return dataList;
    }

    /**
     * Retrive the coordinate in grid space(defined by location) where this
     * adapter is expected to load data.
     * 
     * @param location
     * @return
     * @throws VizException
     */
    private DirectPosition2D getPoint(GridCoverage location)
            throws VizException {
        try {
            return PointUtil.determineExactIndex(resourceData.getPoint(),
                    location.getCrs(), location.getGridGeometry());
        } catch (Exception e) {
            throw new VizException(e);
        }

    }

    /**
     * find the rectangle in grid space(defined by location) where this adapter
     * should request data, the rectangle is formed by adding enough space
     * around the point to perform bilinear interpolation.
     * 
     * @param location
     * @return
     * @throws VizException
     */
    private Rectangle getRectangle(GridCoverage location) throws VizException {
        DirectPosition2D point = getPoint(location);
        Rectangle rectangle = new Rectangle((int) Math.floor(point.x),
                (int) Math.floor(point.y), 2, 2);
        rectangle = rectangle.intersection(new Rectangle(0, 0,
                location.getNx(), location.getNy()));
        return rectangle;
    }

    /**
     * Find the datastore request needed to get data for GridRecords with the
     * provided location.
     * 
     * @param location
     * @return
     * @throws VizException
     */
    private Request getRequest(GridCoverage location) throws VizException {
        Rectangle rectangle = getRectangle(location);
        if (rectangle.isEmpty()) {
            return null;
        }
        return Request.buildSlab(
                new int[] { (int) rectangle.getMinX(),
                        (int) rectangle.getMinY() },
                new int[] { (int) rectangle.getMaxX(),
                        (int) rectangle.getMaxY() });
    }

    public void addRecord(PluginDataObject pdo) {
        DataTime key = pdo.getDataTime().clone();
        key.setLevelValue(null);

        // clear the y records for this time only
        synchronized (yRecordMap) {
            // if it has the time clear the set so it will be reloaded
            yRecordMap.remove(key);
        }

        super.addRecord(pdo);
    }

    public void remove(DataTime time) {
        DataTime key = time.clone();
        key.setLevelValue(null);
        synchronized (yRecordMap) {
            // remove all from this time
            yRecordMap.remove(key);
        }
        super.remove(time);
    }

    private void populateYRecords() throws VizException {
        int size = records.size() - yRecordMap.size();
        size = (size > 4 ? size : 4);
        Set<DataTime> times = new HashSet<DataTime>(size);
        synchronized (records) {
            synchronized (yRecordMap) {
                for (PluginDataObject rec : records) {
                    DataTime key = rec.getDataTime();
                    if (!yRecordMap.containsKey(key)) {
                        times.add(key);
                    }
                }
            }
        }

        if (times.size() > 0) {
            Map<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>(
                    resourceData.getMetadataMap());
            metadataMap.put(GridInventory.PARAMETER_QUERY,
                    new RequestConstraint(heightScale.getParameter()));

            LayerProperty property = new LayerProperty();
            property.setDesiredProduct(ResourceType.PLAN_VIEW);

            property.setEntryQueryParameters(metadataMap, false);
            property.setNumberOfImages(9999);
            property.setSelectedEntryTimes(times.toArray(new DataTime[times
                    .size()]));

            List<Object> recs = DataCubeContainer.getData(property, 60000);

            for (Object obj : recs) {
                GridRecord gRecord = (GridRecord) obj;
                Set<GridRecord> recordSet = yRecordMap.get(gRecord
                        .getDataTime());
                if (recordSet != null) {
                    recordSet.add(gRecord);
                } else {
                    recordSet = new HashSet<GridRecord>();
                    recordSet.add(gRecord);
                    yRecordMap.put(gRecord.getDataTime(), recordSet);
                }
            }
        }
    }

    public void setHeightScale(HeightScale heightScale) {
        if (this.heightScale != null
                && !this.heightScale.getParameter().equals(
                        heightScale.getParameter())) {
            synchronized (yRecordMap) {
                yRecordMap.clear();
            }
        }
        super.setHeightScale(heightScale);
    }

}
