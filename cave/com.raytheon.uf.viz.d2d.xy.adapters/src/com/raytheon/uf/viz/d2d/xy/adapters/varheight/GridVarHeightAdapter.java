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

import javax.measure.Unit;

import org.geotools.geometry.DirectPosition2D;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.xy.InterpUtils;
import com.raytheon.uf.viz.xy.scales.HeightScale;
import com.raytheon.uf.viz.xy.varheight.adapter.AbstractVarHeightAdapter;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.graphing.xy.XYWindImageData;

/**
 * Adapter for Grid data that returns the value of a variable at multiple
 * heights.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May  7, 2010           bsteffen    Initial creation
 * Sep  9, 2013  2277     mschenke    Got rid of ScriptCreator references
 * Feb 17, 2014  2661     bsteffen    Use only u,v for vectors.
 * Feb 06, 2018  6829     njensen     Check for wind
 * 
 * </pre>
 * 
 * @author bsteffen
 */

public class GridVarHeightAdapter extends AbstractVarHeightAdapter<GridRecord> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridVarHeightAdapter.class);

    protected Map<DataTime, Set<GridRecord>> yRecordMap = new HashMap<>();

    @Override
    public String getParameterName() {
        synchronized (records) {
            String name = records.iterator().next()
                    .getParameter().getName();
            if (name == null || name.isEmpty()) {
                name = records.iterator().next().getParameter()
                        .getAbbreviation();
            }
            return name;

        }
    }

    @Override
    public Unit<?> getXUnit() {
        synchronized (records) {
            if (records == null || records.size() == 0) {
                return null;
            }
            return records.iterator().next().getParameter()
                    .getUnit();

        }
    }

    @Override
    public Unit<?> getYUnit() {
        if (yRecordMap.isEmpty()) {
            return null;
        }

        Set<DataTime> keys = yRecordMap.keySet();
        for (DataTime key : keys) {
            Set<GridRecord> aRecord = yRecordMap.get(key);
            if (!aRecord.isEmpty()) {
                return aRecord.iterator().next().getParameter()
                        .getUnit();
            }
        }
        return null;
    }

    @Override
    public List<XYData> loadData(DataTime currentTime) throws VizException {
        populateYRecords();

        List<GridRecord> hasData = new ArrayList<>(
                records.size());

        Map<Level, GridRecord> xMap = new HashMap<>();
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
        Map<Level, GridRecord> yMap = new HashMap<>();

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

        List<XYData> dataList = new ArrayList<>(xMap.size());

        if (xMap.isEmpty()) {
            return dataList;
        }

        Map<GridCoverage, List<PluginDataObject>> recordsByLocation = new HashMap<>(
                4);
        for (GridRecord record : xMap.values()) {
            if (hasData.contains(record)) {
                continue;
            }
            List<PluginDataObject> list = recordsByLocation.get(record
                    .getLocation());
            if (list == null) {
                list = new ArrayList<>(xMap.size() * 2);
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
                list = new ArrayList<>(yMap.size());
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
            try {
                DataCubeContainer.getDataRecords(entry.getValue(), request,
                        null);
            } catch (DataCubeException e) {
                throw new VizException(e);
            }

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
            } else if (results.length == 2) {
                wind = true;
                FloatDataRecord uRec = (FloatDataRecord) results[0];
                FloatDataRecord vRec = (FloatDataRecord) results[1];
                float u = InterpUtils.getInterpolatedData(xRect, xPoint.x,
                        xPoint.y, uRec.getFloatData());
                float v = InterpUtils.getInterpolatedData(xRect, xPoint.x,
                        xPoint.y, vRec.getFloatData());

                double speed = Math.hypot(u, v);
                double dir = Math.toDegrees(Math.atan2(-u, -v));

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

    @Override
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

    @Override
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
        Set<DataTime> times = new HashSet<>(size);
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

        if (!times.isEmpty()) {
            Map<String, RequestConstraint> metadataMap = new HashMap<>(
                    resourceData.getMetadataMap());
            metadataMap.put(GridConstants.PARAMETER_ABBREVIATION,
                    new RequestConstraint(heightScale.getParameter()));

            PluginDataObject[] pdos;
            try {
                pdos = DataCubeContainer.getData(metadataMap,
                        times.toArray(new DataTime[0]));
            } catch (DataCubeException e) {
                throw new VizException(e);
            }
            for (PluginDataObject pdo : pdos) {
                GridRecord gRecord = (GridRecord) pdo;
                Set<GridRecord> recordSet = yRecordMap.get(gRecord
                        .getDataTime());
                if (recordSet != null) {
                    recordSet.add(gRecord);
                } else {
                    recordSet = new HashSet<>();
                    recordSet.add(gRecord);
                    yRecordMap.put(gRecord.getDataTime(), recordSet);
                }
            }
        }
    }

    @Override
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
