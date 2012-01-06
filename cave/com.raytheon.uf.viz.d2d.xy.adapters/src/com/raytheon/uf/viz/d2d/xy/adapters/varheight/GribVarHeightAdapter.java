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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.unit.Unit;

import org.geotools.geometry.DirectPosition2D;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
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
 * TODO Add Description
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

public class GribVarHeightAdapter extends AbstractVarHeightAdapter<GribRecord> {

    protected HashMap<DataTime, Set<GribRecord>> yRecordMap = new HashMap<DataTime, Set<GribRecord>>();

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
            String name = ((GribRecord) records.iterator().next())
                    .getModelInfo().getParameterName();
            if (name == null || name.isEmpty()) {
                name = ((GribRecord) records.iterator().next()).getModelInfo()
                        .getParameterAbbreviation();
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
            return ((GribRecord) records.iterator().next()).getModelInfo()
                    .getParameterUnitObject();

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
            Set<GribRecord> aRecord = yRecordMap.get(key);
            if (!aRecord.isEmpty()) {
                return ((GribRecord) aRecord.iterator().next()).getModelInfo()
                        .getParameterUnitObject();
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

        ArrayList<GribRecord> needsRequested = new ArrayList<GribRecord>();

        Map<Level, GribRecord> xMap = new HashMap<Level, GribRecord>();
        ISpatialObject area = null;
        synchronized (records) {

            for (GribRecord rec : records) {
                area = rec.getSpatialObject();
                if (rec.getDataTime().equals(currentTime)) {
                    xMap.put(rec.getModelInfo().getLevel(), rec);
                    if (rec.getMessageData() == null) {
                        needsRequested.add(rec);
                    }
                }
            }
        }
        Map<Level, GribRecord> yMap = new HashMap<Level, GribRecord>();

        synchronized (yRecordMap) {

            Set<GribRecord> yRecords = yRecordMap.get(currentTime);

            if (yRecords != null) {
                for (GribRecord rec : yRecords) {
                    yMap.put(rec.getModelInfo().getLevel(), rec);
                    if (rec.getMessageData() == null) {
                        needsRequested.add(rec);
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

        DirectPosition2D point = null;
        try {
            point = PointUtil.determineExactIndex(resourceData.getPoint(),
                    area.getCrs(), MapUtil.getGridGeometry(area));
        } catch (Exception e) {
            throw new VizException(e);
        }
        Rectangle requestArea = new Rectangle((int) Math.floor(point.x),
                (int) Math.floor(point.y), 2, 2);
        requestArea = requestArea.intersection(new Rectangle(0, 0,
                area.getNx(), area.getNy()));
        if (requestArea.isEmpty()) {
            throw new VizException("Invalid point position("
                    + resourceData.getPoint()
                    + "). Check that the point is within the grib boundaries.");
        }
        Request request = Request.buildSlab(
                new int[] { (int) requestArea.getMinX(),
                        (int) requestArea.getMinY() },
                new int[] { (int) requestArea.getMaxX(),
                        (int) requestArea.getMaxY() });

        List<PluginDataObject> pdos = new ArrayList<PluginDataObject>(
                xMap.size() * 2);
        pdos.addAll(xMap.values());
        pdos.addAll(yMap.values());

        // only request pdos without data
        pdos.retainAll(needsRequested);

        DataCubeContainer.getDataRecords(pdos, request, null);

        for (Level level : xMap.keySet()) {
            GribRecord yRecord = yMap.get(level);

            FloatDataRecord yRec = (FloatDataRecord) ((IDataRecord[]) yRecord
                    .getMessageData())[0];
            float yVal = InterpUtils.getInterpolatedData(requestArea, point.x,
                    point.y, yRec.getFloatData());
            if (yVal <= -9999) {
                continue;
            }
            GribRecord xRecord = xMap.get(level);
            IDataRecord[] results = ((IDataRecord[]) xRecord.getMessageData());
            if (results.length == 4) {
                FloatDataRecord speedRec = (FloatDataRecord) results[0];
                FloatDataRecord dirRec = (FloatDataRecord) results[1];
                float speed = InterpUtils.getInterpolatedData(requestArea,
                        point.x, point.y, speedRec.getFloatData());
                float dir = InterpUtils.getInterpolatedData(requestArea,
                        point.x, point.y, dirRec.getFloatData());
                dataList.add(new XYWindImageData(speed, yVal, speed, dir));
            } else {
                FloatDataRecord xRec = (FloatDataRecord) results[0];
                float xVal = InterpUtils.getInterpolatedData(requestArea,
                        point.x, point.y, xRec.getFloatData());
                if (xVal <= -9999) {
                    continue;
                }
                dataList.add(new XYData(xVal, yVal));
            }
        }

        return dataList;
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
                GribRecord gRecord = (GribRecord) obj;
                Set<GribRecord> recordSet = yRecordMap.get(gRecord
                        .getDataTime());
                if (recordSet != null) {
                    recordSet.add(gRecord);
                } else {
                    recordSet = new HashSet<GribRecord>();
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
