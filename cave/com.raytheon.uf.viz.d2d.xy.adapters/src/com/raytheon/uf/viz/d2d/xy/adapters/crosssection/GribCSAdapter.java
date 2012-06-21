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
package com.raytheon.uf.viz.d2d.xy.adapters.crosssection;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribModel;
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
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.grid.inv.GridInventory;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 23, 2009            mschenke     Initial creation
 * Feb 04, 2011 7953       bkowal       Fill values will now be placed
 *                                      in the data array for anything
 *                                      below 300MB for RUC80.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GribCSAdapter extends AbstractCrossSectionAdapter<GribRecord> {

    private static final long serialVersionUID = 1L;

    protected String yParameter = null;

    protected Map<DataTime, Set<GribRecord>> yRecords = new HashMap<DataTime, Set<GribRecord>>();

    private Unit<?> unit;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#getParamterName
     * ()
     */
    @Override
    public String getParameterName() {
        String name = null;
        if (records != null && !records.isEmpty()) {
            GribModel modelInfo = records.get(0).getModelInfo();
            name = modelInfo.getParameterName();
            if (name == null || name.isEmpty()) {
                name = modelInfo.getParameterAbbreviation();
            }
        }
        return name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#getXUnits()
     */
    @Override
    public Unit<?> getUnit() {
        return unit;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.varheight.adapter.AbstractVarHeightAdapter#loadData(
     * com.raytheon.uf.common.time.DataTime)
     */
    @Override
    public List<float[]> loadData(DataTime currentTime,
            CrossSectionGraph graph, GridGeometry2D geometry)
            throws VizException {
        DataTime recordTime = currentTime.clone();
        recordTime.setLevelValue(null);
        Set<GribRecord> yRecords = getYRecords(recordTime);
        Map<Level, GribRecord> xMap = new HashMap<Level, GribRecord>();

        synchronized (records) {
            for (GribRecord rec : records) {
                if (rec.getDataTime().equals(recordTime)) {
                    xMap.put(rec.getModelInfo().getLevel(), rec);
                }
            }
        }

        if (xMap.size() < 4) {
            return null;
        }

        Map<Level, GribRecord> yMap = new HashMap<Level, GribRecord>();
        for (GribRecord rec : yRecords) {
            if (rec.getDataTime().equals(recordTime)) {
                yMap.put(rec.getModelInfo().getLevel(), rec);
            }
        }

        xMap.keySet().retainAll(yMap.keySet());
        yMap.keySet().retainAll(xMap.keySet());

        if (xMap.size() < 4) {
            return null;
        }

        int nx = (int) geometry.getGridRange2D().getWidth();
        int ny = (int) geometry.getGridRange2D().getHeight();

        ISpatialObject area = records.get(0).getSpatialObject();

        Rectangle requestArea = null;
        Coordinate[] coordinates = GeoUtil.splitLine(nx,
                descriptor.getLine(currentTime).getCoordinates());

        for (Coordinate c : coordinates) {
            DirectPosition2D point = null;
            try {
                point = PointUtil.determineExactIndex(c, area.getCrs(),
                        MapUtil.getGridGeometry(area));
            } catch (Exception e) {
                throw new VizException(e);
            }
            if (requestArea == null) {
                requestArea = new Rectangle((int) Math.floor(point.x),
                        (int) Math.floor(point.y), 1, 1);
            } else {
                requestArea.add(point);
            }
        }

        requestArea.height += 1;
        requestArea.width += 1;
        requestArea = requestArea.intersection(new Rectangle(0, 0,
                area.getNx(), area.getNy()));
        if (requestArea.isEmpty()) {
            throw new VizException(
                    "Invalid line position. Check that the line is within the grib boundaries.");
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

        DataCubeContainer.getDataRecords(pdos, request, null);

        List<float[]> result = new ArrayList<float[]>();
        for (int i = 0; i < nx; i++) {
            DirectPosition2D point = null;
            try {
                point = PointUtil.determineExactIndex(coordinates[i],
                        area.getCrs(), MapUtil.getGridGeometry(area));
            } catch (Exception e) {
                throw new VizException(e);
            }
            if (!requestArea.contains(point)) {
                continue;
            }
            List<List<XYData>> dataLists = new ArrayList<List<XYData>>(
                    result.size());
            for (Level level : xMap.keySet()) {
                GribRecord yRecord = yMap.get(level);
                FloatDataRecord yRec = (FloatDataRecord) (((IDataRecord[]) yRecord
                        .getMessageData())[0]);
                GribRecord xRecord = xMap.get(level);
                IDataRecord[] results = (IDataRecord[]) xRecord
                        .getMessageData();

                float yVal = InterpUtils.getInterpolatedData(requestArea,
                        point.x, point.y, yRec.getFloatData());
                yVal = (float) yRecord
                        .getModelInfo()
                        .getParameterUnitObject()
                        .getConverterTo(
                                descriptor.getHeightScale().getParameterUnit())
                        .convert(yVal);
                if (yVal <= -9999) {
                    continue;
                }
                while (dataLists.size() < results.length) {
                    dataLists.add(new ArrayList<XYData>());
                }
                double speed = Float.NaN;
                double direction = Double.NaN;
                for (int c = 0; c < results.length; c++) {
                    FloatDataRecord xRec = (FloatDataRecord) results[c];
                    float xVal = InterpUtils.getInterpolatedData(requestArea,
                            point.x, point.y, xRec.getFloatData());
                    if (xVal <= -9999) {
                        continue;
                    }
                    // these cases handle rotating a vector to be oriented
                    // towards the north pole rather than the up direction of a
                    // grid.
                    if (c == 0) {
                        speed = xVal;
                    } else if (c == 1) {
                        direction = xVal - 180
                                + MapUtil.rotation(coordinates[i], area);
                        xVal = (float) direction;
                        direction = Math.toRadians(direction);
                    } else if (c == 2) {
                        xVal = (float) (-speed * Math.sin(direction));
                    } else if (c == 3) {
                        xVal = (float) (-speed * Math.cos(direction));
                    }
                    dataLists.get(c).add(new XYData(xVal, yVal));
                }
            }
            while (result.size() < dataLists.size()) {
                float[] floatData = new float[nx * ny];
                Arrays.fill(floatData, -999999);
                result.add(floatData);
            }
            for (int c = 0; c < dataLists.size(); c++) {
                List<XYData> dataList = dataLists.get(c);
                float[] floatData = result.get(c);
                float[] column = InterpUtils.makeColumn(dataList, ny, graph,
                        descriptor.getHeightScale().getMinVal() < descriptor
                                .getHeightScale().getMaxVal(), -999999f);
                for (int j = 0; j < column.length; j++) {
                    floatData[j * nx + i] = column[j];
                }
            }
        }

        return result;
    }

    public void addRecord(PluginDataObject pdo) {
        super.addRecord(pdo);
        if (pdo != null && pdo instanceof GribRecord) {
            unit = ((GribRecord) pdo).getModelInfo().getParameterUnitObject();
        }
        yRecords.remove(pdo.getDataTime());
    }

    public void remove(DataTime time) {
        yRecords.remove(time);
        super.remove(time);
    }

    private Set<GribRecord> getYRecords(DataTime time) throws VizException {
        synchronized (yRecords) {
            Set<GribRecord> yRecords = this.yRecords.get(time);
            if (yRecords != null) {
                return yRecords;
            }
            if (yParameter == null) {
                yParameter = descriptor.getHeightScale().getParameter();
            }
            Map<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>(
                    resourceData.getMetadataMap());
            metadataMap.put(GridInventory.PARAMETER_QUERY,
                    new RequestConstraint(descriptor.getHeightScale()
                            .getParameter()));

            LayerProperty property = new LayerProperty();
            property.setDesiredProduct(ResourceType.PLAN_VIEW);

            property.setEntryQueryParameters(metadataMap, false);
            property.setNumberOfImages(9999);
            property.setSelectedEntryTimes(new DataTime[] { time });

            List<Object> recs = DataCubeContainer.getData(property, 60000);
            yRecords = new HashSet<GribRecord>(recs.size());
            for (Object obj : recs) {
                yRecords.add((GribRecord) obj);
            }
            this.yRecords.put(time, yRecords);
            if (yRecords.isEmpty()) {
                System.out.println("No Y Data");
            }
            return yRecords;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter
     * #setDescriptor
     * (com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor)
     */
    @Override
    public void setDescriptor(CrossSectionDescriptor descriptor) {
        super.setDescriptor(descriptor);
        if (yParameter == null
                || !yParameter.equals(descriptor.getHeightScale()
                        .getParameter())) {
            yParameter = descriptor.getHeightScale().getParameter();
            yRecords.clear();
        }
    }

}
