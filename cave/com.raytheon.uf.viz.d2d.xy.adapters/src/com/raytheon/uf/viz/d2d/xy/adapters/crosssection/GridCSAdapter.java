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
import java.util.Map.Entry;
import java.util.Set;

import javax.measure.Unit;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.xy.InterpUtils;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.uf.viz.xy.crosssection.display.CrossSectionDescriptor;
import com.raytheon.uf.viz.xy.crosssection.graph.CrossSectionGraph;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.map.GeoUtil;

/**
 * Adapter providing CrossSections of grid data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ------------ ----------------------------------------
 * Nov 23, 2009           mschenke     Initial creation
 * Feb 04, 2011  7953     bkowal       Fill values will now be placed in the 
 *                                     data array for anything below 300MB for 
 *                                     RUC80.
 * Oct 02, 2012  15259    M.Porricelli Allow plotting when 3 levels available
 *                                     (DGEX)
 * Sep 09, 2013  2277     mschenke     Got rid of ScriptCreator references
 * Feb 17, 2014  2661     bsteffen     Remove unnecessary output.
 * Apr 03, 2018  6625     bsteffen     Do not make cross section with only two levels.
 * Apr 15, 2019  7596     lsingh       Upgraded javax.measure to JSR-363. Handled unit
 *                                     conversion.
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class GridCSAdapter extends AbstractCrossSectionAdapter<GridRecord> {

    private static final long serialVersionUID = 1L;

    private static final int MINIMUM_LEVELS = Integer
            .getInteger("crosssection.min.level.count", 4);

    protected String yParameter = null;

    protected Map<DataTime, Set<GridRecord>> yRecords = new HashMap<>();

    private Unit<?> unit;

    private CoordinateReferenceSystem crs;

    private Map<GridCoverage, Map<DataTime, Rectangle>> rectangleCache = new HashMap<>(
            4);

    @Override
    public String getParameterName() {
        String name = null;
        if (records != null && !records.isEmpty()) {
            name = records.get(0).getParameter().getName();
            if (name == null || name.isEmpty()) {
                name = records.get(0).getParameter().getAbbreviation();
            }
        }
        return name;
    }

    @Override
    public Unit<?> getUnit() {
        return unit;
    }

    @Override
    public List<float[]> loadData(DataTime currentTime, CrossSectionGraph graph,
            GridGeometry2D geometry) throws VizException {
        DataTime recordTime = currentTime.clone();
        recordTime.setLevelValue(null);
        Set<GridRecord> yRecords = getYRecords(recordTime);
        Map<Level, GridRecord> xMap = new HashMap<>();

        synchronized (records) {
            for (GridRecord rec : records) {
                if (rec.getDataTime().equals(recordTime)) {
                    xMap.put(rec.getLevel(), rec);
                }
            }
        }

        if (xMap.size() <= MINIMUM_LEVELS) {
            return null;
        }

        Map<Level, GridRecord> yMap = new HashMap<>();
        for (GridRecord rec : yRecords) {
            if (rec.getDataTime().equals(recordTime)) {
                yMap.put(rec.getLevel(), rec);
            }
        }

        xMap.keySet().retainAll(yMap.keySet());
        yMap.keySet().retainAll(xMap.keySet());

        if (xMap.size() <= MINIMUM_LEVELS) {
            return null;
        }

        int nx = (int) geometry.getGridRange2D().getWidth();
        int ny = (int) geometry.getGridRange2D().getHeight();
        Map<GridCoverage, List<PluginDataObject>> recordsByLocation = new HashMap<>();
        for (GridRecord record : xMap.values()) {
            List<PluginDataObject> list = recordsByLocation
                    .get(record.getLocation());
            if (list == null) {
                list = new ArrayList<>();
                recordsByLocation.put(record.getLocation(), list);
            }
            list.add(record);
        }

        for (GridRecord record : yMap.values()) {
            List<PluginDataObject> list = recordsByLocation
                    .get(record.getLocation());
            if (list == null) {
                list = new ArrayList<>();
                recordsByLocation.put(record.getLocation(), list);
            }
            list.add(record);
        }
        for (Entry<GridCoverage, List<PluginDataObject>> entry : recordsByLocation
                .entrySet()) {
            Request request = getRequest(entry.getKey(), currentTime, geometry);
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
        Coordinate[] coordinates = GeoUtil.splitLine(nx,
                descriptor.getLine(currentTime).getCoordinates());
        List<float[]> result = new ArrayList<>();
        for (int i = 0; i < nx; i++) {
            List<List<XYData>> dataLists = new ArrayList<>(result.size());
            for (Level level : xMap.keySet()) {
                GridRecord yRecord = yMap.get(level);
                FloatDataRecord yRec = (FloatDataRecord) (((IDataRecord[]) yRecord
                        .getMessageData())[0]);
                GridRecord xRecord = xMap.get(level);
                IDataRecord[] results = (IDataRecord[]) xRecord
                        .getMessageData();
                DirectPosition2D yPoint = null;
                try {
                    yPoint = PointUtil.determineExactIndex(coordinates[i],
                            yRecord.getLocation().getCrs(),
                            MapUtil.getGridGeometry(yRecord.getLocation()));
                } catch (Exception e) {
                    throw new VizException(e);
                }
                Rectangle yRect = getRectangle(yRecord.getLocation(),
                        currentTime, geometry);
                if (!yRect.contains(yPoint)) {
                    continue;
                }
                float yVal = InterpUtils.getInterpolatedData(yRect, yPoint.x,
                        yPoint.y, yRec.getFloatData());
                yVal = (float) UnitConv
                        .getConverterToUnchecked(
                                yRecord.getParameter().getUnit(),
                                descriptor.getHeightScale().getParameterUnit())
                        .convert(yVal);
                if (yVal <= -9999) {
                    continue;
                }
                while (dataLists.size() < results.length) {
                    dataLists.add(new ArrayList<XYData>());
                }
                DirectPosition2D xPoint = null;
                try {
                    xPoint = PointUtil.determineExactIndex(coordinates[i],
                            xRecord.getLocation().getCrs(),
                            MapUtil.getGridGeometry(xRecord.getLocation()));
                } catch (Exception e) {
                    throw new VizException(e);
                }
                Rectangle xRect = getRectangle(xRecord.getLocation(),
                        currentTime, geometry);
                if (!xRect.contains(xPoint)) {
                    continue;
                }
                for (int c = 0; c < results.length; c++) {
                    FloatDataRecord xRec = (FloatDataRecord) results[c];
                    float xVal = InterpUtils.getInterpolatedData(xRect,
                            xPoint.x, xPoint.y, xRec.getFloatData());
                    if (xVal <= -9999) {
                        continue;
                    }
                    dataLists.get(c).add(new XYData(xVal, yVal));
                }
            }
            while (result.size() < dataLists.size()) {
                float[] floatData = new float[nx * ny];
                Arrays.fill(floatData, -999_999);
                result.add(floatData);
            }
            for (int c = 0; c < dataLists.size(); c++) {
                List<XYData> dataList = dataLists.get(c);
                float[] floatData = result.get(c);
                float[] column = InterpUtils.makeColumn(dataList, ny, graph,
                        descriptor.getHeightScale().getMinVal() < descriptor
                                .getHeightScale().getMaxVal(),
                        -999999f);

                for (int j = 0; j < column.length; j++) {
                    floatData[j * nx + i] = column[j];
                }
            }
        }
        return result;
    }

    private Rectangle getRectangle(GridCoverage location, DataTime time,
            GridGeometry2D geometry) throws VizException {
        Map<DataTime, Rectangle> timeCache = rectangleCache.get(location);
        if (timeCache == null) {
            timeCache = new HashMap<>();
            rectangleCache.put(location, timeCache);
        }
        Rectangle rectangle = timeCache.get(time);
        if (rectangle == null) {
            Coordinate[] coordinates = GeoUtil.splitLine(
                    geometry.getGridRange2D().width,
                    descriptor.getLine(time).getCoordinates());

            for (Coordinate c : coordinates) {
                DirectPosition2D point = null;
                try {
                    point = PointUtil.determineExactIndex(c, location.getCrs(),
                            MapUtil.getGridGeometry(location));
                } catch (Exception e) {
                    throw new VizException(e);
                }
                if (rectangle == null) {
                    rectangle = new Rectangle((int) Math.floor(point.x),
                            (int) Math.floor(point.y), 1, 1);
                } else {
                    rectangle.add(point);
                }
            }

            rectangle.height += 1;
            rectangle.width += 1;
            rectangle = rectangle.intersection(
                    new Rectangle(0, 0, location.getNx(), location.getNy()));
            timeCache.put(time, rectangle);
        }

        return rectangle;
    }

    private Request getRequest(GridCoverage location, DataTime time,
            GridGeometry2D geometry) throws VizException {
        Rectangle rectangle = getRectangle(location, time, geometry);
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
        super.addRecord(pdo);
        if (pdo != null && pdo instanceof GridRecord) {
            unit = ((GridRecord) pdo).getParameter().getUnit();
            crs = ((GridRecord) pdo).getSpatialObject().getCrs();
        }
        yRecords.remove(pdo.getDataTime());
    }

    @Override
    public void remove(DataTime time) {
        yRecords.remove(time);
        super.remove(time);
    }

    private Set<GridRecord> getYRecords(DataTime time) throws VizException {
        synchronized (yRecords) {
            Set<GridRecord> yRecords = this.yRecords.get(time);
            if (yRecords != null) {
                return yRecords;
            }
            if (yParameter == null) {
                yParameter = descriptor.getHeightScale().getParameter();
            }
            Map<String, RequestConstraint> metadataMap = new HashMap<>(
                    resourceData.getMetadataMap());
            metadataMap.put(GridConstants.PARAMETER_ABBREVIATION,
                    new RequestConstraint(
                            descriptor.getHeightScale().getParameter()));

            PluginDataObject[] pdos;
            try {
                pdos = DataCubeContainer.getData(metadataMap, time);
            } catch (DataCubeException e) {
                throw new VizException(e);
            }
            yRecords = new HashSet<>(pdos.length);
            for (PluginDataObject pdo : pdos) {
                yRecords.add((GridRecord) pdo);
            }
            this.yRecords.put(time, yRecords);
            return yRecords;
        }
    }

    @Override
    public void setDescriptor(CrossSectionDescriptor descriptor) {
        super.setDescriptor(descriptor);
        if (yParameter == null || !yParameter
                .equals(descriptor.getHeightScale().getParameter())) {
            yParameter = descriptor.getHeightScale().getParameter();
            yRecords.clear();
        }
    }

    @Override
    public CoordinateReferenceSystem getDataCoordinateReferenceSystem() {
        if (crs == null) {
            return super.getDataCoordinateReferenceSystem();
        }
        return crs;
    }

}
