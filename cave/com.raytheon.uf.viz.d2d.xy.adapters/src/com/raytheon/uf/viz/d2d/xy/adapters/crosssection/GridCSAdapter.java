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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.measure.Unit;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.LineString;
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
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameData;
import com.raytheon.uf.viz.xy.crosssection.CrossSectionFrameExtraRenderable;
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
 * May 17, 2021  8452     randerso     Clear rectangleCache if line is changed.
 * Oct 29, 2022  8959     mapeters     Update how data time levels are set
 * Nov 03, 2022  8966     mapeters     Prevent NPE when Y records have no data
 * Jan 25, 2023  9001     mapeters     Don't hold onto raw data records after
 *                                     processing them
 * Dec 20, 2023  2036519  mapeters     Add dispose()
 * Jun 20, 2024  2037565  mapeters     Remove unused getParameterName()
 * Jul 03, 2024  2037476  bines        Add getCreatingEntity() override
 * Aug 06, 2024  2037698  bines        Use nearest neighbor for HC data and added
 *                                     getParamterAbbrev()
 * Aug 14, 2024  2037631  mapeters     Wrap float data in new class that includes frame
 *                                     info, extract y interpolation to new method
 * Oct 14, 2024  2037939  mapeters     Extract getMetadataMaps() from loadData()
 *
 * </pre>
 *
 * @author mschenke
 */
public class GridCSAdapter extends AbstractCrossSectionAdapter<GridRecord> {

    private static final long serialVersionUID = 1L;

    private static final int MINIMUM_LEVELS = Integer
            .getInteger("crosssection.min.level.count", 4);

    protected static final float INVALID_VALUE_CUTOFF = -9_999f;

    protected static final float FILL_VALUE = -999_999f;

    protected String yParameter = null;

    protected LineString line = null;

    /**
     * y records are the height scale param records for the same
     * levels/constraints as the main data records. The main records are in
     * {@link #records}, and are the actual data being graphed. These y records
     * are used to determine the y-coord/height of those data values within the
     * graph.
     *
     * Examples: <br>
     * 1) Main records are Temp on MB/pressure levels, height scale is 0-5km
     * MSL, y records are Height MSL on MB levels <br>
     * 2) Main records are Reflectivity on TILT levels, height scale is 0-30kft
     * AGL, y records are Height AGL on TILT levels
     */
    protected final Map<DataTime, Set<GridRecord>> yRecords = new HashMap<>();

    private Unit<?> unit;

    private CoordinateReferenceSystem crs;

    private final Map<GridCoverage, Map<DataTime, Rectangle>> rectangleCache = new HashMap<>(
            4);

    @Override
    public Unit<?> getUnit() {
        return unit;
    }

    @Override
    public CrossSectionFrameData loadData(DataTime frameTime,
            CrossSectionGraph graph, GridGeometry2D geometry)
            throws VizException {
        Pair<Map<Level, GridRecord>, Map<Level, GridRecord>> metadataMaps = getMetadataMaps(
                frameTime);
        if (metadataMaps == null) {
            return null;
        }
        // Data being graphed, e.g. Radar Reflectivity on TILT levels
        Map<Level, GridRecord> xMap = metadataMaps.getLeft();
        // Height scale values along same levels, e.g. Height AGL on TILT levels
        Map<Level, GridRecord> yMap = metadataMaps.getRight();

        // Only keep levels that are in both maps
        xMap.keySet().retainAll(yMap.keySet());
        yMap.keySet().retainAll(xMap.keySet());

        if (xMap.size() <= MINIMUM_LEVELS) {
            return null;
        }

        int nx = (int) geometry.getGridRange2D().getWidth();
        int ny = (int) geometry.getGridRange2D().getHeight();

        // Group metadata records by location
        Map<GridCoverage, List<PluginDataObject>> recordsByLocation = new HashMap<>();
        for (GridRecord record : xMap.values()) {
            List<PluginDataObject> locRecords = recordsByLocation
                    .computeIfAbsent(record.getLocation(),
                            loc -> new ArrayList<>());
            locRecords.add(record);
        }
        for (GridRecord record : yMap.values()) {
            List<PluginDataObject> locRecords = recordsByLocation
                    .computeIfAbsent(record.getLocation(),
                            loc -> new ArrayList<>());
            locRecords.add(record);
        }

        // Populate metadata records with actual data
        for (Entry<GridCoverage, List<PluginDataObject>> entry : recordsByLocation
                .entrySet()) {
            Request request = getRequest(entry.getKey(), frameTime, geometry);
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

        /*
         * The below loop splits the line into nx coordinates, with each
         * coordinate corresponding to a column in the graph. For each column,
         * it interpolates a data/height pair where each record intersects it.
         * This results in a coordinate -> collection(data/height) map.
         *
         * Then each column is split into ny heights. For each height, the
         * nearest data/height pair above and below is grabbed, and the data
         * value is interpolated from those 2 data/height pairs.
         */
        boolean useNearestNeighbor = useNearestNeighbor();
        Coordinate[] coordinates = GeoUtil.splitLine(nx,
                descriptor.getLine(frameTime).getCoordinates());
        List<float[]> result = new ArrayList<>();
        for (int i = 0; i < nx; i++) {
            List<List<XYData>> dataLists = new ArrayList<>(result.size());
            for (Level level : xMap.keySet()) {
                GridRecord yRecord = yMap.get(level);
                float yVal = interpolateAndConvertYVal(yRecord, coordinates[i],
                        frameTime, geometry);
                if (yVal <= INVALID_VALUE_CUTOFF) {
                    continue;
                }
                GridRecord xRecord = xMap.get(level);
                IDataRecord[] results = (IDataRecord[]) xRecord
                        .getMessageData();
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
                Rectangle xRect = getRectangle(xRecord.getLocation(), frameTime,
                        geometry);
                if (!xRect.contains(xPoint)) {
                    continue;
                }
                for (int c = 0; c < results.length; c++) {
                    FloatDataRecord xRec = (FloatDataRecord) results[c];
                    float xVal = InterpUtils.getInterpolatedData(xRect,
                            xPoint.x, xPoint.y, xRec.getFloatData(),
                            useNearestNeighbor);
                    if (xVal <= INVALID_VALUE_CUTOFF) {
                        continue;
                    }
                    dataLists.get(c).add(new XYData(xVal, yVal));
                }
            }
            while (result.size() < dataLists.size()) {
                float[] floatData = new float[nx * ny];
                Arrays.fill(floatData, FILL_VALUE);
                result.add(floatData);
            }
            for (int c = 0; c < dataLists.size(); c++) {
                List<XYData> dataList = dataLists.get(c);
                float[] floatData = result.get(c);
                float[] column = InterpUtils.makeColumn(dataList, ny, graph,
                        descriptor.getHeightScale().getMinVal() < descriptor
                                .getHeightScale().getMaxVal(),
                        FILL_VALUE, useNearestNeighbor);

                for (int j = 0; j < column.length; j++) {
                    floatData[j * nx + i] = column[j];
                }
            }
        }

        CrossSectionFrameExtraRenderable extraRenderable = buildExtraFrameRenderable(
                frameTime, graph, xMap, yMap, geometry);

        /*
         * Clear the raw data records to save memory, now that we've already
         * processed them to determine the graph data.
         */
        for (List<PluginDataObject> pdos : recordsByLocation.values()) {
            for (PluginDataObject pdo : pdos) {
                pdo.setMessageData(null);
            }
        }

        return new CrossSectionFrameData(result, extraRenderable);
    }

    /**
     * Interpolate the value in yRecord at the given coordinate, and convert it
     * to the height scale's unit.
     *
     * @param yRecord
     * @param coord
     * @param frameTime
     * @param geometry
     * @return interpolated y value
     * @throws VizException
     */
    protected float interpolateAndConvertYVal(GridRecord yRecord,
            Coordinate coord, DataTime frameTime, GridGeometry2D geometry)
            throws VizException {
        float yVal = FILL_VALUE;
        IDataRecord[] yDataRecords = (IDataRecord[]) yRecord.getMessageData();
        if (ArrayUtils.isEmpty(yDataRecords)) {
            return yVal;
        }
        FloatDataRecord yRec = (FloatDataRecord) yDataRecords[0];
        DirectPosition2D yPoint;
        try {
            yPoint = PointUtil.determineExactIndex(coord,
                    yRecord.getLocation().getCrs(),
                    MapUtil.getGridGeometry(yRecord.getLocation()));
        } catch (Exception e) {
            throw new VizException(e);
        }
        Rectangle yRect = getRectangle(yRecord.getLocation(), frameTime,
                geometry);
        if (!yRect.contains(yPoint)) {
            return yVal;
        }
        yVal = InterpUtils.getInterpolatedData(yRect, yPoint.x, yPoint.y,
                yRec.getFloatData());
        yVal = (float) UnitConv
                .getConverterToUnchecked(yRecord.getParameter().getUnit(),
                        descriptor.getHeightScale().getParameterUnit())
                .convert(yVal);
        return yVal;
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
            Coordinate[] lineVertices = descriptor.getLine(time)
                    .getCoordinates();
            List<Coordinate> coordinates = new ArrayList<>(Arrays.asList(GeoUtil
                    .splitLine(geometry.getGridRange2D().width, lineVertices)));
            /*
             * Ensure any middle vertices are included for multi-segment
             * baselines
             */
            Collections.addAll(coordinates, lineVertices);

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
        if (pdo instanceof GridRecord) {
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

    protected Set<GridRecord> getYRecords(DataTime time) throws VizException {
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

        if (line == null || !line.equals(descriptor.getCurrentLine())) {
            rectangleCache.clear();
            line = descriptor.getCurrentLine();
        }
    }

    @Override
    public CoordinateReferenceSystem getDataCoordinateReferenceSystem() {
        if (crs == null) {
            return super.getDataCoordinateReferenceSystem();
        }
        return crs;
    }

    @Override
    public void dispose() {
        super.dispose();
        yRecords.clear();
        rectangleCache.clear();
    }

    @Override
    public String getCreatingEntity() {
        String dataSetId = "";
        RequestConstraint dataSetIdConstraint = resourceData.getMetadataMap()
                .get(GridConstants.DATASET_ID);
        if (dataSetIdConstraint != null
                && dataSetIdConstraint.getConstraintValue() != null) {
            dataSetId = dataSetIdConstraint.getConstraintValue();
        }
        return dataSetId;
    }

    /**
     * Get the metadata maps for the given frame time. The first map is for the
     * data being graphed, while the second map is for height values (using the
     * height scale param). A couple examples are (Temperature on MB/pressure
     * levels, Height MSL on MB levels) and (Reflectivity on TILT levels, Height
     * AGL on TILT levels).
     *
     * @param time
     *            frame time to get metadata for
     * @return pair of metadata maps: (x/data map, y/height map)
     * @throws VizException
     */
    protected Pair<Map<Level, GridRecord>, Map<Level, GridRecord>> getMetadataMaps(
            DataTime time) throws VizException {
        DataTime recordTime = time.clone();
        recordTime.clearLevel();

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
        Set<GridRecord> yRecords = getYRecords(recordTime);
        Map<Level, GridRecord> yMap = new HashMap<>();
        for (GridRecord rec : yRecords) {
            yMap.put(rec.getLevel(), rec);
        }

        return ImmutablePair.of(xMap, yMap);
    }

    /**
     * Build any extra renderables (beyond the standard image, contour, or
     * vector graph data) that should be displayed for the frame that was
     * generated from the given inputs.
     *
     * @param currentTime
     * @param graph
     * @param xMap
     * @param yMap
     * @param geometry
     * @return extra renderable, may be null if no renderables are needed
     */
    protected CrossSectionFrameExtraRenderable buildExtraFrameRenderable(
            DataTime currentTime, CrossSectionGraph graph,
            Map<Level, GridRecord> xMap, Map<Level, GridRecord> yMap,
            GridGeometry2D geometry) {
        /*
         * Normal grid data doesn't add any extra renderables to its frames.
         * This method only exists so that the radar-as-grid subclass of it can
         * add renderables.
         */
        return null;
    }
}
