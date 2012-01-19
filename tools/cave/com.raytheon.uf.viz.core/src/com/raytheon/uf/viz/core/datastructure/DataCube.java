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
package com.raytheon.uf.viz.core.datastructure;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralDirectPosition;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.JTS;
import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.PointUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.GridUtil;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.interp.IInterpolation;
import com.raytheon.uf.viz.core.interp.InterpolationRequest;
import com.raytheon.uf.viz.core.interp.InterpolationResult;
import com.raytheon.uf.viz.core.interp.PythonInterpolation;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.core.slice.AbstractSlice;
import com.raytheon.viz.core.slice.FloatSlice;
import com.raytheon.viz.core.slice.ISliceable;
import com.raytheon.viz.core.slice.request.SliceRequest;
import com.raytheon.viz.core.slice.request.VerticalPlaneRequest;
import com.raytheon.viz.core.slice.request.VerticalPointRequest;
import com.raytheon.viz.core.slice.request.VerticalPointRequest.TimeDirection;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * 4 dimensional data cube that holds data in x, y, z, and time dimensions. All
 * data in the cube should be of the same parameter and grid geometry.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataCube implements ISliceable {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(DataCube.class);

    public enum Mode {
        EAGER, ON_DEMAND
    };

    protected Mode mode;

    protected String type;

    protected String parameter;

    protected Unit<?> units;

    protected ISpatialObject spatialArea;

    protected HashMap<DataCubeKey, Object> data = new HashMap<DataCubeKey, Object>();

    // /** elevations */
    // private double lowerElevation;
    //
    // private double upperElevation;

    protected MathTransform crsFromLatLon = null;

    protected Polygon cubeGeometry;

    public DataCube(String aType, Mode aMode) {
        type = aType;
        mode = aMode;
    }

    @Override
    public AbstractSlice[] slice(SliceRequest request) throws VizException {
        AbstractSlice[] sliceArray = null;
        if (request instanceof VerticalPlaneRequest) {
            sliceArray = verticalPlaneSlice((VerticalPlaneRequest) request);
        } else if (request instanceof VerticalPointRequest) {
            sliceArray = verticalPointSlice((VerticalPointRequest) request);
        } else {
            throw new VizException(
                    "Data cube does not yet support slice requests of type "
                            + request.getClass().getName());
        }

        return sliceArray;
    }

    protected AbstractSlice[] verticalPlaneSlice(VerticalPlaneRequest vReq)
            throws VizException {
        Point startPosition = null;
        Point endPosition = null;

        GeometryFactory gf = new GeometryFactory();

        LineString sliceReq = gf.createLineString(vReq.getCoordinates());
        try {
            sliceReq = (LineString) JTS.transform(sliceReq, crsFromLatLon);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error converting slice coordinates", e);
        }

        double totalDistance = sliceReq.getLength();

        Geometry intersection = sliceReq.intersection(cubeGeometry);
        if (intersection == null || intersection.getCoordinates().length == 0) {
            System.out.println("No intersection");
            return null;
        }

        List<Point> dataPoints = new ArrayList<Point>();
        List<Point> uniquePoints = new ArrayList<Point>();
        Coordinate[] coords = vReq.getCoordinates();
        for (int i = 0; i < coords.length - 1; ++i) {
            Coordinate start = coords[i];
            Coordinate end = coords[i + 1];

            try {
                startPosition = PointUtil.determineIndex(start, spatialArea
                        .getCrs(), MapUtil.getGridGeometry(spatialArea));
                endPosition = PointUtil.determineIndex(end, spatialArea
                        .getCrs(), MapUtil.getGridGeometry(spatialArea));
            } catch (Exception e) {
                throw new VizException("Error determining index in grid.", e);
            }

            // determine points in 2 dimensional grid space of the line
            ArrayList<Point> points = GridUtil.bresenham(startPosition,
                    endPosition);

            // TODO maybe fill in their values with interpolation
            // if the points are outside the grid, drop them
            List<Point> pointsToRemove = new ArrayList<Point>();
            for (Point c : points) {
                if (c.x < 0 || c.y < 0 || c.x >= spatialArea.getNx()
                        || c.y >= spatialArea.getNy()) {
                    pointsToRemove.add(c);
                }
            }

            points.removeAll(pointsToRemove);

            for (Point p : points) {
                if (uniquePoints.contains(p) == false) {
                    uniquePoints.add(p);
                }
            }

            dataPoints.addAll(uniquePoints);
        }

        int nPoints = dataPoints.size();

        Coordinate[] sliceCoords = new Coordinate[dataPoints.size()];
        MathTransform gridToCrs = MapUtil.getGridGeometry(spatialArea)
                .getGridToCRS();
        MathTransform crsToLL;
        try {
            crsToLL = MapUtil.getTransformToLatLon(spatialArea.getCrs());
        } catch (FactoryException e) {
            throw new VizException(
                    "Error Determining Coordinates of Grid Point.", e);
        }
        for (int i = 0; i < dataPoints.size(); i++) {
            Point p = dataPoints.get(i);
            DirectPosition2D result = new DirectPosition2D();
            try {
                DirectPosition2D crsDP = new DirectPosition2D();
                gridToCrs.transform(new DirectPosition2D(p.x, p.y), crsDP);
                crsToLL.transform(crsDP, result);
            } catch (Exception e) {
                throw new VizException(
                        "Error Determining Coordinates of Grid Point.", e);
            }
            sliceCoords[i] = new Coordinate(result.x, result.y);
        }

        SingleLevel[] reqLevels = vReq.getLevels();
        Arrays.sort(reqLevels);

        double lowestLevel = Math.min(reqLevels[0].getValue(),
                reqLevels[reqLevels.length - 1].getValue());
        double highestLevel = Math.max(reqLevels[0].getValue(),
                reqLevels[reqLevels.length - 1].getValue());
        // TODO fix max distance when slice req endpoints are outside datacube
        GeneralEnvelope env = new GeneralEnvelope(
                new double[] { 0, lowestLevel }, new double[] { totalDistance,
                        highestLevel });
        GeneralGridEnvelope range = new GeneralGridEnvelope(new int[] { 0, 0 },
                new int[] { nPoints, reqLevels.length }, false);

        // create a slice for each time
        ArrayList<AbstractSlice> sliceList = new ArrayList<AbstractSlice>();
        for (DataTime time : vReq.getTimes()) {
            GridGeometry2D sliceGeom = new GridGeometry2D(range, env);
            FloatSlice slice = new FloatSlice();
            slice = new FloatSlice();
            slice.setGridGeometry(sliceGeom);

            int dataLength = nPoints * reqLevels.length;
            PluginDataObject[] organizedData = new PluginDataObject[reqLevels.length];
            int n = 0;
            for (int i = 0; i < reqLevels.length; i++) {
                organizedData[n] = (PluginDataObject) data.get(new DataCubeKey(
                        time, reqLevels[i]));
                n++;
            }

            // retrieve the organized data values
            float[] result = new float[dataLength];
            if (mode == Mode.EAGER) {
                // determine indices in 1 dimensional grid space of the line
                int[] indices = new int[nPoints];
                for (int i = 0; i < nPoints; i++) {
                    Point point = dataPoints.get(i);
                    indices[i] = point.y * spatialArea.getNx() + point.x;
                }

                float[] sliceLevelData = new float[nPoints];
                for (int i = 0; i < organizedData.length; i++) {
                    float[] levelData = ((float[]) organizedData[i]
                            .getMessageData());
                    for (int k = 0; k < nPoints; k++) {
                        sliceLevelData[k] = levelData[indices[k]];
                    }

                    System.arraycopy(sliceLevelData, 0, result, i * nPoints,
                            nPoints);
                }
            } else if (mode == Mode.ON_DEMAND) {
                float[] tmp = CubeUtil
                        .retrieveData(organizedData, uniquePoints);
                if (dataPoints.size() != uniquePoints.size()) {
                    result = new float[dataPoints.size() * organizedData.length];
                    for (int i = 0; i < dataPoints.size(); ++i) {
                        int idx = uniquePoints.indexOf(dataPoints.get(i));
                        for (int j = 0; j < organizedData.length; ++j) {
                            result[i + j * dataPoints.size()] = tmp[idx + j
                                    * uniquePoints.size()];
                        }
                    }
                } else {
                    result = tmp;
                }
            }

            InterpolationRequest request = vReq.getRequest();
            float[] xValues = new float[sliceGeom.getGridRange2D().width];
            for (int i = 0; i < xValues.length; ++i) {
                xValues[i] = i;
            }
            request.setXData(xValues);
            request.setMinX(0);
            request.setMaxX(xValues[xValues.length - 1]);
            request.setZData(result);
            scaleData(request);

            float interpFactor = 1.0f;
            IInterpolation interp = new PythonInterpolation(vReq.getScale());
            InterpolationResult iResults = interp.interpolate(request);
            result = iResults.getValues();

            sliceGeom = iResults.getGeometry();
            slice.setGridGeometry(sliceGeom);
            slice.setInterpolationFactor(interpFactor);
            slice.setSliceData(result);
            slice.setDataTime(new DataTime[] { time });
            slice.setLevels(vReq.getLevels());
            slice.setCoordinates(sliceCoords);
            sliceList.add(slice);
        }

        return sliceList.toArray(new AbstractSlice[sliceList.size()]);
    }

    protected AbstractSlice[] verticalPointSlice(VerticalPointRequest vReq)
            throws VizException {
        if (!this.contains(vReq.getCoordinate())) {
            return new AbstractSlice[0];
        }

        Point position = null;
        try {
            position = PointUtil.determineIndex(vReq.getCoordinate(),
                    spatialArea.getCrs(), MapUtil.getGridGeometry(spatialArea));
        } catch (Exception e) {
            throw new VizException("Error determining index in grid.", e);
        }

        DataTime[] times = vReq.getTimes();
        Arrays.sort(times);
        int nPoints = times.length;

        double earliestTime = times[0].getValidTime().getTimeInMillis();

        SingleLevel[] reqLevels = vReq.getLevels();
        Arrays.sort(reqLevels);

        double lowestLevel = Math.min(reqLevels[0].getValue(),
                reqLevels[reqLevels.length - 1].getValue());
        double highestLevel = Math.max(reqLevels[0].getValue(),
                reqLevels[reqLevels.length - 1].getValue());
        GeneralEnvelope env = new GeneralEnvelope(
                new double[] { 0, lowestLevel }, new double[] { earliestTime,
                        highestLevel });
        GeneralGridEnvelope range = new GeneralGridEnvelope(new int[] { 0, 0 },
                new int[] { nPoints, reqLevels.length }, false);
        GridGeometry2D sliceGeom = new GridGeometry2D(range, env);

        ArrayList<AbstractSlice> sliceList = new ArrayList<AbstractSlice>();
        FloatSlice slice = new FloatSlice();
        slice = new FloatSlice();
        slice.setGridGeometry(sliceGeom);
        int dataLength = nPoints * reqLevels.length;

        PluginDataObject[] organizedData = new PluginDataObject[dataLength];

        int n = 0;
        for (int i = 0; i < reqLevels.length; i++) {
            if (vReq.getTimeDirection() == TimeDirection.LEFT_TO_RIGHT) {
                for (int k = 0; k < times.length; k++) {
                    organizedData[n] = (PluginDataObject) data
                            .get(new DataCubeKey(times[k], reqLevels[i]));
                    n++;
                }
            } else if (vReq.getTimeDirection() == TimeDirection.RIGHT_TO_LEFT) {
                for (int k = times.length - 1; k > -1; k--) {
                    organizedData[n] = (PluginDataObject) data
                            .get(new DataCubeKey(times[k], reqLevels[i]));
                    n++;
                }
            }
        }

        float[] result = new float[dataLength];
        if (mode == Mode.EAGER) {
            // determine indices in 1 dimensional grid space of the line
            int index = position.y * spatialArea.getNx() + position.x;

            for (int i = 0; i < dataLength; i++) {
                result[i] = ((float[]) organizedData[i].getMessageData())[index];
            }
        } else if (mode == Mode.ON_DEMAND) {
            result = CubeUtil.retrieveData(organizedData, position.x,
                    position.y);
        }

        InterpolationRequest request = vReq.getRequest();
        if (request != null) {
            float[] xValues = new float[sliceGeom.getGridRange2D().width];
            for (int i = 0; i < xValues.length; ++i) {
                xValues[i] = i;
            }
            request.setXData(xValues);
            request.setMinX(0);
            request.setMaxX(xValues[xValues.length - 1]);
            request.setZData(result);
            scaleData(request);

            IInterpolation interp = new PythonInterpolation(vReq.getScale());
            InterpolationResult iResults = interp.interpolate(request);
            result = iResults.getValues();

            sliceGeom = iResults.getGeometry();
        }

        slice.setGridGeometry(sliceGeom);
        slice.setSliceData(result);
        slice.setDataTime(times);
        slice.setLevels(vReq.getLevels());
        sliceList.add(slice);

        return sliceList.toArray(new AbstractSlice[sliceList.size()]);
    }

    public void addData(DataTime aTime, SingleLevel aLevel, Object aData) {
        DataCubeKey key = new DataCubeKey(aTime, aLevel);
        data.put(key, aData);
    }

    /**
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * @param parameter
     *            the parameter to set
     */
    public void setParameter(String parameter) {
        this.parameter = parameter;
    }

    /**
     * @return the spatialArea
     */
    public ISpatialObject getSpatialArea() {
        return spatialArea;
    }

    /**
     * @param spatialArea
     *            the spatialArea to set
     */
    public void setSpatialArea(ISpatialObject spatialArea) {
        this.spatialArea = spatialArea;
    }

    /**
     * Get the data levels
     * 
     * @param sort
     *            returned levels are in ascending order
     * @return
     */
    public SingleLevel[] getAvailableLevels(boolean sort) {
        Set<SingleLevel> set = new HashSet<SingleLevel>();
        Set<DataCubeKey> cubeKeys = data.keySet();
        for (DataCubeKey key : cubeKeys) {
            set.add(key.getLevel());
        }
        SingleLevel[] levels = set.toArray(new SingleLevel[set.size()]);

        if (sort) {
            Arrays.sort(levels);
        }
        return levels;
    }

    /**
     * Get the available times
     * 
     * @param sort
     *            returned data times are in ascending order
     * @return
     */
    public DataTime[] getAvailableTimes(boolean sort) {
        Set<DataTime> set = new HashSet<DataTime>();
        Set<DataCubeKey> cubeKeys = data.keySet();
        for (DataCubeKey key : cubeKeys) {
            set.add(key.getTime());
        }

        DataTime[] times = set.toArray(new DataTime[set.size()]);
        if (sort) {
            Arrays.sort(times);
        }
        return times;
    }

    /**
     * @return the units
     */
    public Unit<?> getUnits() {
        return units;
    }

    /**
     * @param units
     *            the units to set
     */
    public void setUnits(Unit<?> units) {
        this.units = units;
    }

    /**
     * Get the raw data for all levels
     */
    public Object getData(DataTime time) {
        SingleLevel[] levels = getAvailableLevels(true);
        DataTime[] times = getAvailableTimes(false);
        Arrays.sort(times);
        int index = 0;
        for (DataTime d : times) {
            if (d.equals(time)) {
                break;
            }
            index++;
        }

        float[] cubeData = (float[]) ((PluginDataObject) data
                .get(new DataCubeKey(times[index], levels[0])))
                .getMessageData();

        float[][] theData = new float[levels.length][cubeData.length];

        for (int i = 0; i < levels.length; ++i) {
            cubeData = (float[]) ((PluginDataObject) this.data
                    .get(new DataCubeKey(times[index], levels[i])))
                    .getMessageData();

            System.arraycopy(cubeData, 0, theData[i], 0, cubeData.length);
        }

        return theData;
    }

    /**
     * Determine if coordinate is inside the data cube
     * 
     * @param c
     * @return
     */
    public boolean contains(Coordinate c) {
        DirectPosition ll = new GeneralDirectPosition(MapUtil.LATLON_PROJECTION);
        ll.setOrdinate(0, c.x);
        ll.setOrdinate(1, c.y);
        DirectPosition crs = new GeneralDirectPosition(spatialArea.getCrs());
        try {
            crsFromLatLon.transform(ll, crs);
        } catch (MismatchedDimensionException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (TransformException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        Coordinate newC = new Coordinate(crs.getOrdinate(0), crs.getOrdinate(1));

        GeometryFactory gf = new GeometryFactory();
        com.vividsolutions.jts.geom.Point p = gf.createPoint(newC);

        return this.cubeGeometry.contains(p);
    }

    public void initGeometry() {

        try {
            crsFromLatLon = MapUtil
                    .getTransformFromLatLon(spatialArea.getCrs());
        } catch (FactoryException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        DirectPosition lowerCorner = MapUtil.getGridGeometry(spatialArea)
                .getEnvelope().getLowerCorner();
        DirectPosition upperCorner = MapUtil.getGridGeometry(spatialArea)
                .getEnvelope().getUpperCorner();

        GeometryFactory gf = new GeometryFactory();

        Coordinate p1 = new Coordinate(lowerCorner.getOrdinate(0), lowerCorner
                .getOrdinate(1));
        Coordinate p2 = new Coordinate(lowerCorner.getOrdinate(0), upperCorner
                .getOrdinate(1));
        Coordinate p3 = new Coordinate(upperCorner.getOrdinate(0), upperCorner
                .getOrdinate(1));
        Coordinate p4 = new Coordinate(upperCorner.getOrdinate(0), lowerCorner
                .getOrdinate(1));

        LinearRing lr = gf.createLinearRing(new Coordinate[] { p1, p2, p3, p4,
                p1 });

        cubeGeometry = gf.createPolygon(lr, null);

    }

    private void scaleData(InterpolationRequest request)
            throws VizDataCubeException {
        int maxPoints = 1000;
        float[] xData = request.getXData();
        float[] yData = request.getYData();
        float[] zValues = request.getZData();
        int xlen = xData.length;
        int ylen = yData.length;
        int totalSize = xlen * ylen;
        float inc = totalSize / maxPoints;
        Set<Integer> keep = new HashSet<Integer>();

        if (inc > 1) {
            keep.add(0);
            for (int i = (int) inc; i < totalSize && keep.size() < maxPoints; i += inc) {
                keep.add(i);
            }
            totalSize = maxPoints;
        }
        int skipped = 0;
        float[] tmpZ = new float[totalSize];
        float[] tmpX = new float[totalSize];
        float[] tmpY = new float[totalSize];
        int actualSize = 0;
        for (int i = 0; i < yData.length; ++i) {
            for (int j = 0; j < xData.length; ++j) {
                int index = i * xData.length + j;
                if ((inc <= 1 || keep.contains(index))
                        && (zValues[index] > CubeUtil.MISSING)) {
                    tmpX[index - skipped] = xData[j];
                    tmpY[index - skipped] = yData[i];
                    tmpZ[index - skipped] = zValues[index];
                    ++actualSize;
                } else {
                    skipped++;
                }
            }
        }
        if (actualSize == 0) {
            throw new VizDataCubeException(
                    "No Valid data found for interpolation");
        }
        zValues = new float[actualSize];
        xData = new float[actualSize];
        yData = new float[actualSize];
        System.arraycopy(tmpZ, 0, zValues, 0, actualSize);
        System.arraycopy(tmpX, 0, xData, 0, actualSize);
        System.arraycopy(tmpY, 0, yData, 0, actualSize);

        request.setXData(xData);
        request.setYData(yData);
        request.setZData(zValues);
    }

}
