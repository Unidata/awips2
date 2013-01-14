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
package com.raytheon.uf.edex.topo;

import java.awt.Point;
import java.awt.Rectangle;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.factory.Hints;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.ReferencingFactoryFinder;
import org.geotools.referencing.operation.AbstractCoordinateOperationFactory;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.geotools.referencing.operation.transform.IdentityTransform;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.DirectPosition;
import org.opengis.geometry.Envelope;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.GeographicCRS;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.CoordinateOperationFactory;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.MathTransformFactory;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.topo.ITopoQuery;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TopoQuery allows the caller to query topo height for a single point, a series
 * of points (line), or a grid. If a grid is requested the data will be
 * re-projected and interpolated to the callers grid geometry.
 * 
 * Known Limitations:
 * 
 * Fails for grids covering "large" area especially if contains the pole.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/19/2007   #377       randerso    Initial creation
 * Jun 13, 2008     #1160   randerso    Moved to server side
 * 03/09/2012   DR 14581   D. Friedman Fix grid referencing and use custom 
 *                                     nearest-neighbor resampling.i
 * 01/14/2013   #1469       bkowal     Removed the hdf5 data directory.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TopoQuery implements ITopoQuery {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TopoQuery.class);

    private static final String TOPO_FILE = "/topo/srtm30.hdf";

    private static final int TOPO_LIMIT = 4096 * 4096;;

    private static Map<Integer, TopoQuery> topoQueryMap;

    /**
     * @return Initialized TopoQuery instance
     */
    public static synchronized TopoQuery getInstance() {
        // default Topo is 4800X6000, level zero
        return getInstance(0);
    }

    /**
     * @return Initialized TopoQuery instance
     */
    public static synchronized TopoQuery getInstance(int topoLevel) {
        return getInstance(new File(TOPO_FILE), topoLevel);
    }

    public static synchronized TopoQuery getInstance(File hdf5File,
            int topoLevel) {
        if (topoQueryMap == null) {
            topoQueryMap = new Hashtable<Integer, TopoQuery>();
        }
        TopoQuery query = topoQueryMap.get(topoLevel);
        if (query == null) {
            try {
                query = new TopoQuery(hdf5File, topoLevel);
                topoQueryMap.put(topoLevel, query);
            } catch (EdexException e) {
                statusHandler.handle(Priority.PROBLEM, e.getMessage(), e);
            }
        }
        return query;
    }

    /**
     * The hdf5 file containing the topo data
     */
    private File hdf5File;

    private IDataStore dataStore;

    private String dataset;

    protected Rectangle worldRect;

    private GridGeometry2D worldGeomPM;

    private MathTransform worldToLLPM;

    private GridGeometry2D worldGeomDL;

    private MathTransform llToWorldPM;

    private int topoLevel;

    private DefaultMathTransformFactory dmtf;

    private int numLevels;

    TopoQuery(File hdf5File) throws EdexException {
        this(hdf5File, 0);
    }

    TopoQuery(File hdf5File, int level) throws EdexException {
        this.topoLevel = level;
        this.hdf5File = hdf5File;
        init();
    }

    private void init() throws EdexException {
        dmtf = new DefaultMathTransformFactory();
        dataStore = DataStoreFactory.getDataStore(hdf5File);
        numLevels = getNumLevels();

        Request request = Request.buildSlab(new int[] { 0, 0 }, new int[] { 1,
                1 });

        try {
            if (topoLevel == 0) {
                dataset = "/full";
            } else {
                dataset = "/interpolated/" + topoLevel;
            }

            IDataRecord record = dataStore.retrieve("", dataset, request);
            Map<String, Object> attributes = record.getDataAttributes();
            int width = (Integer) attributes.get("Width");
            int height = (Integer) attributes.get("Height");
            double ulLat = (Double) attributes.get("ulLat");
            double ulLon = (Double) attributes.get("ulLon");
            double lrLat = (Double) attributes.get("lrLat");
            double lrLon = (Double) attributes.get("lrLon");
            String crsString = (String) attributes.get("CRS");

            worldRect = new Rectangle(0, 0, width, height);

            // construct the grid geometry that covers the topo grid
            CoordinateReferenceSystem crs = CRSCache.getInstance()
                    .getCoordinateReferenceSystem(crsString);

            crs = MapUtil.constructEquidistantCylindrical(
                    MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS, 0,
                    0);

            double[] input = new double[] { ulLon, ulLat, lrLon, lrLat };
            double[] output = new double[4];

            MathTransform llToCrsPM = MapUtil.getTransformFromLatLon(crs);
            llToCrsPM.transform(input, 0, output, 0, 2);

            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(crs);
            ge.setRange(0, output[0], output[2]);
            ge.setRange(1, output[3], output[1]);

            GeneralGridEnvelope gr = new GeneralGridEnvelope(
                    new int[] { 1, 1 }, new int[] { width, height }, false);

            GridToEnvelopeMapper mapper = new GridToEnvelopeMapper();
            mapper.setEnvelope(ge);
            mapper.setGridRange(gr);
            mapper.setPixelAnchor(PixelInCell.CELL_CENTER);
            mapper.setReverseAxis(new boolean[] { false, true });
            MathTransform mt = mapper.createTransform();

            worldGeomPM = new GridGeometry2D(PixelInCell.CELL_CORNER, mt, ge,
                    null);

            // set up the transform from grid coordinates to lon/lat
            MathTransform worldToCRSPM = worldGeomPM
                    .getGridToCRS(PixelOrientation.UPPER_LEFT);
            MathTransform crsToLL = MapUtil.getTransformToLatLon(crs);
            worldToLLPM = dmtf.createConcatenatedTransform(worldToCRSPM,
                    crsToLL);
            llToWorldPM = worldToLLPM.inverse();

            crs = MapUtil.constructEquidistantCylindrical(
                    MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                    180, 0);

            input = new double[] { 180 + ulLon, ulLat, 180 + lrLon, lrLat };
            MathTransform llToCrsDL = MapUtil.getTransformFromLatLon(crs);
            llToCrsDL.transform(input, 0, output, 0, 2);

            ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(crs);
            ge.setRange(0, output[0], output[2]);
            ge.setRange(1, output[3], output[1]);

            gr = new GeneralGridEnvelope(new int[] { 1 + (width / 2), 1 },
                    new int[] { width + (width / 2), height }, false);

            mapper.setEnvelope(ge);
            mapper.setGridRange(gr);
            mapper.setPixelAnchor(PixelInCell.CELL_CENTER);
            mapper.setReverseAxis(new boolean[] { false, true });
            mt = mapper.createTransform();

            worldGeomDL = new GridGeometry2D(PixelInCell.CELL_CORNER, mt, ge,
                    null);

        } catch (Exception e) {
            throw new EdexException("Error initializing TopoQuery from "
                    + hdf5File.getAbsolutePath(), e);
        }
    }

    /**
     * Retrieves topo height in meters above mean sea level for the specified
     * coordinate.
     * 
     * @param latLon
     *            should contain lon/lat in degrees
     * @return topo height in meters MSL
     */
    public double getHeight(Coordinate latLon) {
        return getHeight(new Coordinate[] { latLon })[0];
    }

    /**
     * Retrieves topo height in meters above mean sea level for the specified
     * coordinates.
     * 
     * @param coords
     *            should contain lon/lat in degrees
     * @return topo height in meters MSL
     */
    public double[] getHeight(Coordinate[] coords) {
        final int size = coords.length;
        double[] topo = new double[size];

        double[] input = new double[size * 2];
        double[] output = new double[input.length];
        int index = 0;
        for (Coordinate c : coords) {
            input[index++] = c.x;
            input[index++] = c.y;
        }

        try {
            llToWorldPM.transform(input, 0, output, 0, size);
            Point[] points = new Point[size];
            index = 0;
            for (int i = 0; i < size; i++) {
                points[i] = new Point((int) output[index],
                        (int) output[index + 1]);
                index += 2;
            }

            Request request = Request.buildPointRequest(points);
            ShortDataRecord record = (ShortDataRecord) dataStore.retrieve("/",
                    "full", request);
            short[] data = record.getShortData();
            // bounds checking?
            for (int i = 0; i < size; i++) {
                topo[i] = data[i];
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retriving topo value for lat/lons", e);
            Arrays.fill(topo, Double.NaN);
        }

        return topo;
    }

    /**
     * Computes the bounding lat/lon envelope containing the specified geometry
     * 
     * @param geom
     * @return the bounding envelope
     */
    private Envelope[] computeLLEnv(GridGeometry2D geom) {
        GridEnvelope range = geom.getGridRange();
        try {
            MathTransform toLatLon = MapUtil.getTransformToLatLon(geom
                    .getCoordinateReferenceSystem());
            MathTransform toCRS = geom
                    .getGridToCRS(PixelOrientation.UPPER_LEFT);
            MathTransform gridToLL = dmtf.createConcatenatedTransform(toCRS,
                    toLatLon);

            int minX = range.getLow(0);
            int minY = range.getLow(1);
            int maxX = range.getHigh(0) + 2;
            int maxY = range.getHigh(1) + 2;

            double minPosLon = Double.POSITIVE_INFINITY;
            double maxPosLon = Double.NEGATIVE_INFINITY;
            double minPosLat = Double.POSITIVE_INFINITY;
            double maxPosLat = Double.NEGATIVE_INFINITY;

            double minNegLon = Double.POSITIVE_INFINITY;
            double maxNegLon = Double.NEGATIVE_INFINITY;
            double minNegLat = Double.POSITIVE_INFINITY;
            double maxNegLat = Double.NEGATIVE_INFINITY;

            double prev = 0.0;
            boolean first = true;
            boolean crossedDLHoriz = false;
            for (int x = minX; x <= maxX; x++) {
                DirectPosition p1 = new DirectPosition2D(x, minY);
                DirectPosition p2 = new DirectPosition2D(x, maxY);

                p1 = gridToLL.transform(p1, null);
                p2 = gridToLL.transform(p2, null);

                if (first) {
                    first = false;
                } else if (Math.abs(p1.getOrdinate(0) - prev) > 180.0
                        || (Math.abs(p1.getOrdinate(0)) > 180 && Math.abs(prev) < 180)) {
                    crossedDLHoriz = true;
                }
                prev = p1.getOrdinate(0);
                if (p1.getOrdinate(0) > 180) {
                    p1.setOrdinate(0, p1.getOrdinate(0) - 360);
                }
                if (p2.getOrdinate(0) > 180) {
                    p2.setOrdinate(0, p2.getOrdinate(0) - 360);
                }

                if (p1.getOrdinate(0) > 0.0) {
                    minPosLon = Math.min(minPosLon, p1.getOrdinate(0));
                    maxPosLon = Math.max(maxPosLon, p1.getOrdinate(0));
                } else {
                    minNegLon = Math.min(minNegLon, p1.getOrdinate(0));
                    maxNegLon = Math.max(maxNegLon, p1.getOrdinate(0));
                }

                if (p2.getOrdinate(0) > 0.0) {
                    minPosLon = Math.min(minPosLon, p2.getOrdinate(0));
                    maxPosLon = Math.max(maxPosLon, p2.getOrdinate(0));
                } else {
                    minNegLon = Math.min(minNegLon, p2.getOrdinate(0));
                    maxNegLon = Math.max(maxNegLon, p2.getOrdinate(0));
                }

                if (p1.getOrdinate(1) > 0.0) {
                    minPosLat = Math.min(minPosLat, p1.getOrdinate(1));
                    maxPosLat = Math.max(maxPosLat, p1.getOrdinate(1));
                } else {
                    minNegLat = Math.min(minNegLat, p1.getOrdinate(1));
                    maxNegLat = Math.max(maxNegLat, p1.getOrdinate(1));
                }

                if (p2.getOrdinate(1) > 0.0) {
                    minPosLat = Math.min(minPosLat, p2.getOrdinate(1));
                    maxPosLat = Math.max(maxPosLat, p2.getOrdinate(1));
                } else {
                    minNegLat = Math.min(minNegLat, p2.getOrdinate(1));
                    maxNegLat = Math.max(maxNegLat, p2.getOrdinate(1));
                }
            }

            prev = 0.0;
            first = true;
            boolean crossedDLVert = false;
            for (int y = minY; y <= maxY; y++) {
                DirectPosition p1 = new DirectPosition2D(minX, y);
                DirectPosition p2 = new DirectPosition2D(maxX, y);

                p1 = gridToLL.transform(p1, null);
                p2 = gridToLL.transform(p2, null);

                if (first) {
                    first = false;
                } else if (Math.abs(p1.getOrdinate(0) - prev) > 180.0
                        || (Math.abs(p1.getOrdinate(0)) > 180 && Math.abs(prev) < 180)) {
                    crossedDLVert = true;
                }
                prev = p1.getOrdinate(0);
                if (p1.getOrdinate(0) > 180) {
                    p1.setOrdinate(0, p1.getOrdinate(0) - 360);
                }
                if (p2.getOrdinate(0) > 180) {
                    p2.setOrdinate(0, p2.getOrdinate(0) - 360);
                }

                if (p1.getOrdinate(0) > 0.0) {
                    minPosLon = Math.min(minPosLon, p1.getOrdinate(0));
                    maxPosLon = Math.max(maxPosLon, p1.getOrdinate(0));
                } else {
                    minNegLon = Math.min(minNegLon, p1.getOrdinate(0));
                    maxNegLon = Math.max(maxNegLon, p1.getOrdinate(0));
                }

                if (p2.getOrdinate(0) > 0.0) {
                    minPosLon = Math.min(minPosLon, p2.getOrdinate(0));
                    maxPosLon = Math.max(maxPosLon, p2.getOrdinate(0));
                } else {
                    minNegLon = Math.min(minNegLon, p2.getOrdinate(0));
                    maxNegLon = Math.max(maxNegLon, p2.getOrdinate(0));
                }

                if (p1.getOrdinate(1) > 0.0) {
                    minPosLat = Math.min(minPosLat, p1.getOrdinate(1));
                    maxPosLat = Math.max(maxPosLat, p1.getOrdinate(1));
                } else {
                    minNegLat = Math.min(minNegLat, p1.getOrdinate(1));
                    maxNegLat = Math.max(maxNegLat, p1.getOrdinate(1));
                }

                if (p2.getOrdinate(1) > 0.0) {
                    minPosLat = Math.min(minPosLat, p2.getOrdinate(1));
                    maxPosLat = Math.max(maxPosLat, p2.getOrdinate(1));
                } else {
                    minNegLat = Math.min(minNegLat, p2.getOrdinate(1));
                    maxNegLat = Math.max(maxNegLat, p2.getOrdinate(1));
                }
            }

            Envelope[] llEnv = null;
            if (crossedDLHoriz) {
                llEnv = new Envelope[2];

                GeneralEnvelope env = new GeneralEnvelope(2);
                env.setCoordinateReferenceSystem(MapUtil.LATLON_PROJECTION);
                env.setRange(0, minPosLon, 180.0);
                env.setRange(1, Math.min(minNegLat, minPosLat),
                        Math.max(maxNegLat, maxPosLat));
                llEnv[0] = env;

                env = new GeneralEnvelope(2);
                env.setCoordinateReferenceSystem(MapUtil.LATLON_PROJECTION);
                env.setRange(0, -180.0, maxNegLon);
                env.setRange(1, Math.min(minNegLat, minPosLat),
                        Math.max(maxNegLat, maxPosLat));
                llEnv[1] = env;
            } else if (crossedDLVert) {
                llEnv = new Envelope[1];

                GeneralEnvelope env = new GeneralEnvelope(2);
                env.setCoordinateReferenceSystem(MapUtil.LATLON_PROJECTION);
                env.setRange(0, -180.0, 180.0);
                // if (maxLat > 0) {
                // env.setRange(1, minLat, 90.0);
                // } else {
                // env.setRange(1, -90.0, maxLat);
                // }
                llEnv[0] = env;
            } else {
                llEnv = new Envelope[1];

                GeneralEnvelope env = new GeneralEnvelope(2);
                env.setCoordinateReferenceSystem(MapUtil.LATLON_PROJECTION);
                env.setRange(0, Math.min(minNegLon, minPosLon),
                        Math.max(maxNegLon, maxPosLon));
                env.setRange(1, Math.min(minNegLat, minPosLat),
                        Math.max(maxNegLat, maxPosLat));
                llEnv[0] = env;
            }

            return llEnv;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error computing lat/lon envelope", e);
        }
        return null;
    }

    private int getNumLevels() {
        try {
            return dataStore.getDatasets("/interpolated").length + 1;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting interpolation levels", e);
            return 0;
        }
    }

    /**
     * Computes the bounding envelope containing the rectangle in the world grid
     * 
     * @param worldRect
     * @return the bounding envelope
     */
    private Envelope computeEnv(Rectangle worldRect) {
        try {
            double[] worldCorners = new double[] { worldRect.getMinX(),
                    worldRect.getMinY(), worldRect.getMaxX(),
                    worldRect.getMaxY() };
            double[] crsCorners = new double[worldCorners.length];
            GeneralEnvelope env = new GeneralEnvelope(2);
            if (worldCorners[2] > worldGeomPM.getGridRange().getHigh(0) + 1) {
                worldGeomDL.getGridToCRS(PixelInCell.CELL_CORNER).transform(worldCorners, 0,
                        crsCorners, 0, worldCorners.length / 2);
                env.setCoordinateReferenceSystem(worldGeomDL.getEnvelope()
                        .getCoordinateReferenceSystem());
            } else {
                worldGeomPM.getGridToCRS(PixelInCell.CELL_CORNER).transform(worldCorners, 0,
                        crsCorners, 0, worldCorners.length / 2);
                env.setCoordinateReferenceSystem(worldGeomPM.getEnvelope()
                        .getCoordinateReferenceSystem());
            }

            double minX = Double.POSITIVE_INFINITY;
            double maxX = Double.NEGATIVE_INFINITY;
            double minY = Double.POSITIVE_INFINITY;
            double maxY = Double.NEGATIVE_INFINITY;

            for (int i = 0; i < crsCorners.length; i += 2) {
                minX = Math.min(minX, crsCorners[i]);
                minY = Math.min(minY, crsCorners[i + 1]);
                maxX = Math.max(maxX, crsCorners[i]);
                maxY = Math.max(maxY, crsCorners[i + 1]);
            }

            env.setRange(0, minX, maxX);
            env.setRange(1, minY, maxY);

            return env;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error computing envelope",
                    e);
        }
        return null;
    }

    /**
     * Computes the bounding rectangle in the topo world space containing the
     * specified geometry
     * 
     * @param geom
     * @return the bounding rectangle
     */
    private Rectangle[] computeWorldRect(GridGeometry2D geom) {
        Envelope llEnv[] = computeLLEnv(geom);
        Rectangle result[] = new Rectangle[llEnv.length];
        int i = 0;
        for (Envelope env : llEnv) {
            result[i++] = computeWorldRect(env);
        }
        return result;
    }

    /**
     * Computes the bounding rectangle in the topo world space containing the
     * specified lon/lat envelope
     * 
     * @param llEnv
     * @return the bounding rectangle
     */
    private Rectangle computeWorldRect(Envelope llEnv) {
        try {
            double[] llCorners = new double[] { llEnv.getMinimum(0),
                    llEnv.getMinimum(1), llEnv.getMaximum(0),
                    llEnv.getMaximum(1) };
            double[] worldCorners = new double[llCorners.length];
            llToWorldPM.transform(llCorners, 0, worldCorners, 0,
                    llCorners.length / 2);
            int x = (int) worldCorners[0];
            int y = (int) worldCorners[3];
            int width = (int) Math.abs(Math.round(worldCorners[2]) - x);
            int height = (int) Math.round(worldCorners[1]) - y;
            Rectangle worldRect = new Rectangle(x, y, width, height);

            return worldRect;

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error computing world rectangle", e);
        }

        return null;
    }

    /**
     * Retrieves topo height in meters above mean sea level reprojected and
     * interpolated to the specified grid geometry.
     * 
     * @param targetGeom
     * @return the topo data array in row major order
     */
    public float[] getHeight(GridGeometry2D targetGeom) {
        Rectangle rectangles[] = computeWorldRect(targetGeom);

        int width = 0;
        int height = rectangles[0].height;
        for (Rectangle rect : rectangles) {
            width += rect.getWidth();
        }

        // if grid is too big to load into memory
        if (width * height > TOPO_LIMIT) {
            // try the next interpolation level if it exists
            int level = topoLevel + 1;
            if (level < numLevels) {
                return getInstance(hdf5File, level).getHeight(targetGeom);
            } else {
                throw new IllegalArgumentException(
                        "Grid requires too much memory to interpolate");
            }
        }
        float[][] topoValues = new float[height][width];

        for (float[] fs : topoValues) {
            Arrays.fill(fs, Float.NaN);
        }

        // boolean someData = false;
        int rectOffset = 0;
        for (Rectangle worldRect : rectangles) {
            if (this.worldRect.intersects(worldRect)) {
                Rectangle intersection = this.worldRect.intersection(worldRect);

                int x = intersection.x - this.worldRect.x;
                int y = intersection.y - this.worldRect.y;

                ShortDataRecord rec;
                try {
                    Request request = Request.buildSlab(new int[] { x, y },
                            new int[] { x + intersection.width,
                                    y + intersection.height });
                    rec = (ShortDataRecord) dataStore.retrieve("", dataset,
                            request);

                    int xOffset = intersection.x - worldRect.x + rectOffset;
                    int yOffset = intersection.y - worldRect.y;

                    int recOffset = 0;
                    for (int j = 0; j < intersection.height; j++) {
                        for (int i = 0; i < intersection.width; i++) {
                            topoValues[j + yOffset][i + xOffset] = rec
                                    .getShortData()[i + recOffset];
                        }
                        recOffset += intersection.width;
                        // someData = true;
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error retrieving topo data", e);
                }

            }
            rectOffset += worldRect.width;
        }

        // if (!someData) {
        // throw new EdexException("No topo data available");
        // }

        Envelope env = computeEnv(new Rectangle(rectangles[0].x,
                rectangles[0].y, width, height));

        GridCoverageFactory factory = new GridCoverageFactory();
        GridCoverage2D baseGC = factory.create("", topoValues, env);

        return simpleResample(baseGC, topoValues, targetGeom);
    }
    
    float[] simpleResample(GridCoverage2D sourceGC, float[][] sourceData, GridGeometry2D targetGG) {
    	int sourceWidth = sourceGC.getGridGeometry().getGridRange2D().getSpan(0);
    	int sourceHeight = sourceGC.getGridGeometry().getGridRange2D().getSpan(1);
    	int targetWidth = targetGG.getGridRange2D().getSpan(0);
    	int targetHeight = targetGG.getGridRange2D().getSpan(1);
    	float[] output = new float[targetWidth * targetHeight];
    	Arrays.fill(output, Float.NaN);

    	ArrayList<MathTransform> transforms = new ArrayList<MathTransform>();
    	ArrayList<MathTransform> toGeoXforms = new ArrayList<MathTransform>();
    	ArrayList<MathTransform> sourceToGeoXforms = new ArrayList<MathTransform>();

    	MathTransform targetGtoCRS = targetGG.getGridToCRS(PixelInCell.CELL_CENTER);
    	MathTransform sourceCRStoG = sourceGC.getGridGeometry().getCRSToGrid2D(PixelOrientation.CENTER);
    	CoordinateReferenceSystem targetCRS = targetGG.getCoordinateReferenceSystem();
    	CoordinateReferenceSystem sourceCRS = sourceGC.getCoordinateReferenceSystem();
    	
    	transforms.add(targetGtoCRS);
    	if (! CRS.equalsIgnoreMetadata(sourceCRS, targetCRS)) {
    		GeographicCRS sourceGeoCRS = null;
    		GeographicCRS targetGeoCRS = null;
    		if (sourceCRS instanceof ProjectedCRS) {
    			sourceGeoCRS = ((ProjectedCRS) sourceCRS).getBaseCRS();
    		}
    		if (targetCRS instanceof ProjectedCRS) {
    			targetGeoCRS = ((ProjectedCRS) targetCRS).getBaseCRS();
    		}
    		try {
				transforms.add(CRS.findMathTransform(targetCRS, targetGeoCRS, true));
				toGeoXforms.addAll(transforms);
	    		if (CRS.equalsIgnoreMetadata(sourceGeoCRS, targetGeoCRS)) {
	    			// nothing...
	    		} else {
	    			transforms.add(CRS.findMathTransform(targetGeoCRS, sourceGeoCRS));
	    		}
				transforms.add(CRS.findMathTransform(sourceGeoCRS, sourceCRS, true));
				sourceToGeoXforms.add(0, CRS.findMathTransform(sourceCRS, sourceGeoCRS));
    		} catch (FactoryException e) {
    			statusHandler.error(e.getMessage(), e);
    			return output;
    		}
    	}
    	transforms.add(sourceCRStoG);
    	sourceToGeoXforms.add(0, sourceGC.getGridGeometry().getGridToCRS(PixelInCell.CELL_CENTER));

    	MathTransform mt;
		try {
			mt = concatenateTransforms(transforms);
		} catch (FactoryException e) {
			statusHandler.error(e.getMessage(), e);
			return output;
		}
		
    	double[] coord = new double[2];
    	for (int y = 0; y < targetHeight; ++y) {
    		for (int x = 0; x < targetWidth; ++x) {
    			coord[0] = x;
    			coord[1] = y;
    	    	try {
    	    		mt.transform(coord, 0, coord, 0, 1);
    	    	} catch (TransformException e) {
					continue;
				}
    	    	
    	    	int sx = (int) Math.round(coord[0]);
    	    	int sy = (int) Math.round(coord[1]);
    	    	if (sx >= 0 && sx < sourceWidth && sy >= 0 && sy < sourceHeight)
    	    		output[y * targetWidth + x] = sourceData[sy][sx]; 
    		}
    	}
    	
    	return output;
    }
    
	private static MathTransform concatenateTransforms(ArrayList<MathTransform> transforms) throws FactoryException {
    	Hints hints = new Hints();
        final CoordinateOperationFactory factory =
            ReferencingFactoryFinder.getCoordinateOperationFactory(hints);
	    final MathTransformFactory mtFactory;
	    if (factory instanceof AbstractCoordinateOperationFactory) {
	        mtFactory = ((AbstractCoordinateOperationFactory) factory).getMathTransformFactory();
	    } else {
	        mtFactory = ReferencingFactoryFinder.getMathTransformFactory(hints);
	    }
	    
    	MathTransform mt = null;
    	for (MathTransform mti : transforms) {
    		if (mt == null)
    			mt = mti;
    		else {
				mt = mtFactory.createConcatenatedTransform(mt, mti);
    		}
    	}
    	
    	return mt != null ? mt : IdentityTransform.create(2);
    }

}
