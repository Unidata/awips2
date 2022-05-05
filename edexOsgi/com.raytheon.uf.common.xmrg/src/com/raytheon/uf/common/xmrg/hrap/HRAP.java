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
package com.raytheon.uf.common.xmrg.hrap;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.HashMap;
import java.util.Map;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import org.locationtech.jts.geom.Coordinate;

/**
 * Defines the HRAP spatial coordinate system
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2008            randerso    Initial creation
 * Mar 31, 2014   2689     mpduff      Log input values on conversion failure.
 * Dec 09, 2015  18391     snaples     Updated gridmapper to use CELL CENTER instead of corner.
 * Apr 19, 2016  18865     snaples     Updated gridmapper to correct an offset in the grid to point mapping.
 *                                     Using CELL_CORNER now to fix that issue.
 * Dec 05, 2016  19589     snaples     Updated gridmapper to use CELL CENTER instead of corner.
 * Feb 28, 2017  19733     snaples     Updated to use transforms from MapUtil class
 * Sep 15, 2017  6407      bkowal      Minor cleanup.
 * 
 * </pre>
 * 
 * @author randerso
 */

public class HRAP {

    private static float COMPARISON_THRESHOLD = 0.005f;

    private static Map<Integer, HRAP> instances = new HashMap<>();

    /**
     * This is sort of nasty but it is meant to allow for full grid XMRG files
     * 
     * @return
     */
    public static synchronized HRAP getInstance() {
        return getInstance(1);
    }

    /**
     * Factor to increase grid resolution by
     * 
     * @param gridResolution
     * @return
     */
    public static synchronized HRAP getInstance(int gridResolution) {
        HRAP instance = instances.get(gridResolution);

        if (instance == null) {
            instance = new HRAP(gridResolution);
            instances.put(gridResolution, instance);
        }

        return instance;
    }

    private String projectionID;

    private Coordinate latLonUR;

    private Coordinate latLonLL;

    private Coordinate latLonOrigin;

    private double stdParallelOne;

    private double stdParallelTwo;

    private Point gridPointLL;

    private Point gridPointUR;

    private double latIntersect;

    private double lonCenter;

    private double lonOrigin;

    private CoordinateReferenceSystem crs;

    private final Map<PixelOrientation, MathTransform> toLatLonMap;

    private final Map<PixelOrientation, MathTransform> fromLatLonMap;

    private GridToEnvelopeMapper gridMapper;

    private GridGeometry2D gridGeometry;

    private HRAP(int gridResolution) {
        this.toLatLonMap = new HashMap<>();
        this.fromLatLonMap = new HashMap<>();

        this.projectionID = "HRAP";
        this.latLonLL = new Coordinate(-119.03624346792651, 23.09739145101471);
        this.latLonUR = new Coordinate(-60.0, 45.61982902388795);
        this.latLonOrigin = new Coordinate(0, 0);
        this.stdParallelOne = 0.0;
        this.stdParallelTwo = 0.0;
        this.gridPointLL = new Point(1, 1);
        this.gridPointUR = new Point(1121 * gridResolution,
                881 * gridResolution);
        this.latIntersect = 0.0;
        this.lonCenter = 0.0;
        this.lonOrigin = -105.0;
    }

    @Override
    public String toString() {
        String result = "{";

        result += latLonLL + ", ";
        result += latLonUR + ", ";
        result += latLonOrigin + ", ";
        result += Double.toString(stdParallelOne) + ", ";
        result += Double.toString(stdParallelTwo) + ", ";
        result += gridPointLL + ", ";
        result += gridPointUR + ", ";
        result += Double.toString(latIntersect) + ", ";
        result += Double.toString(lonCenter) + ", ";
        result += Double.toString(lonOrigin);

        result += "}";
        return result;
    }

    /**
     * @return
     */
    public CoordinateReferenceSystem getCrs() {
        if (crs == null) {
            crs = MapUtil.constructNorthPolarStereo(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, 60.0, this.lonOrigin);
        }
        return crs;
    }

    public GridToEnvelopeMapper getGridMapper() throws HrapConversionException {
        if (gridMapper == null) {
            try {
                // transform the projection corner points to CRS units
                MathTransform mt = MapUtil.getTransformFromLatLon(getCrs());
                double[] output = new double[4];
                mt.transform(
                        new double[] { getLatLonLL().x, getLatLonLL().y,
                                getLatLonUR().x, getLatLonUR().y },
                        0, output, 0, 2);

                // create a grid geometry for the projection
                GeneralEnvelope userRange = new GeneralEnvelope(2);
                userRange.setCoordinateReferenceSystem(getCrs());
                userRange.setRange(0, Math.min(output[0], output[2]),
                        Math.max(output[0], output[2]));
                userRange.setRange(1, Math.min(output[1], output[3]),
                        Math.max(output[1], output[3]));

                GeneralGridEnvelope gridRange = new GeneralGridEnvelope(
                        new int[] { getGridPointLL().x, getGridPointLL().y },
                        new int[] { getGridPointUR().x, getGridPointUR().y },
                        false);

                gridMapper = new GridToEnvelopeMapper(gridRange, userRange);
                gridMapper.setEnvelope(userRange);
                gridMapper.setGridRange(gridRange);
                gridMapper.setPixelAnchor(PixelInCell.CELL_CORNER);
                gridMapper.setReverseAxis(new boolean[] { false, false });

            } catch (Exception e) {
                throw new HrapConversionException(
                        "Unable to create HRAP grid mapper.", e);
            }
        }

        return gridMapper;
    }

    public GridGeometry2D getGridGeometry() throws HrapConversionException {
        if (gridGeometry == null) {
            gridGeometry = new GridGeometry2D(getGridMapper().getGridRange(),
                    getGridMapper().createTransform(), getCrs());
        }

        return gridGeometry;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((crs == null) ? 0 : crs.hashCode());
        result = prime * result
                + ((fromLatLonMap == null) ? 0 : fromLatLonMap.hashCode());
        result = prime * result
                + ((gridGeometry == null) ? 0 : gridGeometry.hashCode());
        result = prime * result
                + ((gridMapper == null) ? 0 : gridMapper.hashCode());
        result = prime * result
                + ((gridPointLL == null) ? 0 : gridPointLL.hashCode());
        result = prime * result
                + ((gridPointUR == null) ? 0 : gridPointUR.hashCode());
        long temp;
        temp = Double.doubleToLongBits(latIntersect);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result
                + ((latLonLL == null) ? 0 : latLonLL.hashCode());
        result = prime * result
                + ((latLonOrigin == null) ? 0 : latLonOrigin.hashCode());
        result = prime * result
                + ((latLonUR == null) ? 0 : latLonUR.hashCode());
        temp = Double.doubleToLongBits(lonCenter);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(lonOrigin);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result
                + ((projectionID == null) ? 0 : projectionID.hashCode());
        temp = Double.doubleToLongBits(stdParallelOne);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(stdParallelTwo);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result
                + ((toLatLonMap == null) ? 0 : toLatLonMap.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj.getClass() == this.getClass())) {
            return false;
        }

        HRAP rhs = (HRAP) obj;
        if (Math.abs(this.latLonLL.y - rhs.latLonLL.y) > COMPARISON_THRESHOLD
                || Math.abs(
                        this.latLonLL.x - rhs.latLonLL.x) > COMPARISON_THRESHOLD
                || Math.abs(
                        this.latLonUR.y - rhs.latLonUR.y) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonLL.x
                        - rhs.latLonLL.x) > COMPARISON_THRESHOLD) {
            return false;
        }

        // specific projection comparisons
        return (Math
                .abs(this.lonOrigin - rhs.lonOrigin) < COMPARISON_THRESHOLD);
    }

    public String getProjectionID() {
        return projectionID;
    }

    public Coordinate getLatLonLL() {
        return latLonLL;
    }

    public Coordinate getLatLonUR() {
        return latLonUR;
    }

    public Coordinate getLatLonOrigin() {
        return latLonOrigin;
    }

    public double getStdParallelOne() {
        return stdParallelOne;
    }

    public double getStdParallelTwo() {
        return stdParallelTwo;
    }

    public Point getGridPointLL() {
        return gridPointLL;
    }

    public Point getGridPointUR() {
        return gridPointUR;
    }

    public double getLatIntersect() {
        return latIntersect;
    }

    public double getLonCenter() {
        return lonCenter;
    }

    public double getLonOrigin() {
        return lonOrigin;
    }

    public void setProjectionID(String projectionID) {
        this.projectionID = projectionID;
    }

    public void setLatLonLL(Coordinate latLonLL) {
        this.latLonLL = latLonLL;
    }

    public void setLatLonUR(Coordinate latLonUR) {
        this.latLonUR = latLonUR;
    }

    public void setLatLonOrigin(Coordinate latLonOrigin) {
        this.latLonOrigin = latLonOrigin;
    }

    public void setStdParallelOne(double stdParallelOne) {
        this.stdParallelOne = stdParallelOne;
    }

    public void setStdParallelTwo(double stdParallelTwo) {
        this.stdParallelTwo = stdParallelTwo;
    }

    public void setGridPointLL(Point gridPointLL) {
        this.gridPointLL = gridPointLL;
    }

    public void setGridPointUR(Point gridPointUR) {
        this.gridPointUR = gridPointUR;
    }

    public void setLatIntersect(double latIntersect) {
        this.latIntersect = latIntersect;
    }

    public void setLonCenter(double lonCenter) {
        this.lonCenter = lonCenter;
    }

    public void setLonOrigin(double lonOrigin) {
        this.lonOrigin = lonOrigin;
    }

    public HRAPSubGrid getHRAPSubGrid(Rectangle extent)
            throws HrapConversionException {
        HRAPSubGrid subGrid = new HRAPSubGrid(extent);
        return subGrid;
    }

    public HRAPSubGrid getHRAPSubGrid(Rectangle extent, int gribFactor)
            throws HrapConversionException {
        HRAPSubGrid subGrid = new HRAPSubGrid(extent, gribFactor);
        return subGrid;
    }

    private MathTransform getTransformToLatLon(PixelOrientation orientation)
            throws HrapConversionException {
        MathTransform toLatLon = toLatLonMap.get(orientation);
        if (toLatLon == null) {
            try {
                MathTransform gridToProj = getGridMapper().createTransform();
                MathTransform projToLatLon = MapUtil
                        .getTransformToLatLon(getCrs());
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                toLatLon = dmtf.createConcatenatedTransform(gridToProj,
                        projToLatLon);
            } catch (Exception e) {
                throw new HrapConversionException(
                        "Error creating HRAP to Lat/Lon transform", e);
            }
            toLatLonMap.put(orientation, toLatLon);
        }
        return toLatLon;
    }

    private MathTransform getTransformFromLatLon(PixelOrientation orientation)
            throws HrapConversionException {
        MathTransform fromLatLon = fromLatLonMap.get(orientation);
        if (fromLatLon == null) {
            try {
                MathTransform latLonToProj = MapUtil
                        .getTransformFromLatLon(getCrs());
                MathTransform projToGrid = getGridMapper().createTransform()
                        .inverse();
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                fromLatLon = dmtf.createConcatenatedTransform(latLonToProj,
                        projToGrid);
            } catch (Exception e) {
                throw new HrapConversionException(
                        "Error creating Lat/Lon to HRAP transform", e);
            }
            fromLatLonMap.put(orientation, fromLatLon);
        }
        return fromLatLon;
    }

    public Integer getNx() {
        return this.gridPointUR.x - gridPointLL.x + 1;
    }

    public Integer getNy() {
        return this.gridPointUR.y - gridPointLL.y + 1;
    }

    public Coordinate gridCoordinateToLatLon(Point point,
            PixelOrientation orientation) throws HrapConversionException {
        return gridCoordinateToLatLon(new Coordinate(point.x, point.y),
                orientation);
    }

    public Coordinate gridCoordinateToLatLon(Coordinate gridCoord,
            PixelOrientation orientation) throws HrapConversionException {
        Coordinate latLon = new Coordinate();
        MathTransform mt = getTransformToLatLon(orientation);

        double[] output = new double[2];
        try {
            mt.transform(new double[] { gridCoord.x, gridCoord.y }, 0, output,
                    0, 1);
        } catch (TransformException e) {
            throw new HrapConversionException(
                    "Failed to transform grid coordinate: "
                            + gridCoord.toString()
                            + " to a lat/lon coordinate.",
                    e);
        }
        latLon.x = output[0];
        latLon.y = output[1];

        return latLon;
    }

    public Coordinate latLonToGridCoordinate(Coordinate latLon,
            PixelOrientation orientation) throws HrapConversionException {
        Coordinate gridCell = new Coordinate();
        MathTransform mt = getTransformFromLatLon(orientation);
        double[] output = new double[2];
        try {
            mt.transform(new double[] { latLon.x, latLon.y }, 0, output, 0, 1);
        } catch (TransformException e) {
            throw new HrapConversionException(
                    "Failed to transform lat/lon coordinate: "
                            + latLon.toString() + " to a grid coordinate.",
                    e);
        }
        gridCell.x = output[0];
        gridCell.y = output[1];

        return gridCell;
    }
}
