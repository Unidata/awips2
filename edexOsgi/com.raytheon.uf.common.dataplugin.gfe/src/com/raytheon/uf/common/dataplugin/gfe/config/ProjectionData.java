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
package com.raytheon.uf.common.dataplugin.gfe.config;

import java.awt.Point;
import java.util.HashMap;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Transient;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * ProjectionData class containing all the data any Projection could want.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/13/08     #1030      randerso    Initial port
 * 04/24/08     #1047      randerso    Made all fields private and created getters
 * 04/24/13     #1935      randerso    Fixed date line spanning issue
 * 08/06/13     #1571      randerso    Added hibernate annotations
 *                                     Removed constructor with int for ProjectionType
 * 10/22/13     #2361      njensen     Remove XML annotations
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@Embeddable
@DynamicSerialize
public class ProjectionData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProjectionData.class);

    private static float COMPARISON_THRESHOLD = 0.005f;

    /**
     * Enumeration of supported map projections
     */
    public static enum ProjectionType {
        /** Undefined map projection */
        NONE,

        /** Lambert Conformal map projection */
        LAMBERT_CONFORMAL,

        /** MERCATOR map projection */
        MERCATOR,

        /** North polar stereographic map projection */
        POLAR_STEREOGRAPHIC,

        /** Lat/Lon (implemented as Equidistant Cylindrical) map projection */
        LATLON
    };

    @Column(length = 32, nullable = false)
    @DynamicSerializeElement
    private String projectionID;

    @Column(length = 20, nullable = false)
    @Enumerated(EnumType.STRING)
    @DynamicSerializeElement
    private ProjectionType projectionType;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Coordinate latLonLL;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Coordinate latLonUR;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Coordinate latLonOrigin;

    @Column(nullable = false)
    @DynamicSerializeElement
    private double stdParallelOne;

    @Column(nullable = false)
    @DynamicSerializeElement
    private double stdParallelTwo;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Point gridPointLL;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Point gridPointUR;

    @Column(nullable = false)
    @DynamicSerializeElement
    private double latIntersect;

    @Column(nullable = false)
    @DynamicSerializeElement
    private double lonCenter;

    @Column(nullable = false)
    @DynamicSerializeElement
    private double lonOrigin;

    @Transient
    private CoordinateReferenceSystem crs;

    @Transient
    private GridGeometry2D gridGeometry;

    @Transient
    private MathTransform crsToLatLon;

    @Transient
    private MathTransform latLonToCrs;

    @Transient
    private HashMap<PixelOrientation, MathTransform> gridToLatLon;

    @Transient
    private HashMap<PixelOrientation, MathTransform> latLonToGrid;

    @Transient
    private HashMap<PixelOrientation, MathTransform> gridToCrs;

    @Transient
    private boolean initialized;

    /**
     * Default constructor
     */
    public ProjectionData() {
        latLonLL = new Coordinate();
        latLonUR = new Coordinate();
        latLonOrigin = new Coordinate();
        gridPointLL = new Point();
        gridPointUR = new Point();

        gridToLatLon = new HashMap<PixelOrientation, MathTransform>();
        latLonToGrid = new HashMap<PixelOrientation, MathTransform>();
        gridToCrs = new HashMap<PixelOrientation, MathTransform>();

        initialized = false;
    }

    /**
     * Constructor
     * 
     * @param projID
     *            name associated with this projection
     * @param projType
     *            projection type
     * @param latLonLL
     *            lat/lon of lower left corner
     * @param latLonUR
     *            lat/lon of upper right corner
     * @param latLonOrig
     *            lat/lon of origin (Lambert)
     * @param stdPar1
     *            standard parallel one (Lambert, Mercator)
     * @param stdPar2
     *            standard parallel two (Lambert)
     * @param gridLL
     *            coordinates of lower left grid cell, typically (1,1)
     * @param gridUR
     *            coordinates of upper right grid cell, typically (nX,nY)
     * @param latInt
     *            lat intersect (unused, preserved for A1 compatibility)
     * @param lonCenter
     *            lon center (Mercator, LatLon)
     * @param lonOrig
     *            lon origin (Lambert, Polar Stereographic)
     */
    public ProjectionData(String projID, ProjectionType projType,
            Coordinate latLonLL, Coordinate latLonUR, Coordinate latLonOrig,
            float stdPar1, float stdPar2, Point gridLL, Point gridUR,
            float latInt, float lonCenter, float lonOrig) {
        this();

        this.projectionID = projID;
        this.projectionType = projType;
        this.latLonLL = latLonLL;
        this.latLonUR = latLonUR;
        this.latLonOrigin = latLonOrig;
        this.stdParallelOne = stdPar1;
        this.stdParallelTwo = stdPar2;
        this.gridPointLL = gridLL;
        this.gridPointUR = gridUR;
        this.latIntersect = latInt;
        this.lonCenter = lonCenter;
        this.lonOrigin = lonOrig;

        init();
    }

    /**
     * Initialize the object. Must be called after database retrieval
     */
    public void init() {
        if (initialized) {
            return;
        }

        try {
            // transform the projection corner points to CRS units
            MathTransform mt = MapUtil.getTransformFromLatLon(getCrs());
            double[] output = new double[4];
            mt.transform(new double[] { getLatLonLL().x, getLatLonLL().y,
                    getLatLonUR().x, getLatLonUR().y }, 0, output, 0, 2);

            // create a grid geometry for the projection
            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(getCrs());
            ge.setRange(0, Math.min(output[0], output[2]),
                    Math.max(output[0], output[2]));
            ge.setRange(1, Math.min(output[1], output[3]),
                    Math.max(output[1], output[3]));

            // NOTE: the LL + 1 is a kludge to work around an apparent geotools
            // issue
            GeneralGridEnvelope gr = new GeneralGridEnvelope(new int[] {
                    getGridPointLL().x + 1, getGridPointLL().y + 1 },
                    new int[] { getGridPointUR().x, getGridPointUR().y }, true);

            GridToEnvelopeMapper mapper = new GridToEnvelopeMapper();
            mapper.setEnvelope(ge);
            mapper.setGridRange(gr);
            mapper.setPixelAnchor(PixelInCell.CELL_CENTER);
            mapper.setReverseAxis(new boolean[] { false, false });
            mt = mapper.createTransform();

            gridGeometry = new GridGeometry2D(PixelInCell.CELL_CORNER, mt, ge,
                    null);

            crsToLatLon = MapUtil.getTransformToLatLon(getCrs());
            latLonToCrs = MapUtil.getTransformFromLatLon(getCrs());

            initialized = true;

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String result = "{";

        result += projectionType + ", ";
        result += latLonLL + ", ";
        result += latLonUR + ", ";
        result += latLonOrigin + ", ";
        result += stdParallelOne + ", ";
        result += stdParallelTwo + ", ";
        result += gridPointLL + ", ";
        result += gridPointUR + ", ";
        result += latIntersect + ", ";
        result += lonCenter + ", ";
        result += lonOrigin;

        result += "}";
        return result;
    }

    /**
     * Retrieve the Coordinate Reference System (CRS) of this projection
     * 
     * @return the CRS
     */
    public synchronized CoordinateReferenceSystem getCrs() {
        if (crs == null) {

            // construct the appropriate CRS based on the projection type
            switch (this.projectionType) {
            case LAMBERT_CONFORMAL:
                crs = MapUtil.constructLambertConformal(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        this.stdParallelOne, this.stdParallelTwo,
                        this.latLonOrigin.x);
                break;

            case MERCATOR:
                crs = MapUtil.constructMercator(MapUtil.AWIPS_EARTH_RADIUS,
                        MapUtil.AWIPS_EARTH_RADIUS, this.stdParallelOne,
                        this.lonCenter);
                break;

            case POLAR_STEREOGRAPHIC:
                crs = MapUtil.constructNorthPolarStereo(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        60.0, this.lonOrigin);
                break;

            case LATLON:
                crs = MapUtil.constructEquidistantCylindrical(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        this.lonCenter, 0);
                break;

            case NONE:
            default:
                System.out.println("ERROR: unknown projection type: "
                        + this.projectionType);
            }
        }

        return crs;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ProjectionData)) {
            return false;
        }

        ProjectionData rhs = (ProjectionData) obj;
        if (!this.getProjectionType().equals(rhs.getProjectionType())) {
            return false;
        }

        if ((Math.abs(this.latLonLL.y - rhs.latLonLL.y) > COMPARISON_THRESHOLD)
                || (Math.abs(this.latLonLL.x - rhs.latLonLL.x) > COMPARISON_THRESHOLD)
                || (Math.abs(this.latLonUR.y - rhs.latLonUR.y) > COMPARISON_THRESHOLD)
                || (Math.abs(this.latLonLL.x - rhs.latLonLL.x) > COMPARISON_THRESHOLD)) {
            return false;
        }

        // specific projection comparisons
        switch (this.projectionType) {
        case LAMBERT_CONFORMAL:
            return ((Math.abs(this.latLonOrigin.y - rhs.latLonOrigin.y) < COMPARISON_THRESHOLD)
                    || (Math.abs(this.latLonOrigin.x - rhs.latLonOrigin.x) < COMPARISON_THRESHOLD)
                    || (Math.abs(this.stdParallelOne - rhs.stdParallelOne) < COMPARISON_THRESHOLD) || (Math
                    .abs(this.stdParallelTwo - rhs.stdParallelTwo) < COMPARISON_THRESHOLD));
        case POLAR_STEREOGRAPHIC:
            return (Math.abs(this.lonOrigin - rhs.lonOrigin) < COMPARISON_THRESHOLD);
        case MERCATOR:
            return (Math.abs(this.lonCenter - rhs.lonCenter) < COMPARISON_THRESHOLD);
        case LATLON:
            return true;
        default:
            return false;
        }
    }

    /**
     * Return the lat/lon of the specified corner or center of a grid cell
     * 
     * @param gridCoord
     *            coordinates of the grid cell
     * @param orientation
     *            desired corner or center
     * @return the lat/lon
     */
    public Coordinate gridCoordinateToLatLon(Coordinate gridCoord,
            PixelOrientation orientation) {
        Coordinate latLon = new Coordinate();
        MathTransform mt = gridToLatLon.get(orientation);
        try {
            if (mt == null) {
                init();
                DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                mt = dmtf.createConcatenatedTransform(
                        gridGeometry.getGridToCRS(orientation), crsToLatLon);
                gridToLatLon.put(orientation, mt);
            }

            double[] output = new double[2];
            mt.transform(new double[] { gridCoord.x, gridCoord.y }, 0, output,
                    0, 1);
            latLon.x = output[0];
            latLon.y = output[1];
        } catch (Exception e) {
            statusHandler.error(
                    "Error computing grid coordinate to lat/lon transform", e);
        }
        return latLon;
    }

    /**
     * Return the math transform from grid coordinate to the specified corner or
     * center lat/lon
     * 
     * @param orientation
     *            desired corner or center
     * @return the transform
     */
    public MathTransform getGridToCrs(PixelOrientation orientation) {
        MathTransform mt = gridToCrs.get(orientation);
        if (mt == null) {
            init();
            mt = gridGeometry.getGridToCRS(orientation);
            gridToCrs.put(orientation, mt);
        }
        return mt;
    }

    /**
     * Convert a grid cell coordinate to the native CRS coordinate of this
     * projection
     * 
     * @param gridCoord
     *            grid cell coordinate
     * @param orientation
     *            desired corner or center of the grid cell
     * @return native CRS coordinate
     */
    public Coordinate gridCoordinateToCrs(Coordinate gridCoord,
            PixelOrientation orientation) {
        Coordinate crsCoordinate = new Coordinate();
        MathTransform mt = getGridToCrs(orientation);
        try {
            double[] output = new double[2];
            mt.transform(new double[] { gridCoord.x, gridCoord.y }, 0, output,
                    0, 1);
            crsCoordinate.x = output[0];
            crsCoordinate.y = output[1];
        } catch (Exception e) {
            statusHandler.error(
                    "Error computing grid coordinate to CRS transform", e);
        }
        return crsCoordinate;
    }

    /**
     * @return the projectionID
     */
    public String getProjectionID() {
        return projectionID;
    }

    /**
     * @return the projectionType
     */
    public ProjectionType getProjectionType() {
        return projectionType;
    }

    /**
     * @return the latLonLL
     */
    public Coordinate getLatLonLL() {
        return latLonLL;
    }

    /**
     * @return the latLonUR
     */
    public Coordinate getLatLonUR() {
        return latLonUR;
    }

    /**
     * @return the latLonOrigin
     */
    public Coordinate getLatLonOrigin() {
        return latLonOrigin;
    }

    /**
     * @return the stdParallelOne
     */
    public double getStdParallelOne() {
        return stdParallelOne;
    }

    /**
     * @return the stdParallelTwo
     */
    public double getStdParallelTwo() {
        return stdParallelTwo;
    }

    /**
     * @return the gridPointLL
     */
    public Point getGridPointLL() {
        return gridPointLL;
    }

    /**
     * @return the gridPointUR
     */
    public Point getGridPointUR() {
        return gridPointUR;
    }

    /**
     * @return the latIntersect
     */
    public double getLatIntersect() {
        return latIntersect;
    }

    /**
     * @return the lonCenter
     */
    public double getLonCenter() {
        return lonCenter;
    }

    /**
     * @return the lonOrigin
     */
    public double getLonOrigin() {
        return lonOrigin;
    }

    /**
     * @param projectionID
     *            the projectionID to set
     */
    public void setProjectionID(String projectionID) {
        this.projectionID = projectionID;
    }

    /**
     * @param projectionType
     *            the projectionType to set
     */
    public void setProjectionType(ProjectionType projectionType) {
        this.projectionType = projectionType;
    }

    /**
     * @param latLonLL
     *            the latLonLL to set
     */
    public void setLatLonLL(Coordinate latLonLL) {
        this.latLonLL = latLonLL;
    }

    /**
     * @param latLonUR
     *            the latLonUR to set
     */
    public void setLatLonUR(Coordinate latLonUR) {
        this.latLonUR = latLonUR;
    }

    /**
     * @param latLonOrigin
     *            the latLonOrigin to set
     */
    public void setLatLonOrigin(Coordinate latLonOrigin) {
        this.latLonOrigin = latLonOrigin;
    }

    /**
     * @param stdParallelOne
     *            the stdParallelOne to set
     */
    public void setStdParallelOne(double stdParallelOne) {
        this.stdParallelOne = stdParallelOne;
    }

    /**
     * @param stdParallelTwo
     *            the stdParallelTwo to set
     */
    public void setStdParallelTwo(double stdParallelTwo) {
        this.stdParallelTwo = stdParallelTwo;
    }

    /**
     * @param gridPointLL
     *            the gridPointLL to set
     */
    public void setGridPointLL(Point gridPointLL) {
        this.gridPointLL = gridPointLL;
    }

    /**
     * @param gridPointUR
     *            the gridPointUR to set
     */
    public void setGridPointUR(Point gridPointUR) {
        this.gridPointUR = gridPointUR;
    }

    /**
     * @param latIntersect
     *            the latIntersect to set
     */
    public void setLatIntersect(double latIntersect) {
        this.latIntersect = latIntersect;
    }

    /**
     * @param lonCenter
     *            the lonCenter to set
     */
    public void setLonCenter(double lonCenter) {
        this.lonCenter = lonCenter;
    }

    /**
     * @param lonOrigin
     *            the lonOrigin to set
     */
    public void setLonOrigin(double lonOrigin) {
        this.lonOrigin = lonOrigin;
    }

    /**
     * @return the width of the projection in grid cells
     */
    public Integer getNx() {
        return (this.gridPointUR.x - this.gridPointLL.x) + 1;
    }

    /**
     * @return the height of the projection in grid cells
     */
    public Integer getNy() {
        return (this.gridPointUR.y - this.gridPointLL.y) + 1;
    }

}
