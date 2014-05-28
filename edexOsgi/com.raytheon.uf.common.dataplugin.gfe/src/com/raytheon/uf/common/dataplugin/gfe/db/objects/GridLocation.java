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
package com.raytheon.uf.common.dataplugin.gfe.db.objects;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import jep.INumpyable;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData.ProjectionType;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.GridUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.operation.buffer.BufferParameters;
import com.vividsolutions.jts.simplify.TopologyPreservingSimplifier;

/**
 * Contains spatial definition for GFE grids
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/24/08       @1047     randerso    Added fields to store projection information
 * 10/10/12      #1260      randerso    Added new constructor that takes a GridCoverage
 * 07/10/13      #2044      randerso    Changed constructor to take ISpatialObject instead of GridCoverage
 * 07/16/13      #2181      bsteffen    Convert geometry types to use hibernate-
 *                                      spatial
 * 08/06/13      #1571      randerso    Added hibernate annotations, javadoc cleanup, 
 *                                      made init method public for use in GFEDao
 * 09/30/13      #2333      mschenke    Added method to construct from {@link IGridGeometryProvider}
 * 10/22/13      #2361      njensen     Remove XML annotations
 * 04/11/14      #2947      bsteffen    Remove ISpatialObject constructor.
 * 05/06/14      #3118      randerso    Added clone() method
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@Entity
@Table(name = "gfe_gridlocation", uniqueConstraints = { @UniqueConstraint(columnNames = { "dbId_id" }) })
@DynamicSerialize
public class GridLocation extends PersistableDataObject<String> implements
        ISpatialObject, Cloneable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridLocation.class);

    /**
     * Container for lat/lon grids to be returned to python where they are
     * reshaped using numpy for performance
     * 
     */
    private static class PythonNumpyLatLonGrid implements INumpyable {
        private float[] data;

        public PythonNumpyLatLonGrid(int nx, int ny, float[] data) {
            if ((nx * ny * 2) != data.length) {
                throw new IllegalArgumentException(
                        "data must be of length nx*ny*2");
            }
            this.data = data;
        }

        @Override
        public Object[] getNumpy() {
            return new Object[] { data };
        }

        @Override
        public int getNumpyX() {
            return data.length;
        }

        /*
         * (non-Javadoc)
         * 
         * @see jep.INumpyable#getNumpyY()
         */
        @Override
        public int getNumpyY() {
            return 1;
        }

    }

    private static final long serialVersionUID = 1L;

    /**
     * Auto-generated surrogate key
     */
    @Id
    @SequenceGenerator(name = "GFE_GRIDLOCATION_GENERATOR", sequenceName = "gfe_gridlocation_seq")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GFE_GRIDLOCATION_GENERATOR")
    private int id;

    /** The database id associated with this grid location */
    @OneToOne(fetch = FetchType.EAGER, optional = false)
    @OnDelete(action = OnDeleteAction.CASCADE)
    @JoinColumn(referencedColumnName = "id", name = "dbId_id")
    private DatabaseID dbId;

    @Column(length = 8, nullable = false)
    @DynamicSerializeElement
    private String siteId;

    /** Number of points along the x-axis */
    @Column(nullable = false)
    @DynamicSerializeElement
    protected Integer nx;

    /** Number of points along the y-axis */
    @Column(nullable = false)
    @DynamicSerializeElement
    protected Integer ny;

    @Column(length = 32, nullable = false)
    @DynamicSerializeElement
    private String timeZone;

    @Transient
    private Coordinate gridCellSize;

    @Embedded
    @DynamicSerializeElement
    private ProjectionData projection;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Coordinate origin;

    @Column(nullable = false)
    @DynamicSerializeElement
    private Coordinate extent;

    @Transient
    @DynamicSerializeElement
    private Polygon geometry;

    @Transient
    @DynamicSerializeElement
    private String crsWKT;

    @Transient
    private CoordinateReferenceSystem crsObject;

    /**
     * Default constructor for serialization
     */
    public GridLocation() {

    }

    /**
     * Constructor
     * 
     * @param id
     * @param proj
     * @param gridSize
     * @param domainOrigin
     * @param domainExtent
     * @param timeZone
     */
    public GridLocation(String id, ProjectionData proj,
            java.awt.Point gridSize, Coordinate domainOrigin,
            Coordinate domainExtent, String timeZone) {
        if ((id == null) || id.isEmpty()) {
            throw new IllegalArgumentException("id may not be null or empty");
        }
        this.siteId = id;
        this.nx = gridSize.x;
        this.ny = gridSize.y;
        this.projection = proj;
        this.origin = domainOrigin;
        this.extent = domainExtent;
        this.timeZone = timeZone;

        init();
    }

    /**
     * Copy constructor
     * 
     * @param gridLocation
     */
    public GridLocation(GridLocation other) {
        // don't copy id or dbId
        this.siteId = other.siteId;
        this.nx = other.nx;
        this.ny = other.ny;
        this.timeZone = other.timeZone;
        this.projection = other.projection;
        this.origin = other.origin == null ? null : (Coordinate) other.origin
                .clone();
        this.extent = other.extent == null ? null : (Coordinate) other.extent
                .clone();
        this.geometry = (Polygon) other.geometry.clone();
        this.crsWKT = other.crsWKT;
        this.crsObject = other.crsObject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public GridLocation clone() {
        return new GridLocation(this);
    }

    /**
     * Initialize the object. Must be called after database retrieval
     */
    /**
     * 
     */
    public void init() {
        try {
            this.projection.init();
            this.crsObject = this.projection.getCrs();
            this.crsWKT = this.crsObject.toWKT();

            // This is here to help find issues where the WKT won't parse.
            // This happened with Lambert Conformal after the GeoTools 2.6.4
            // update. It can be commented out if necessary for performance.
            try {
                CRSCache.getInstance()
                        .getCoordinateReferenceSystem(this.crsWKT);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error parsing WKT: "
                        + e.getLocalizedMessage(), e);
            }

            // transform the grid corners from grid coordinates to CRS units
            Coordinate ll = this.origin;
            Coordinate ur = new Coordinate(this.origin.x + this.extent.x,
                    this.origin.y + this.extent.y);

            Coordinate llCrs = this.projection.gridCoordinateToCrs(ll,
                    PixelOrientation.CENTER);
            Coordinate urCrs = this.projection.gridCoordinateToCrs(ur,
                    PixelOrientation.CENTER);

            // construct the grid geometry that covers the GFE grid
            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(this.crsObject);
            ge.setRange(0, Math.min(llCrs.x, urCrs.x),
                    Math.max(llCrs.x, urCrs.x));
            ge.setRange(1, Math.min(llCrs.y, urCrs.y),
                    Math.max(llCrs.y, urCrs.y));

            GeneralGridEnvelope gr = new GeneralGridEnvelope(
                    new int[] { 1, 1 }, new int[] { this.nx, this.ny }, false);

            GridToEnvelopeMapper mapper = new GridToEnvelopeMapper();
            mapper.setEnvelope(ge);
            mapper.setGridRange(gr);
            mapper.setPixelAnchor(PixelInCell.CELL_CENTER);
            mapper.setReverseAxis(new boolean[] { false, false });
            MathTransform mt = mapper.createTransform();

            GridGeometry2D gridGeom = new GridGeometry2D(
                    PixelInCell.CELL_CORNER, mt, ge, null);

            // set up the transform from grid coordinates to lon/lat
            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            mt = dmtf.createConcatenatedTransform(
                    gridGeom.getGridToCRS(PixelOrientation.UPPER_LEFT),
                    MapUtil.getTransformToLatLon(crsObject));

            // transform grid corner points to Lat/Lon
            double[] latLon = new double[8];
            mt.transform(new double[] { 0, this.ny, 0, 0, this.nx, 0, this.nx,
                    this.ny }, 0, latLon, 0, 4);

            Coordinate[] corners = new Coordinate[] {
                    MapUtil.getCoordinate(latLon[0], latLon[1]),
                    MapUtil.getCoordinate(latLon[2], latLon[3]),
                    MapUtil.getCoordinate(latLon[4], latLon[5]),
                    MapUtil.getCoordinate(latLon[6], latLon[7]),
                    MapUtil.getCoordinate(latLon[0], latLon[1]) };

            this.geometry = MapUtil.getPolygon(corners);
        } catch (Exception e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error creating GridLocation", e);
        }
    }

    /**
     * @param proj
     */
    public GridLocation(ProjectionData proj) {
        this(
                proj.getProjectionID(), //
                proj, //
                new Point(
                        //
                        (proj.getGridPointUR().x - proj.getGridPointLL().x) + 1,
                        (proj.getGridPointUR().y - proj.getGridPointLL().y) + 1),
                new Coordinate(proj.getGridPointLL().x, proj.getGridPointLL().y),
                new Coordinate( //
                        proj.getGridPointUR().x - proj.getGridPointLL().x,
                        proj.getGridPointUR().y - proj.getGridPointLL().y),
                "GMT");
    }

    /**
     * @param id
     * @param provider
     */
    public GridLocation(String id, IGridGeometryProvider provider) {
        this.siteId = id;
        GridGeometry2D gridGeometry = provider.getGridGeometry();
        this.crsObject = gridGeometry.getCoordinateReferenceSystem();
        this.crsWKT = this.crsObject.toWKT();
        this.nx = gridGeometry.getGridRange().getSpan(0);
        this.ny = gridGeometry.getGridRange().getSpan(1);

        Envelope2D envelope = gridGeometry.getEnvelope2D();
        Coordinate ul = new Coordinate(envelope.getMinX(), envelope.getMinY());
        Coordinate ur = new Coordinate(envelope.getMaxX(), envelope.getMinY());
        Coordinate lr = new Coordinate(envelope.getMaxX(), envelope.getMaxY());
        Coordinate ll = new Coordinate(envelope.getMinX(), envelope.getMaxY());
        GeometryFactory gf = new GeometryFactory();
        Geometry crsPolygon = gf.createPolygon(
                gf.createLinearRing(new Coordinate[] { ul, ur, lr, ll, ul }),
                null);
        try {
            MathTransform crsToLL = MapUtil.getTransformToLatLon(crsObject);
            this.geometry = (Polygon) JTS.transform(crsPolygon, crsToLL);
        } catch (Exception e) {
            throw new IllegalArgumentException(
                    "GridGeometry provided does not support conversion to lat/lon");
        }
    }

    /**
     * @param id
     * @param gloc
     * @param subGrid
     */
    public GridLocation(String id, GridLocation gloc, Rectangle subGrid) {
        try {
            this.siteId = id;
            this.crsObject = gloc.crsObject;
            this.crsWKT = gloc.crsWKT;
            this.nx = subGrid.width;
            this.ny = subGrid.height;
            this.origin = new Coordinate(subGrid.x, subGrid.y);
            this.extent = new Coordinate(subGrid.width, subGrid.height);

            GridGeometry2D gridGeom = MapUtil.getGridGeometry(gloc);

            // set up the transform from grid coordinates to lon/lat
            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            MathTransform mt = dmtf.createConcatenatedTransform(
                    gridGeom.getGridToCRS(PixelOrientation.UPPER_LEFT),
                    MapUtil.getTransformToLatLon(crsObject));

            // transform grid corner points to Lat/Lon
            double[] latLon = new double[8];
            mt.transform(new double[] { subGrid.x, subGrid.y + subGrid.height,
                    subGrid.x, subGrid.y, subGrid.x + subGrid.width, subGrid.y,
                    subGrid.x + subGrid.width, subGrid.y + subGrid.height }, 0,
                    latLon, 0, 4);

            Coordinate[] corners = new Coordinate[] {
                    MapUtil.getCoordinate(latLon[0], latLon[1]),
                    MapUtil.getCoordinate(latLon[2], latLon[3]),
                    MapUtil.getCoordinate(latLon[4], latLon[5]),
                    MapUtil.getCoordinate(latLon[6], latLon[7]),
                    MapUtil.getCoordinate(latLon[0], latLon[1]) };

            this.geometry = MapUtil.getPolygon(corners);
        } catch (Exception e) {
            statusHandler.handle(Priority.CRITICAL,
                    "Error creating GridLocation", e);
        }
    }

    /**
     * @return the id
     */
    public int getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * @param dbId
     *            the dbId to set
     */
    public void setDbId(DatabaseID dbId) {
        this.dbId = dbId;
    }

    /**
     * @return the timeZone
     */
    public String getTimeZone() {
        return timeZone;
    }

    /**
     * @param timeZone
     *            the timeZone to set
     */
    public void setTimeZone(String timeZone) {
        this.timeZone = timeZone;
    }

    /**
     * @return the grid size
     */
    public java.awt.Point gridSize() {
        return new java.awt.Point(nx, ny);
    }

    //
    // /*
    // * (non-Javadoc)
    // *
    // * @see com.raytheon.edex.db.objects.SpatialAreaDataObject#getMapGeom()
    // */
    // @Override
    // public GridGeometry2D getMapGeom() {
    // if (mapGeom == null) {
    // try {
    // WKTReader reader = new WKTReader();
    // com.vividsolutions.jts.geom.Polygon geometry =
    // (com.vividsolutions.jts.geom.Polygon) reader
    // .read(this.geometry.toString());
    //
    // com.vividsolutions.jts.geom.Point[] points = new
    // com.vividsolutions.jts.geom.Point[geometry
    // .getExteriorRing().getNumPoints()];
    //
    // for (int i = 0; i < points.length; i++) {
    // points[i] = geometry.getExteriorRing().getPointN(i);
    // }
    //
    // GeneralEnvelope env2 = MapUtil.extractProjectedEnvelope(
    // getCrsObject(), points, MapUtil
    // .getTransformFromLatLon(getCrsObject()));
    //
    // mapGeom = new GridGeometry2D(new GeneralGridEnvelope(new int[] {
    // 0, 0 }, new int[] { nx, ny }, false), env2, new boolean[] {
    // false, false }, false);
    // } catch (Exception e) {
    // e.printStackTrace();
    // }
    // }
    // return mapGeom;
    // }

    /**
     * ConnectGridPoints fills any gaps found in the specified gridPoint array
     * such that all adjacent points in the array touch each other. This
     * function ensures that there are no duplicates and that no point is more
     * than one point away from the next point.
     * 
     * For each point, check the integer distance to the next point. If zero, do
     * nothing so we remove duplicate. If the distance is 1, append the point to
     * the output array. If the distance is more than one, calculate the x and y
     * component to the next point and generate the points in between. Check to
     * make sure each point is exactly one point away and save it.
     * 
     * @param points
     * @return the connected grid points array
     */
    public java.awt.Point[] connectGridPoints(java.awt.Point points[]) {
        if (points.length == 0) {
            return new java.awt.Point[0];
        }

        // Make the return array and append the first point
        ArrayList<java.awt.Point> filledGridPoints = new ArrayList<java.awt.Point>(
                points.length);

        filledGridPoints.add(points[0]);
        for (int i = 1; i < points.length; i++) {
            GridUtil.bresenham(points[i - 1], points[i], filledGridPoints);
        }

        return filledGridPoints.toArray(new java.awt.Point[filledGridPoints
                .size()]);
    }

    /**
     * Returns the size in kilometers for each grid cell.
     * 
     * @return grid cell size in kilometers
     */
    public Coordinate gridCellSize() {

        if (this.gridCellSize != null) {
            return this.gridCellSize;
        }

        int[] p0 = new int[] { this.gridSize().x / 2, this.gridSize().y / 2 };

        if (this.crsObject instanceof ProjectedCRS) {
            // Native units are meters

            MathTransform gridToCRS = MapUtil.getGridGeometry(this)
                    .getGridToCRS(PixelOrientation.CENTER);

            try {
                double[] out1 = new double[2];
                gridToCRS.transform(new double[] { p0[0], p0[1] }, 0, out1, 0,
                        1);
                double[] out2 = new double[2];
                gridToCRS.transform(new double[] { p0[0] + 1, p0[1] + 1 }, 0,
                        out2, 0, 1);

                this.gridCellSize = new Coordinate(
                        Math.abs(out1[0] - out2[0]) / 1000.0, Math.abs(out1[1]
                                - out2[1]) / 1000.0);
            } catch (TransformException e) {
                statusHandler.error(
                        "Error computing gridCellSize: "
                                + e.getLocalizedMessage(), e);
            }

        } else {

            // Not a projected CRS in meters, so calculate using geodetic
            // calculator
            GeodeticCalculator gc = new GeodeticCalculator(this.getCrs());
            Coordinate p0LatLon = MapUtil
                    .gridCoordinateToLatLon(new Coordinate(p0[0], p0[1]),
                            PixelOrientation.CENTER, this);

            Coordinate p1LatLon = MapUtil.gridCoordinateToLatLon(
                    new Coordinate(p0[0] + 1, p0[1]), PixelOrientation.CENTER,
                    this);

            gc.setStartingGeographicPoint(p0LatLon.x, p0LatLon.y);
            gc.setDestinationGeographicPoint(p1LatLon.x, p1LatLon.y);

            double distanceX = gc.getOrthodromicDistance();

            p1LatLon = MapUtil.gridCoordinateToLatLon(new Coordinate(p0[0],
                    p0[1] + 1), PixelOrientation.CENTER, this);

            gc.setStartingGeographicPoint(p0LatLon.x, p0LatLon.y);
            gc.setDestinationGeographicPoint(p1LatLon.x, p1LatLon.y);

            double distanceY = gc.getOrthodromicDistance();

            this.gridCellSize = new Coordinate(distanceX / 1000.0,
                    distanceY / 1000.0);
        }

        return this.gridCellSize;

    }

    /**
     * Returns a Grid2DBit with bits set along the path defined by points and
     * width defined by influence width.
     * 
     * @param llPts
     *            in lat/lon
     * @param influenceSize
     *            in cells
     * @return grid with all cells within influenceSize of the path set
     */
    public Grid2DBit gridCellSwath(Coordinate llPts[], double influenceSize) {
        return gridCellSwath(llPts, influenceSize, true);
    }

    /**
     * Returns a Grid2DBit with bits set along the path defined by points and
     * width defined by influence width.
     * 
     * @param points
     *            in lat/lon or grid cells
     * @param influenceSize
     *            in cells
     * @param convertFromLatLon
     *            true if points are in lat/lon, false if in grid cells
     * @return grid with all cells within influenceSize of the path set
     */
    public Grid2DBit gridCellSwath(Coordinate[] points, double influenceSize,
            boolean convertFromLatLon) {

        Coordinate[] gcPts = points;
        if (convertFromLatLon) {
            gcPts = new Coordinate[points.length];
            for (int i = 0; i < points.length; i++) {
                gcPts[i] = new Coordinate(points[i]);
            }
            MapUtil.latLonToGridCoordinate(gcPts, PixelOrientation.CENTER, this);
        }

        GeometryFactory gf = new GeometryFactory();
        LineString ls = gf.createLineString(gcPts);
        ls = (LineString) TopologyPreservingSimplifier.simplify(ls, 0.5);

        Geometry g = ls.buffer(influenceSize / 2, 8,
                BufferParameters.CAP_SQUARE);
        MultiPolygon mp = null;
        if (g instanceof com.vividsolutions.jts.geom.Polygon) {
            mp = gf.createMultiPolygon(new com.vividsolutions.jts.geom.Polygon[] { (com.vividsolutions.jts.geom.Polygon) g });
        } else if (g instanceof MultiPolygon) {
            mp = (MultiPolygon) g;
        }
        ReferenceData ref = new ReferenceData(this, null, mp,
                CoordinateType.GRID);

        return ref.getGrid();
    }

    /**
     * Compute grid cell coordinate containing a lat/lon
     * 
     * @param lat
     * @param lon
     * @return the grid cell coordinate
     */
    public Point gridCell(float lat, float lon) {
        return gridCoordinate(new Coordinate(lon, lat));
    }

    /**
     * Compute grid cell coordinate containing a lat/lon
     * 
     * @param lonLat
     * @return the grid cell coordinate
     */
    public Point gridCoordinate(Coordinate lonLat) {
        Coordinate gcf = MapUtil.latLonToGridCoordinate(lonLat,
                PixelOrientation.CENTER, this);
        int x = (int) (gcf.x > -0.5 ? gcf.x + 0.5 : gcf.x - 0.5);
        int y = (int) (gcf.y > -0.5 ? gcf.y + 0.5 : gcf.y - 0.5);
        return new Point(x, y);
    }

    /**
     * Compute the lat/lon coordinate at the center of a grid cell
     * 
     * @param gridCell
     * @return the lat/lon
     */
    public Coordinate latLonCenter(Coordinate gridCell) {
        return MapUtil.gridCoordinateToLatLon(gridCell,
                PixelOrientation.CENTER, this);
    }

    @Override
    public String toString() {
        String s = "[SiteID =" + siteId + ",ProjID="
                + getCrs().getName().getCode() + ",gridSize=(" + nx + ',' + ny
                + "),loc=" + this.geometry.getGeometryType();
        // if (proj())
        // s += "," + proj()->pdata();
        s += ']';
        return s;
    }

    /**
     * @return the projection
     */
    public ProjectionData getProjection() {
        return projection;
    }

    /**
     * @return the origin
     */
    public Coordinate getOrigin() {
        return origin;
    }

    /**
     * @return the extent
     */
    public Coordinate getExtent() {
        return extent;
    }

    /**
     * @return the serialVersionUID
     */
    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * @param projection
     *            the projection
     */
    public void setProjection(ProjectionData projection) {
        this.projection = projection;
    }

    /**
     * @param origin
     *            the origin
     */
    public void setOrigin(Coordinate origin) {
        this.origin = origin;
    }

    /**
     * @param extent
     *            the extent
     */
    public void setExtent(Coordinate extent) {
        this.extent = extent;
    }

    @Override
    public CoordinateReferenceSystem getCrs() {
        if (crsObject == null) {
            try {
                crsObject = CRSCache.getInstance()
                        .getCoordinateReferenceSystem(crsWKT);
            } catch (FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, "Error creating CRS: "
                        + e.getLocalizedMessage(), e);
                crsObject = null;
            }
        }
        return crsObject;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj == null) {
            return false;
        }

        if (getClass() != obj.getClass()) {
            return false;
        }

        GridLocation other = (GridLocation) obj;

        if (nx == null) {
            if (other.nx != null) {
                return false;
            }
        } else if (!nx.equals(other.nx)) {
            return false;
        }

        if (ny == null) {
            if (other.ny != null) {
                return false;
            }
        } else if (!ny.equals(other.ny)) {
            return false;
        }

        if (origin == null) {
            if (other.origin != null) {
                return false;
            }
        } else if (!closeEnough(origin, other.origin, 0.00005)) {
            return false;
        }

        if (extent == null) {
            if (other.extent != null) {
                return false;
            }
        } else if (!closeEnough(extent, other.extent, 0.00005)) {
            return false;
        }

        if (crsWKT == null) {
            if (other.crsWKT != null) {
                return false;
            }
        } else if (!crsWKT.equals(other.crsWKT)) {
            return false;
        }
        return true;
    }

    private boolean closeEnough(Coordinate c1, Coordinate c2, double tolerance) {
        if (Math.abs(c1.x - c2.x) > tolerance) {
            return false;
        }
        if (Math.abs(c1.y - c2.y) > tolerance) {
            return false;
        }
        return true;
    }

    /**
     * Ensures that the projection id is a valid id, and that the grid size is
     * at least 2 x 2, and the grid location is defined. If so, returns true.
     * Else, returns false.
     * 
     * @return Returns if the grid Location is valid.
     */
    public boolean isValid() {
        if (projection == null) {
            return false;
        }

        // grid size must be at least 2x2
        if ((nx < 2) || (ny < 2)) {
            return false;
        }

        // grid location must be defined
        if ((origin == null) || (extent == null)) {
            return false;
        }

        return true;
    }

    @Override
    public Polygon getGeometry() {
        return geometry;
    }

    @Override
    public Integer getNx() {
        return nx;
    }

    @Override
    public Integer getNy() {
        return ny;
    }

    /**
     * @return the siteId
     */
    public String getSiteId() {
        return siteId;
    }

    /**
     * @param siteId
     *            the siteId
     */
    public void setSiteId(String siteId) {
        this.siteId = siteId;
    }

    /**
     * @return the crsWKT
     */
    public String getCrsWKT() {
        return crsWKT;
    }

    /**
     * @param crsWKT
     *            the crsWKT
     */
    public void setCrsWKT(String crsWKT) {
        this.crsWKT = crsWKT;
    }

    /**
     * @return the crs
     */
    public CoordinateReferenceSystem getCrsObject() {
        return getCrs();
    }

    /**
     * @param crsObject
     *            the crs
     */
    public void setCrsObject(CoordinateReferenceSystem crsObject) {
        this.crsObject = crsObject;
    }

    /**
     * @param nx
     *            the nx
     */
    public void setNx(Integer nx) {
        this.nx = nx;
    }

    /**
     * @param ny
     *            the ny
     */
    public void setNy(Integer ny) {
        this.ny = ny;
    }

    /**
     * @param geometry
     */
    public void setGeometry(Geometry geometry) {
        this.geometry = (Polygon) geometry;
    }

    /**
     * @param geometry
     */
    public void setGeometry(Polygon geometry) {
        this.geometry = geometry;
    }

    /**
     * Returns a 1-dimensional lat/lon grid to be reshaped by numpy for
     * performance. End users should use the getLatLonGrids function in
     * SmartScript.py or MetLib.py
     * 
     * This function is not intended for use by Java code.
     * 
     * @return the lat/lon grid
     */
    public PythonNumpyLatLonGrid getLatLonGrid() {
        float[] gridCells = new float[2 * nx * ny];
        int i = 0;
        for (float x = 0; x < nx; x++) {
            for (float y = 0; y < ny; y++) {
                gridCells[i++] = x;
                gridCells[i++] = y;
            }
        }

        float[] latLon = new float[gridCells.length];
        try {
            MathTransform mt = MapUtil.getTransformToLatLon(
                    PixelOrientation.CENTER, this);
            mt.transform(gridCells, 0, latLon, 0, gridCells.length / 2);
        } catch (Exception e) {
            e.printStackTrace();
        }

        return new PythonNumpyLatLonGrid(nx, ny, latLon);
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        ProjectionData grid211 = new ProjectionData("Grid211",
                ProjectionType.LAMBERT_CONFORMAL, new Coordinate(-133.459,
                        12.190), new Coordinate(-49.385, 57.290),
                new Coordinate(-95.0, 25.0), 25.0f, 25.0f, new Point(1, 1),
                new Point(93, 65), 0.0f, 0.0f, 0.0f);

        // GridLocation gloc = new GridLocation("ABR", grid211,
        // new Point(145, 145), new Coordinate(45.0, 35.0),
        // new Coordinate(9, 9), "CST6CDT");

        GridLocation gloc = new GridLocation("OAX", grid211,
                new Point(145, 145), new Coordinate(45.0, 30.0),
                new Coordinate(9, 9), "CST6CDT");

        System.out.println(gloc.getSiteId());
        Coordinate gridCoord = new Coordinate();
        Coordinate latLon = new Coordinate();
        PixelOrientation orientation = PixelOrientation.CENTER;

        gridCoord.x = 0;
        gridCoord.y = 0;
        latLon = MapUtil.gridCoordinateToLatLon(gridCoord, orientation, gloc);
        System.out.println(gridCoord.x + "," + gridCoord.y + "  " + latLon);

        gridCoord.x = 0;
        gridCoord.y = gloc.getNy() - 1;
        latLon = MapUtil.gridCoordinateToLatLon(gridCoord, orientation, gloc);
        System.out.println(gridCoord.x + "," + gridCoord.y + "  " + latLon);

        gridCoord.x = gloc.getNx() - 1;
        gridCoord.y = gloc.getNy() - 1;
        latLon = MapUtil.gridCoordinateToLatLon(gridCoord, orientation, gloc);
        System.out.println(gridCoord.x + "," + gridCoord.y + "  " + latLon);

        gridCoord.x = gloc.getNx() - 1;
        gridCoord.y = 0;
        latLon = MapUtil.gridCoordinateToLatLon(gridCoord, orientation, gloc);
        System.out.println(gridCoord.x + "," + gridCoord.y + "  " + latLon);

        PythonNumpyLatLonGrid latLonGrid = gloc.getLatLonGrid();
        float[] data = (float[]) latLonGrid.getNumpy()[0];
        for (int x = 0; x < gloc.getNx(); x++) {
            for (int y = 0; y < gloc.getNy(); y++) {
                int idx = 2 * ((x * gloc.ny) + y);
                float lon = data[idx];
                float lat = data[idx + 1];
                System.out.println(x + "," + y + "  " + lon + ", " + lat);
            }
        }
    }

    /**
     * Returns a ReferenceData that represents the grid points defined by the
     * specified Grid2DBit.
     * 
     * @param grid
     *            the grid points
     * @return the reference data
     */
    public ReferenceData convertToReferenceData(Grid2DBit grid) {
        // Make a ReferenceData and return it
        return new ReferenceData(this, new ReferenceID("test"), grid);
    }
}
