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
import java.util.List;

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

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.builder.GridToEnvelopeMapper;
import org.hibernate.annotations.Columns;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;
import org.hibernate.annotations.Type;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LineString;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.operation.buffer.BufferParameters;
import org.locationtech.jts.simplify.TopologyPreservingSimplifier;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.config.ProjectionData;
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

import jep.NDArray;

/**
 * Contains spatial definition for GFE grids
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 24, 2008  @1047    randerso  Added fields to store projection information
 * Oct 10, 2012  1260     randerso  Added new constructor that takes a
 *                                  GridCoverage
 * Jul 10, 2013  2044     randerso  Changed constructor to take ISpatialObject
 *                                  instead of GridCoverage
 * Jul 16, 2013  2181     bsteffen  Convert geometry types to use hibernate-
 *                                  spatial
 * Aug 06, 2013  1571     randerso  Added hibernate annotations, javadoc
 *                                  cleanup, made init method public for use in
 *                                  GFEDao
 * Sep 30, 2013  2333     mschenke  Added method to construct from {@link
 *                                  IGridGeometryProvider}
 * Oct 22, 2013  2361     njensen   Remove XML annotations
 * Apr 11, 2014  2947     bsteffen  Remove ISpatialObject constructor.
 * May 06, 2014  3118     randerso  Added clone() method
 * May 14, 2014  3069     randerso  Changed to store math transforms and CRS
 *                                  instead of GridGeometry2D since GeoTools now
 *                                  changes the supplied math transform when
 *                                  creating GridGeometry2D
 * Apr 23, 2015  4259     njensen   Updated for new JEP API
 * Jan 04, 2018  7178     randerso  Change clone() to copy(). Code cleanup
 * Feb 11, 2020  7596     randerso  Use Coordinate2dType adapter
 *
 * </pre>
 *
 * @author randerso
 */
@Entity
@Table(name = "gfe_gridlocation", uniqueConstraints = {
        @UniqueConstraint(columnNames = { "dbId_id" }) })
@DynamicSerialize
public class GridLocation extends PersistableDataObject<String>
        implements ISpatialObject {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridLocation.class);

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

    @Columns(columns = { @Column(nullable = false, name = "origin_x"),
            @Column(nullable = false, name = "origin_y") })
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.Coordinate2DType")
    @DynamicSerializeElement
    private Coordinate origin;

    @Columns(columns = { @Column(nullable = false, name = "extent_x"),
            @Column(nullable = false, name = "extent_y") })
    @Type(type = "com.raytheon.uf.common.dataplugin.gfe.db.type.Coordinate2DType")
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
    public GridLocation(String id, ProjectionData proj, java.awt.Point gridSize,
            Coordinate domainOrigin, Coordinate domainExtent, String timeZone) {
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
     * @param other
     */
    public GridLocation(GridLocation other) {
        // don't copy id or dbId
        this.siteId = other.siteId;
        this.nx = other.nx;
        this.ny = other.ny;
        this.timeZone = other.timeZone;
        this.projection = other.projection;
        this.origin = other.origin == null ? null
                : (Coordinate) other.origin.clone();
        this.extent = other.extent == null ? null
                : (Coordinate) other.extent.clone();
        this.geometry = (Polygon) other.geometry.clone();
        this.crsWKT = other.crsWKT;
        this.crsObject = other.crsObject;
    }

    /**
     * @return a copy of this object
     */
    public GridLocation copy() {
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
                statusHandler.handle(Priority.PROBLEM,
                        "Error parsing WKT: " + e.getLocalizedMessage(), e);
            }

            // transform the grid corners from grid coordinates to CRS units
            Coordinate ll = this.origin;
            Coordinate ur = new Coordinate(this.origin.x + this.extent.x,
                    this.origin.y + this.extent.y);

            Coordinate llCrs = this.projection.gridCoordinateToCrs(ll);
            Coordinate urCrs = this.projection.gridCoordinateToCrs(ur);

            // construct the grid geometry that covers the GFE grid
            GeneralEnvelope ge = new GeneralEnvelope(2);
            ge.setCoordinateReferenceSystem(this.crsObject);
            ge.setRange(0, Math.min(llCrs.x, urCrs.x),
                    Math.max(llCrs.x, urCrs.x));
            ge.setRange(1, Math.min(llCrs.y, urCrs.y),
                    Math.max(llCrs.y, urCrs.y));

            // GeoTools 10.5 kludge to use nx-1, ny-1 non-inclusive
            GeneralGridEnvelope gr = new GeneralGridEnvelope(new int[] { 0, 0 },
                    new int[] { this.nx - 1, this.ny - 1 }, false);

            // GeoTools 10.5 kludge to use CELL_CORNER instead of CELL_CENTER
            GridToEnvelopeMapper mapper = new GridToEnvelopeMapper();
            mapper.setEnvelope(ge);
            mapper.setGridRange(gr);
            mapper.setPixelAnchor(PixelInCell.CELL_CORNER);
            mapper.setReverseAxis(new boolean[] { false, true });
            MathTransform gridToCrs = mapper.createTransform();

            // set up the transform from grid coordinates to lon/lat
            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
            MathTransform gridToLatLon = dmtf.createConcatenatedTransform(
                    gridToCrs, MapUtil.getTransformToLatLon(crsObject));
            // transform grid corner points to Lat/Lon
            double[] latLon = new double[8];
            double[] gridCells = new double[] { -0.5, -0.5, -0.5, this.ny - 0.5,
                    this.nx - 0.5, this.ny - 0.5, this.nx - 0.5, -0.5 };
            gridToLatLon.transform(gridCells, 0, latLon, 0, 4);

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
        this(proj.getProjectionID(), proj,
                new Point(
                        (proj.getGridPointUR().x - proj.getGridPointLL().x) + 1,
                        (proj.getGridPointUR().y - proj.getGridPointLL().y)
                                + 1),
                new Coordinate(proj.getGridPointLL().x,
                        proj.getGridPointLL().y),
                new Coordinate(
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
                    "GridGeometry provided does not support conversion to lat/lon",
                    e);
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
        List<java.awt.Point> filledGridPoints = new ArrayList<>(points.length);

        filledGridPoints.add(points[0]);
        for (int i = 1; i < points.length; i++) {
            GridUtil.bresenham(points[i - 1], points[i], filledGridPoints);
        }

        return filledGridPoints
                .toArray(new java.awt.Point[filledGridPoints.size()]);
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
                        Math.abs(out1[0] - out2[0]) / 1000.0,
                        Math.abs(out1[1] - out2[1]) / 1000.0);
            } catch (TransformException e) {
                statusHandler.error("Error computing gridCellSize: "
                        + e.getLocalizedMessage(), e);
            }

        } else {

            // Not a projected CRS in meters, so calculate using geodetic
            // calculator
            GeodeticCalculator gc = new GeodeticCalculator(this.getCrs());
            Coordinate p0LatLon = MapUtil.gridCoordinateToLatLon(
                    new Coordinate(p0[0], p0[1]), PixelOrientation.CENTER,
                    this);

            Coordinate p1LatLon = MapUtil.gridCoordinateToLatLon(
                    new Coordinate(p0[0] + 1, p0[1]), PixelOrientation.CENTER,
                    this);

            gc.setStartingGeographicPoint(p0LatLon.x, p0LatLon.y);
            gc.setDestinationGeographicPoint(p1LatLon.x, p1LatLon.y);

            double distanceX = gc.getOrthodromicDistance();

            p1LatLon = MapUtil.gridCoordinateToLatLon(
                    new Coordinate(p0[0], p0[1] + 1), PixelOrientation.CENTER,
                    this);

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
            MapUtil.latLonToGridCoordinate(gcPts, PixelOrientation.CENTER,
                    this);
        }

        GeometryFactory gf = new GeometryFactory();
        LineString ls = gf.createLineString(gcPts);
        ls = (LineString) TopologyPreservingSimplifier.simplify(ls, 0.5);

        Geometry g = ls.buffer(influenceSize / 2, 8,
                BufferParameters.CAP_SQUARE);
        MultiPolygon mp = null;
        if (g instanceof org.locationtech.jts.geom.Polygon) {
            mp = gf.createMultiPolygon(new org.locationtech.jts.geom.Polygon[] {
                    (org.locationtech.jts.geom.Polygon) g });
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
        return MapUtil.gridCoordinateToLatLon(gridCell, PixelOrientation.CENTER,
                this);
    }

    @Override
    public String toString() {
        String s = String.format(
                "[SiteID = %s, ProjID=%s, gridSize=(%d,%d), loc=[o=%s, e=%s]]",
                siteId, getCrs().getName().getCode(), nx, ny, this.origin,
                this.extent);
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
                statusHandler.handle(Priority.PROBLEM,
                        "Error creating CRS: " + e.getLocalizedMessage(), e);
                crsObject = null;
            }
        }
        return crsObject;
    }

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

    private boolean closeEnough(Coordinate c1, Coordinate c2,
            double tolerance) {
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
    public NDArray<float[]> getLatLonGrid() {
        float[] gridCells = new float[2 * nx * ny];
        int i = 0;
        for (float x = 0; x < nx; x++) {
            for (float y = 0; y < ny; y++) {
                gridCells[i] = x;
                i++;

                gridCells[i] = y;
                i++;
            }
        }

        float[] latLon = new float[gridCells.length];
        try {
            MathTransform mt = MapUtil
                    .getTransformToLatLon(PixelOrientation.CENTER, this);
            mt.transform(gridCells, 0, latLon, 0, gridCells.length / 2);
        } catch (Exception e) {
            statusHandler.error("Error computing lat/lon grid", e);
        }

        /*
         * return the 1-d array and let python reshape it
         */
        return new NDArray<>(latLon, 1, latLon.length);
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
