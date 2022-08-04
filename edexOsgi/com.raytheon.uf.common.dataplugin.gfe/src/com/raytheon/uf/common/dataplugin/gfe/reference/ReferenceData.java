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
package com.raytheon.uf.common.dataplugin.gfe.reference;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.geometry.jts.JTS;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.adapter.GeometryAdapter;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.Polygonal;

/**
 * A ReferenceData (a.k.a. Reference Set) contains a description of an area on
 * the earth's surface (series of polygons in lat/lon), and/or a query.
 *
 * implementation
 *
 * You can have a query and polygons defined in a single reference set. In this
 * case, the polygons represent an evaluation of the query for a particular time
 * and set of data.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 31, 2008           randerso  Initial creation
 * Oct 01, 2013  2361     njensen   Added static JAXBManager
 * Feb 19, 2015  4125     rjpeter   Updated to return a new pooled JAXBManager
 *                                  on request.
 * Jan 08, 2018  19900    ryu       Fix CAVE crash when starting GFE for non-activated site.
 * Feb 07, 2018  6882     randerso  Added isEmpty() method. Code Cleanup.
 *
 * </pre>
 *
 * @author randerso
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ReferenceData {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReferenceData.class);

    private static final GeometryFactory geometryFactory = new GeometryFactory();

    private static Reference<SingleTypeJAXBManager<ReferenceData>> jaxbRef = null;

    /**
     * enum defining the valid types for ReferenceData objects
     */
    public enum RefType {
        /** Empty, non-query */
        NONE,

        /** query based without polygons */
        QUERY,

        /** polygon based */
        POLYGON,

        /** query based with polygons */
        QUERY_POLYGON
    };

    /**
     * enum defining the valid coordinate types for polygons in ReferenceData
     */
    public enum CoordinateType {
        /** Latitude/Longitude (i.e. world) coordinates */
        LATLON,

        /** Grid indices */
        GRID
    };

    private ReferenceID _id;

    @XmlAttribute
    @DynamicSerializeElement
    private String query;

    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    @DynamicSerializeElement
    private MultiPolygon polygons;

    private GridLocation gloc;

    private Grid2DBit grid;

    private CoordinateType coordType;

    /**
     * @return the JAXBManager that handles ReferenceData.
     */
    public static synchronized SingleTypeJAXBManager<ReferenceData> getJAXBManager() {
        // not worried about concurrency, two can be created without issue
        SingleTypeJAXBManager<ReferenceData> rval = null;

        if (jaxbRef != null) {
            rval = jaxbRef.get();
        }

        if (rval == null) {
            rval = SingleTypeJAXBManager.createWithoutException(true,
                    ReferenceData.class);
            if (rval != null) {
                jaxbRef = new SoftReference<>(rval);
            }
        }

        return rval;
    }

    /**
     * Default constructor
     */
    public ReferenceData() {
        this.coordType = CoordinateType.LATLON;
    }

    /**
     * Constructor for ReferenceData with polygons.
     *
     * @param gloc
     * @param id
     * @param polygons
     * @param coordType
     */
    public ReferenceData(final GridLocation gloc, final ReferenceID id,
            final Polygonal polygons, CoordinateType coordType) {
        this(gloc, id, null, polygons, coordType);
    }

    /**
     * Constructor for ReferenceData taking a query string.
     *
     * @param gloc
     * @param id
     * @param query
     */
    public ReferenceData(final GridLocation gloc, final ReferenceID id,
            final String query) {
        this(gloc, id, null, null, CoordinateType.LATLON);
    }

    /**
     * Constructor for ReferenceData taking a query string and a set of
     * polygons.
     *
     * @param gloc
     * @param id
     * @param query
     * @param polygons
     * @param coordType
     */
    public ReferenceData(final GridLocation gloc, final ReferenceID id,
            final String query, final Polygonal polygons,
            CoordinateType coordType) {
        this._id = id;
        this.gloc = gloc;
        this.query = query;
        this.coordType = coordType;

        this.polygons = null;
        if (polygons != null) {
            if (polygons instanceof MultiPolygon) {
                this.polygons = (MultiPolygon) ((MultiPolygon) polygons)
                        .clone();
            } else if (polygons instanceof Polygon) {
                this.polygons = geometryFactory.createMultiPolygon(
                        new Polygon[] { (Polygon) polygons });
            }
        }
    }

    /**
     * Constructor for ReferenceData taking a grid.
     *
     * @param gloc
     * @param id
     * @param grid
     */
    public ReferenceData(final GridLocation gloc, final ReferenceID id,
            final Grid2DBit grid) {
        this._id = id;
        this.gloc = gloc;
        this.grid = grid == null ? null : new Grid2DBit(grid);
        this.coordType = CoordinateType.GRID;
    }

    /**
     * Constructor for ReferenceData taking a query and a grid.
     *
     * @param gloc
     * @param id
     * @param query
     * @param grid
     */
    public ReferenceData(final GridLocation gloc, final ReferenceID id,
            final String query, final Grid2DBit grid) {
        this._id = id;
        this.gloc = gloc;
        this.query = query;
        this.grid = grid == null ? null : new Grid2DBit(grid);
        this.coordType = CoordinateType.GRID;
    }

    /**
     * Copy Constructor
     *
     * @param rhs
     */
    public ReferenceData(final ReferenceData rhs) {
        this._id = rhs._id;
        this.query = rhs.query;
        this.polygons = (rhs.polygons == null ? null
                : (MultiPolygon) rhs.polygons.clone());
        this.gloc = rhs.gloc;
        this.grid = rhs.grid == null ? null : new Grid2DBit(rhs.grid);
        this.coordType = rhs.coordType;
    }

    /**
     * @return the ReferenceID for this ReferenceData
     */
    public ReferenceID getId() {
        return _id;
    }

    /**
     * Set the ReferenceID for this ReferenceData
     *
     * @param id
     */
    public void setId(final ReferenceID id) {
        this._id = id;
    }

    /**
     * @param coordType
     * @return the polygons associated with this ReferenceData in the desired
     *         coordinate type
     */
    public MultiPolygon getPolygons(CoordinateType coordType) {
        if ((grid != null) && (polygons == null)) {
            calcPolygons();
        }
        convertTo(coordType);
        return polygons;
    }

    /**
     * @return the query for this ReferenceData
     */
    public String getQuery() {
        return query;
    }

    /**
     * Set the query for this ReferenceData
     *
     * @param s
     *            the query to be set
     */
    public void setQuery(String s) {
        query = s;
    }

    /**
     * @param coordType
     * @return a list of domains (bounding boxes or envelopes) for each polygon
     *         in this ReferenceData in the desired coordinate type
     */
    public List<Envelope> getDomains(CoordinateType coordType) {
        List<Envelope> domains = new ArrayList<>();

        if (polygons == null) {
            // makes polygons, so we can getDomains()
            getPolygons(coordType);
        }

        for (int i = 0; i < polygons.getNumGeometries(); i++) {
            domains.add(polygons.getGeometryN(i).getEnvelopeInternal());
        }
        return domains;
    }

    /**
     * @param coordType
     * @return the overall domain (bounding box or envelope) of all polygons in
     *         the desired coordinate type
     */
    public Envelope overallDomain(CoordinateType coordType) {
        if (polygons == null) {
            getPolygons(coordType);
        }
        convertTo(coordType);

        Envelope domain = polygons.getEnvelopeInternal();
        return domain;
    }

    /**
     * Convert to desired coordinate type
     *
     * @param coordType
     */
    private void convertTo(CoordinateType coordType) {
        switch (coordType) {
        case GRID:
            convertToGrid();
            break;

        case LATLON:
            convertToLatLon();
            break;
        default:
            throw new IllegalArgumentException(
                    "Unrecognized coordinate type: " + coordType);
        }
    }

    /**
     * Returns a new ReferenceData object based on the current object, but in
     * grid coordinates.
     *
     * implementation
     *
     * For each region in this ReferenceData object, convert the points to grid
     * coords and make a new region. Return a ReferenceData object constructed
     * from these newly converted polygons and the current Reference ID.
     */
    private void convertToGrid() {
        if ((grid != null) && (polygons == null)) {
            coordType = CoordinateType.GRID;
            calcPolygons();
        }

        if (coordType == CoordinateType.LATLON) {
            try {

                polygons = (MultiPolygon) JTS.transform(polygons, MapUtil
                        .getTransformFromLatLon(PixelOrientation.CENTER, gloc));
                coordType = CoordinateType.GRID;

            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error converting to grid coordinates", e);
            }
        }
    }

    /**
     * Returns a new ReferenceData object based on the current object, but in
     * lat/lon coordinates.
     *
     * implementation
     *
     * For each region in this ReferenceData object, convert the points to
     * lat/lon coords and make a new region. Return a ReferenceData object
     * constructed from these newly converted polygons and the current Reference
     * ID.
     */
    private void convertToLatLon() {
        if ((grid != null) && (polygons == null)) {
            coordType = CoordinateType.GRID;
            calcPolygons();
        }
        if (coordType == CoordinateType.GRID) {
            try {
                polygons = (MultiPolygon) JTS.transform(polygons, MapUtil
                        .getTransformToLatLon(PixelInCell.CELL_CENTER, gloc));
                coordType = CoordinateType.LATLON;
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error converting to Lat/Lon coordinates", e);
            }
        }
    }

    /**
     * @return the type of this ReferenceData
     */
    public RefType refType() {
        if ((query != null) && (query.length() > 0) && (polygons != null)) {
            return RefType.QUERY_POLYGON;
        } else if ((query != null) && (query.length() > 0)) {
            return RefType.QUERY;
        } else if ((polygons != null) || (grid != null)) {
            return RefType.POLYGON;
        }

        return RefType.NONE;
    }

    /**
     * @return true if ReferenceData contains a query.
     */
    public boolean isQuery() {
        return (query != null) && (query.length() > 0);
    }

    /**
     *
     * @return true if ReferenceData is not a query and grid has not bits set
     */
    public boolean isEmpty() {
        return !this.isQuery() && !this.getGrid().isAnyBitsSet();
    }

    /**
     * @return the current coordinate type for this ReferenceData
     */
    public CoordinateType getCoordinateType() {
        return coordType;
    }

    /**
     * @return the GridLocation for this ReferenceData
     */
    public GridLocation getGloc() {
        return gloc;
    }

    /**
     * Set the GridLocation for this ReferenceData
     *
     * @param gloc
     */
    public void setGloc(final GridLocation gloc) {
        if (gloc != this.gloc) {
            if (coordType == CoordinateType.GRID) {
                // the coordinate system is changing, GRID is no longer valid
                convertToLatLon();
            }
            this.grid = null;
            this.gloc = gloc;
        }
    }

    /**
     * @return the mask grid for this ReferenceData
     */
    public Grid2DBit getGrid() {
        if (grid == null) {
            try {
                calcGrid();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "getGrid() failed for " + this.getId().getName(), e);

                // return an empty grid
       	        grid = new Grid2DBit(gloc.getNx(), gloc.getNy());
            }
        }
        return grid;
    }

    /**
     * Set this ReferenceData to the supplied grid
     *
     * @param grid
     */
    public void setGrid(final Grid2DBit grid) {
        this.grid = grid;
        this.polygons = null;
        this.coordType = CoordinateType.GRID;
    }

    /**
     * Set this ReferenceData to the supplied polygons
     *
     * @param polygons
     * @param coordType
     */
    public void setPolygons(MultiPolygon polygons, CoordinateType coordType) {
        this.grid = null;
        this.polygons = polygons;
        this.coordType = coordType;
    }

    /**
     * Invert the selected area of this ReferenceData
     */
    public void invert() {
        if (grid == null) {
            calcGrid();
        }
        grid.negate();
        polygons = null;
        query = null;
    }

    /**
     * Remove all polygons containing point from this ReferenceData
     *
     * @param point
     * @param coordType
     */
    public void removeIfContains(final Point point, CoordinateType coordType) {
        // force polygons to be recalculated if necessary
        getPolygons(coordType);

        List<Polygon> result = new ArrayList<>();
        for (int i = 0; i < polygons.getNumGeometries(); i++) {
            Polygon poly = (Polygon) polygons.getGeometryN(i);
            if (!poly.contains(point)) {
                result.add(poly);
            }
        }

        if (result.size() != polygons.getNumGeometries()) {
            polygons = geometryFactory.createMultiPolygon(
                    result.toArray(new Polygon[result.size()]));
            grid = null;
        }
    }

    private void calcGrid() {
        if (polygons != null) {
            grid = convertToGridpoints(this);
        } else if (gloc != null) {
            grid = new Grid2DBit(gloc.getNx(), gloc.getNy());
        } else {
            grid = new Grid2DBit(0, 0);
        }
    }

    private void calcPolygons() {
        if (grid != null) {
            coordType = CoordinateType.GRID;
            polygons = convertToPolygons(grid);
        }
    }

    /**
     * Returns a Grid2DBit that represents the grid points defined by the
     * specified ReferenceData object.
     *
     * implementation
     *
     * Loop through each region. Get make a Grid2DBit for each one and apply it
     * to the final grid based on the include flag. Return the result.
     *
     * @param refData
     * @return
     */
    private static Grid2DBit convertToGridpoints(final ReferenceData refData) {
        MultiPolygon polygon = refData.getPolygons(CoordinateType.GRID);
        GridLocation gloc = refData.getGloc();

        return GfeUtil.filledBitArray(polygon, gloc);
    }

    /**
     * Returns a sequence of polygons based on the Grid2DBit.
     *
     * @param grid
     * @return
     */
    private static MultiPolygon convertToPolygons(final Grid2DBit grid) {
        int rows = grid.getYdim();
        int cols = grid.getXdim();

        List<Polygon> polygons = new ArrayList<>();

        for (int row = 0; row < rows; row++) {
            int startcol = 0;
            int endcol = -1;
            while (endcol < cols) {
                startcol = endcol + 1;
                while ((startcol < cols) && !grid.getAsBoolean(startcol, row)) {
                    startcol++;
                }

                endcol = startcol + 1;
                if (startcol < cols) {
                    while ((endcol < cols) && grid.getAsBoolean(endcol, row)) {
                        endcol++;
                    }

                    Coordinate[] corners = new Coordinate[] {
                            new Coordinate(startcol - 0.5, row - 0.5),
                            new Coordinate(endcol - 0.5, row - 0.5),
                            new Coordinate(endcol - 0.5, row + 0.5),
                            new Coordinate(startcol - 0.5, row + 0.5), null };
                    corners[4] = corners[0];
                    LinearRing shell = geometryFactory
                            .createLinearRing(corners);
                    polygons.add(geometryFactory.createPolygon(shell, null));
                }
            }
        }
        Geometry g = geometryFactory
                .createMultiPolygon(
                        polygons.toArray(new Polygon[polygons.size()]))
                .buffer(0.0);

        MultiPolygon p = null;
        if (g instanceof MultiPolygon) {
            p = (MultiPolygon) g;
        } else if (g instanceof Polygon) {
            p = geometryFactory
                    .createMultiPolygon(new Polygon[] { (Polygon) g });
        }
        return p;
    }

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("Id: " + _id);

        RefType type = refType();
        s.append(" Type: " + type);

        if ((type == RefType.POLYGON) || (type == RefType.QUERY_POLYGON)) {
            s.append(" CoordType: " + coordType);
        }
        if ((type == RefType.QUERY) || (type == RefType.QUERY_POLYGON)) {
            s.append(" Query: " + query);
        }

        return s.toString();
    }

    /**
     * @param rhs
     * @return a new ReferenceData that is the union of this and rhs
     */
    public ReferenceData or(final ReferenceData rhs) {
        return new ReferenceData(this.gloc, new ReferenceID("temporary"),
                getGrid().or(rhs.getGrid()));
    }

    /**
     * Set this ReferenceData to the union of this and rhs
     *
     * @param rhs
     * @return this ReferenceData
     */
    public ReferenceData orEquals(final ReferenceData rhs) {
        getGrid();
        polygons = null;
        grid.orEquals(rhs.getGrid());
        return this;
    }

    /**
     * @param rhs
     * @return a new ReferenceData that is this minus rhs
     */
    public ReferenceData minus(final ReferenceData rhs) {
        return new ReferenceData(gloc, new ReferenceID("temporary"),
                getGrid().xor(getGrid().and(rhs.getGrid())));
    }

    /**
     * Subtract rhs from this ReferenceData and return this
     *
     * @param rhs
     * @return this ReferenceData
     */
    public ReferenceData minusEquals(final ReferenceData rhs) {
        getGrid();
        polygons = null;
        grid.xorEquals(grid.and(rhs.getGrid()));
        return this;
    }

    /**
     * @param rhs
     * @return a new ReferenceData that contains the intersection of this and
     *         rhs
     */
    public ReferenceData and(final ReferenceData rhs) {
        return new ReferenceData(gloc, new ReferenceID("temporary"),
                getGrid().and(rhs.getGrid()));
    }

    /**
     * Set this ReferenceData to the intersection of this and rhs
     *
     * @param rhs
     * @return this ReferenceData
     */
    public ReferenceData andEquals(final ReferenceData rhs) {
        getGrid();
        polygons = null;
        grid.andEquals(rhs.getGrid());
        return this;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result)
                + (coordType == null ? 0 : coordType.hashCode());
        result = (prime * result) + (gloc == null ? 0 : gloc.hashCode());
        result = (prime * result) + (grid == null ? 0 : grid.hashCode());
        result = (prime * result)
                + (polygons == null ? 0 : polygons.hashCode());
        result = (prime * result) + (query == null ? 0 : query.hashCode());
        return result;
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
        final ReferenceData other = (ReferenceData) obj;
        if (coordType == null) {
            if (other.coordType != null) {
                return false;
            }
        } else if (!coordType.equals(other.coordType)) {
            return false;
        }
        if (gloc == null) {
            if (other.gloc != null) {
                return false;
            }
        } else if (!gloc.equals(other.gloc)) {
            return false;
        }
        if (grid == null) {
            if (other.grid != null) {
                return false;
            }
        } else if (!grid.equals(other.grid)) {
            return false;
        }
        if (polygons == null) {
            if (other.polygons != null) {
                return false;
            }
        } else if (!polygons.equals(other.polygons)) {
            return false;
        }
        if (query == null) {
            if (other.query != null) {
                return false;
            }
        } else if (!query.equals(other.query)) {
            return false;
        }
        return true;
    }
}
