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

package com.raytheon.uf.common.dataplugin.satellite;

import java.awt.geom.Rectangle2D;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Polygon;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.geospatial.adapter.GeometryAdapter;
import com.raytheon.uf.common.geospatial.util.EnvelopeIntersection;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Represents a map coverage area for satelllite images. It contains the
 * geometry information necessary for client applications to correctly
 * geo-locate and project satellite imagery.
 *
 * This class maps to the spatial_satellite table in the postGres database via
 * Hibernate.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 24, 2007  353      bphillip    Initial Checkin
 * Jul 12, 2012  798      jkorman     Changed projection "magic" numbers
 * Jul 16, 2013  2181     bsteffen    Convert geometry types to use hibernate-
 *                                    spatial
 * Sep 30, 2013  2333     mschenke    Refactored to store coordinates in CRS
 *                                    space
 * Apr 11, 2014  2947     bsteffen    Fix equals
 * Oct 16, 2014  3454     bphillip    Upgrading to Hibernate 4
 * Nov 05, 2014  3788     bsteffen    Make gid a sequence instead of a hash.
 * Mar 20, 2019  6140     tgurney     Hibernate 5 GeometryType fix
 * May  8, 2019  6140     tgurney     Lower the log level of some previously
 *                                    ignored exceptions (from error to debug)
 *
 * </pre>
 */
@Entity
@Table(name = "satellite_spatial", uniqueConstraints = {
        @UniqueConstraint(columnNames = { "minX", "minY", "dx", "dy", "nx",
                "ny", "crsWKT" }) })
@SequenceGenerator(name = "SATELLITE_SPATIAL_GENERATOR", sequenceName = "satspatial_seq", allocationSize = 1)
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SatMapCoverage extends PersistableDataObject<Object>
        implements IGridGeometryProvider {

    private static final long serialVersionUID = 1L;

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(SatMapCoverage.class);

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SATELLITE_SPATIAL_GENERATOR")
    @DynamicSerializeElement
    @DataURI(position = 0)
    private int gid;

    /**
     * The projection of the map coverage 1=Mercator, 3=Lambert Conformal
     * 5=Polar Stereographic
     *
     * @deprecated This field is only useful for GINI satellite format decoding
     *             and should not be in the coverage object
     */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    @Deprecated
    private Integer projection;

    /** Minimum x coordinate in crs space */
    @XmlAttribute
    @DynamicSerializeElement
    @Column
    private double minX;

    /** Minimum y coordinate in crs space */
    @XmlAttribute
    @DynamicSerializeElement
    @Column
    private double minY;

    /** Number of points along the x-axis */
    @XmlAttribute
    @DynamicSerializeElement
    @Column
    private Integer nx;

    /** Number of points along the y-axis */
    @XmlAttribute
    @DynamicSerializeElement
    @Column
    private Integer ny;

    /** The horizontal resolution of the grid */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private double dx;

    /** The vertical resolution of the grid */
    @Column
    @XmlAttribute
    @DynamicSerializeElement
    private double dy;

    @Column(length = 2047)
    @XmlAttribute
    @DynamicSerializeElement
    private String crsWKT;

    @Transient
    private CoordinateReferenceSystem crsObject;

    /** The map coverage */
    @Column(name = "the_geom", columnDefinition = "geometry")
    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    @DynamicSerializeElement
    private Geometry location;

    public SatMapCoverage() {
        super();
    }

    /**
     * Constructs a new SatMapCoverage Object
     *
     * @param projection
     *            the projection id value
     * @param minX
     *            minimum x value in crs space
     * @param minY
     *            minimum y value in crs space
     * @param nx
     *            number of x points in the satellite grid
     * @param ny
     *            number of y points in the satellite grid
     * @param dx
     *            spacing between grid cells in crs x space
     * @param dy
     *            spacing between grid cells in crs y space
     * @param crs
     *            the satellite data crs
     */
    public SatMapCoverage(int projection, double minX, double minY, int nx,
            int ny, double dx, double dy, CoordinateReferenceSystem crs) {
        this(projection, minX, minY, nx, ny, dx, dy, crs, null);
    }

    /**
     * Constructs a new SatMapCoverage Object
     *
     * @param projection
     *            the projection id value
     * @param minX
     *            minimum x value in crs space
     * @param minY
     *            minimum y value in crs space
     * @param nx
     *            number of x points in the satellite grid
     * @param ny
     *            number of y points in the satellite grid
     * @param dx
     *            spacing between grid cells in crs x space
     * @param dy
     *            spacing between grid cells in crs y space
     * @param crs
     *            the satellite data crs
     * @param latLonGeometry
     *            A Geometry representing the satellite bounds in lat/lon space
     */
    public SatMapCoverage(int projection, double minX, double minY, int nx,
            int ny, double dx, double dy, CoordinateReferenceSystem crs,
            Geometry latLonGeometry) {
        this.projection = projection;
        this.minX = minX;
        this.minY = minY;
        this.nx = nx;
        this.ny = ny;
        this.dx = dx;
        this.dy = dy;
        this.crsObject = crs;
        if (latLonGeometry == null) {
            try {
                latLonGeometry = EnvelopeIntersection
                        .createEnvelopeIntersection(
                                getGridGeometry().getEnvelope(),
                                new Envelope2D(DefaultGeographicCRS.WGS84, -180,
                                        -90, 360, 180),
                                1.0, 10, 10)
                        .getEnvelope();
            } catch (Exception e) {
                logger.debug(
                        "Error in constructing a new SatMapCoverage Object", e);
            }
        }
        this.location = latLonGeometry;
    }

    /**
     * @deprecated This field is only useful for GINI satellite format decoding
     *             and should not be in the coverage object
     * @return
     */
    @Deprecated
    public Integer getProjection() {
        return projection;
    }

    /**
     * @deprecated This field is only useful for GINI satellite format decoding
     *             and should not be in the coverage object
     * @param projection
     */
    @Deprecated
    public void setProjection(Integer projection) {
        this.projection = projection;
    }

    public int getGid() {
        return gid;
    }

    public void setGid(int gid) {
        this.gid = gid;
    }

    public double getMinX() {
        return minX;
    }

    public void setMinX(double minX) {
        this.minX = minX;
    }

    public double getMinY() {
        return minY;
    }

    public void setMinY(double minY) {
        this.minY = minY;
    }

    public Integer getNx() {
        return nx;
    }

    public void setNx(Integer nx) {
        this.nx = nx;
    }

    public Integer getNy() {
        return ny;
    }

    public void setNy(Integer ny) {
        this.ny = ny;
    }

    public double getDx() {
        return dx;
    }

    public void setDx(double dx) {
        this.dx = dx;
    }

    public double getDy() {
        return dy;
    }

    public void setDy(double dy) {
        this.dy = dy;
    }

    public String getCrsWKT() {
        if (crsWKT == null && crsObject != null) {
            crsWKT = crsObject.toWKT();
        }
        return crsWKT;
    }

    public void setCrsWKT(String crsWKT) {
        this.crsWKT = crsWKT;
        if (crsObject != null) {
            crsObject = null;
        }
    }

    public Geometry getLocation() {
        if (location == null) {
            location = generateLocation();
            if (location == null) {
                /*
                 * Default to empty MultiPolygon so various Geometry methods
                 * still work
                 */
                location = new GeometryFactory()
                        .createMultiPolygon(new Polygon[0]);
            }
        }
        return location;
    }

    public void setLocation(Geometry location) {
        this.location = location;
    }

    /**
     * Generates the lat/lon bounding geometry for the spatial record
     *
     * @return lat/lon bounding geometry or null if none could be generated
     */
    private Geometry generateLocation() {
        try {
            return EnvelopeIntersection
                    .createEnvelopeIntersection(getGridGeometry().getEnvelope(),
                            new Envelope2D(DefaultGeographicCRS.WGS84, -180,
                                    -90, 360, 180),
                            1.0, 10, 10)
                    .getEnvelope();
        } catch (Exception e) {
            /*
             * Don't care, probably happened during deserialization because some
             * fields have not been populated yet
             */
            logger.debug(
                    "Error in generating the lat/lon bounding geometry for the spatial record",
                    e);
        }
        return null;
    }

    public CoordinateReferenceSystem getCrs() {
        if (crsObject == null && crsWKT != null) {
            try {
                crsObject = CRSCache.getInstance()
                        .getCoordinateReferenceSystem(crsWKT);
            } catch (FactoryException e) {
                logger.debug("Error in getting the Coordinate Reference System",
                        e);
                crsObject = null;
            }
        }
        return crsObject;
    }

    @Override
    public GridGeometry2D getGridGeometry() {
        GridEnvelope gridRange = new GridEnvelope2D(0, 0, getNx(), getNy());
        Envelope crsRange = new Envelope2D(getCrs(), new Rectangle2D.Double(
                minX, minY, getNx() * getDx(), getNy() * getDy()));
        return new GridGeometry2D(gridRange, crsRange);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(projection);
        builder.append(getCrsWKT());
        builder.append(minX);
        builder.append(minY);
        builder.append(dx);
        builder.append(dy);
        builder.append(nx);
        builder.append(ny);
        return builder.toHashCode();
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
        SatMapCoverage other = (SatMapCoverage) obj;
        if (projection != other.projection) {
            return false;
        }
        String crsWKT = getCrsWKT();
        String otherCrsWKT = other.getCrsWKT();
        if (crsWKT == null) {
            if (otherCrsWKT != null) {
                return false;
            }
        } else if (!crsWKT.equals(otherCrsWKT)) {
            return false;
        }
        if (Double.doubleToLongBits(dx) != Double.doubleToLongBits(other.dx)) {
            return false;
        }
        if (Double.doubleToLongBits(dy) != Double.doubleToLongBits(other.dy)) {
            return false;
        }
        if (Double.doubleToLongBits(minX) != Double
                .doubleToLongBits(other.minX)) {
            return false;
        }
        if (Double.doubleToLongBits(minY) != Double
                .doubleToLongBits(other.minY)) {
            return false;
        }
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
        return true;
    }

}
