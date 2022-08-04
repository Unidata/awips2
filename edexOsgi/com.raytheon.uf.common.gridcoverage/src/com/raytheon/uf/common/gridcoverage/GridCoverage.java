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

package com.raytheon.uf.common.gridcoverage;

import java.text.ParsePosition;

import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Length;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.geotools.coverage.grid.GridGeometry2D;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.adapter.GeometryAdapter;
import com.raytheon.uf.common.geospatial.util.GridGeometryWrapChecker;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import si.uom.SI;
import tec.uom.se.format.SimpleUnitFormat;
import tec.uom.se.unit.MetricPrefix;

/**
 * Base class for encapsulating grid spatial information
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 07, 2009  1994     bphillip    Initial Creation
 * Sep 07, 2012  1102     djohnson    Add missing JAXB annotations.
 * Sep 10, 2012  15270    D. Friedman Fix subgrid model name handling.
 * Nov 02, 2012  1302     djohnson    Remove commented out code.
 * Jul 16, 2013  2181     bsteffen    Convert geometry types to use hibernate-
 *                                    spatial
 * Oct 15, 2013  2473     bsteffen    add @XmlSeeAlso for self contained JAXB
 *                                    context.
 * Apr 11, 2014  2947     bsteffen    Implement IGridGeometryProvider.
 * Oct 16, 2014  3454     bphillip    Upgrading to Hibernate 4
 * Mar 04, 2015  3959     rjpeter     Update for grid based subgridding.
 * Sep 16, 2015  4696     nabowle     Implement cloneable and add clone().
 * Oct 01, 2015  4868     rjpeter     Reject subGrids that don't meet minimum
 *                                    coverage percent.
 * Feb 26, 2016  5414     rjpeter      Fix subgrids along boundary.
 * Jun 24, 2016  ASM18440 dfriedman   Fix spatial tolerance for degree values.
 * Aug 28, 2017  6378     bsteffen    Remove cached lower left ordinates.
 * Mar 20, 2019  6140     tgurney     Hibernate 5 UserType fix
 *
 * </pre>
 *
 * @author bphillip
 */
@Entity
@Table
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@SequenceGenerator(name = "GRIDCOVERAGE_GENERATOR", sequenceName = "gridcoverage_seq", allocationSize = 1)
@XmlAccessorType(XmlAccessType.NONE)
@XmlSeeAlso({ LambertConformalGridCoverage.class, LatLonGridCoverage.class,
        MercatorGridCoverage.class, PolarStereoGridCoverage.class,
        StereographicGridCoverage.class })
@DynamicSerialize
public abstract class GridCoverage extends PersistableDataObject<Integer>
        implements ISpatialObject, IGridGeometryProvider, Cloneable {

    private static final long serialVersionUID = -1355232934065074837L;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridCoverage.class);

    protected static final String SUBGRID_TOKEN = "SubGrid-";

    public static final double SPATIAL_TOLERANCE_KM = 0.1;

    public static final double SPATIAL_TOLERANCE_DEG = 0.0025;

    /**
     * The id for this grid. This value is generated in the initialize method
     **/
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRIDCOVERAGE_GENERATOR")
    @DynamicSerializeElement
    protected Integer id;

    /** The name of the grid */
    @Column
    @XmlElement
    @DynamicSerializeElement
    @DataURI(position = 0)
    protected String name;

    /** A description of the grid coverage */
    @Transient
    @XmlElement
    @DynamicSerializeElement
    protected String description;

    /** Geometry object holding the corner points of the grid */
    @Column(name = "the_geom", columnDefinition = "geometry")
    @XmlJavaTypeAdapter(value = GeometryAdapter.class)
    @DynamicSerializeElement
    protected Geometry geometry;

    /** The CRS as a WKT String */
    @Column(name = "crs", length = 2047)
    @XmlElement
    @DynamicSerializeElement
    protected String crsWKT;

    /** Latitude of first grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    protected double la1;

    /** Longitude of the first grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    protected double lo1;

    /** Corner of the first grid point */
    @XmlElement
    @Column
    @DynamicSerializeElement
    @Enumerated(EnumType.STRING)
    protected Corner firstGridPointCorner;

    /** The CRS object */
    @Transient
    protected CoordinateReferenceSystem crs;

    /** The Grid geometry */
    @Transient
    protected transient GridGeometry2D gridGeometry;

    /** Number of points along I direction */
    @Column
    @XmlElement
    @DynamicSerializeElement
    protected Integer nx;

    /** Number of points along J direction */
    @Column
    @XmlElement
    @DynamicSerializeElement
    protected Integer ny;

    /** I direction increment */
    @Column
    @XmlElement
    @DynamicSerializeElement
    protected double dx;

    /** J direction increment */
    @Column
    @XmlElement
    @DynamicSerializeElement
    protected double dy;

    /** Spacing unit of dx and dy */
    @Column
    @XmlElement
    @DynamicSerializeElement
    protected String spacingUnit;

    /** The scan mode. */
    @Transient
    @XmlElement(required = false)
    protected Integer scanMode;

    /** The resolution. */
    @Transient
    @XmlElement(required = false)
    protected Integer resolution;

    /** If a data point for the pole is included, either row or column. */
    @Transient
    @XmlElement(required = false)
    protected String includePole;

    /**
     * Creates an empty GridCoverage object
     */
    protected GridCoverage() {

    }

    @Override
    public String toString() {
        if (id == null) {
            return "Coverage Information Not Specified yet";
        }
        return Integer.toString(id);
    }

    @Override
    public int hashCode() {
        return generateHash();
    }

    /**
     * Generates a hash code based on selected fields in the grid coverage
     * object. The fields used will vary among different projections.
     *
     * @return The hash code generated from selected fields in the object
     */
    public int generateHash() {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(name);
        hashBuilder.append(nx);
        hashBuilder.append(ny);
        hashBuilder.append(dx);
        hashBuilder.append(dy);
        hashBuilder.append(la1);
        hashBuilder.append(lo1);
        // enum hashCode is unstable, use StringRep
        hashBuilder.append(firstGridPointCorner.name());
        return hashBuilder.toHashCode();
    }

    /**
     * Initializes the grid coverage object. Initialization should entail
     * creation of the crs and geometry object as well as assigning the id field
     *
     * @throws GridCoverageException
     *             If problems occur while creating the crs, geometry, or the id
     */
    public abstract void initialize() throws GridCoverageException;

    /**
     * Gets the name of the projection. The projection type is specified by each
     * subclass and accessed through this method.
     *
     * @return The name/type of the projection
     */
    public abstract String getProjectionType();

    /**
     * Trim this GridCoverage to a sub grid. Intersection returned instead of
     * shifting grid. This allows grids to be discarded that do not intersect
     * CWA, such as CONUS grids for Alaska sites.
     *
     * @param subGrid
     * @return trimmed coverage
     */
    public GridCoverage trim(SubGrid subGrid) {
        /*
         * validate the subgrid bounds, adjusting as necessary to fit in bounds.
         * Also validate world wrap settings.
         */
        int sgUlx = subGrid.getUpperLeftX();
        int sgUly = subGrid.getUpperLeftY();
        int sgNx = subGrid.getNX();
        int sgNy = subGrid.getNY();

        /* validate sgUlx and sgNx */
        int worldWrapCount = getWorldWrapCount();

        if (worldWrapCount != GridGeometryWrapChecker.NO_WRAP) {
            /* Check western boundary */
            if (sgUlx < 0) {
                /*
                 * All subGrid code wraps on the western boundary, offset sgUlx
                 * into valid range
                 */
                sgUlx += worldWrapCount;
            }

            /*
             * subgrid allowed to extend beyond boundary, ensure subgrid is no
             * bigger than the wrap count, this allows moving of the seam if
             * desired
             */
            if (sgNx > worldWrapCount) {
                sgNx = worldWrapCount;
            }
        } else {
            /* Check western boundary */
            if (sgUlx < 0) {
                sgNx += sgUlx;
                sgUlx = 0;
            }

            if (sgNx > 0 && sgUlx + sgNx > nx) {
                /*
                 * subgrid extending beyond eastern boundary of grid
                 */
                sgNx = nx - sgUlx;
            }
        }

        /* Check northern boundary */
        if (sgUly < 0) {
            sgNy += sgUly;
            sgUly = 0;
        }

        /* validate sgUly and sgNy */
        if (sgNy > 0 && sgUly + sgNy > ny) {
            sgNy = ny - sgUly;
        }

        if (sgNx < 0) {
            sgNx = 0;
        }

        if (sgNy < 0) {
            sgNy = 0;
        }

        subGrid.setUpperLeftX(sgUlx);
        subGrid.setUpperLeftY(sgUly);
        subGrid.setNX(sgNx);
        subGrid.setNY(sgNy);

        GridCoverage rval = cloneCrsParameters(subGrid);
        return rval;
    }

    /**
     * Convenience method for returning if this grid world wraps. Note: Coverage
     * must be initialized before calling this.
     *
     * @return
     */
    public int getWorldWrapCount() {
        if (geometry == null) {
            try {
                this.initialize();
            } catch (GridCoverageException e) {
                throw new IllegalStateException(
                        "Cannot look up world wrap count.  GridCoverage not initialized.",
                        e);
            }

        }
        return GridGeometryWrapChecker.checkForWrapping(getGridGeometry());
    }

    /**
     * Create a clone of this coverage setting any crs parameters based on the
     * defined subgrid.
     *
     * @param subGrid
     * @return
     */
    private GridCoverage cloneCrsParameters(SubGrid subGrid) {
        GridCoverage rval = cloneImplCrsParameters(subGrid);
        /* Set the base GridCoverage values */
        rval.setName(SUBGRID_TOKEN + this.getId());
        rval.description = "SubGrid of " + this.description;
        rval.dx = this.dx;
        rval.dy = this.dy;
        rval.spacingUnit = this.spacingUnit;

        /* grid space is 0,0 for upper left */
        Coordinate lowerLeft = new Coordinate(subGrid.getUpperLeftX(),
                subGrid.getUpperLeftY() + subGrid.getNY() - 1);
        lowerLeft = MapUtil.gridCoordinateToLatLon(lowerLeft,
                PixelOrientation.CENTER, this);

        rval.firstGridPointCorner = Corner.LowerLeft;
        rval.lo1 = lowerLeft.x;
        rval.la1 = lowerLeft.y;
        rval.nx = subGrid.getNX();
        rval.ny = subGrid.getNY();
        return rval;
    }

    /**
     * Create a clone of this coverage setting any implementation specific crs
     * parameters.
     *
     * @param subGrid
     * @return
     */
    protected abstract GridCoverage cloneImplCrsParameters(SubGrid subGrid);

    @Override
    public Geometry getGeometry() {
        return geometry;
    }

    @Override
    public CoordinateReferenceSystem getCrs() {
        if (crs == null) {
            try {
                this.crs = CRSCache.getInstance()
                        .getCoordinateReferenceSystem(crsWKT);
            } catch (FactoryException e) {
                statusHandler.debug("Unable to initialize crs", e);
                this.crs = null;
            }
        }
        return crs;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setGeometry(Geometry geometry) {
        this.geometry = geometry;
    }

    public void setCrs(CoordinateReferenceSystem crs) {
        this.crs = crs;
    }

    public String getCrsWKT() {
        return crsWKT;
    }

    public void setCrsWKT(String crsWKT) {
        if (crsWKT == null || !crsWKT.equals(this.crsWKT)) {
            this.crs = null;
        }
        this.crsWKT = crsWKT;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @Override
    public GridGeometry2D getGridGeometry() {
        if (gridGeometry == null) {
            gridGeometry = MapUtil.getGridGeometry(this);
        }

        return gridGeometry;
    }

    public void setGridGeometry(GridGeometry2D gridGeometry) {
        this.gridGeometry = gridGeometry;
    }

    public double getLa1() {
        return la1;
    }

    public void setLa1(double la1) {
        this.la1 = la1;
    }

    public double getLo1() {
        return lo1;
    }

    public void setLo1(double lo1) {
        this.lo1 = lo1;
    }

    /**
     * @deprecated Use getLowerLeft().y instead, usually lat and lon are needed
     *             together and getLowerLeft() can be called once for both
     *             values to improve performance.
     */
    @Deprecated
    public Double getLowerLeftLat() throws GridCoverageException {
        return generateLowerLeft().y;
    }

    /**
     * @deprecated Use getLowerLeft().x instead, usually lat and lon are needed
     *             together and getLowerLeft() can be called once for both
     *             values to improve performance.
     */
    @Deprecated
    public Double getLowerLeftLon() throws GridCoverageException {
        return generateLowerLeft().x;
    }

    public Coordinate getLowerLeft() throws GridCoverageException {
        return generateLowerLeft();
    }

    public Corner getFirstGridPointCorner() {
        return firstGridPointCorner;
    }

    public void setFirstGridPointCorner(Corner firstGridPointCorner) {
        this.firstGridPointCorner = firstGridPointCorner;
    }

    @Override
    public Integer getNx() {
        return nx;
    }

    public void setNx(Integer nx) {
        this.nx = nx;
    }

    @Override
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

    public String getSpacingUnit() {
        return spacingUnit;
    }

    public void setSpacingUnit(String spacingUnit) {
        this.spacingUnit = spacingUnit;
    }

    public Integer getScanMode() {
        return scanMode;
    }

    public void setScanMode(Integer scanMode) {
        this.scanMode = scanMode;
    }

    public Integer getResolution() {
        return resolution;
    }

    public void setResolution(Integer resolution) {
        this.resolution = resolution;
    }

    public String getIncludePole() {
        return includePole;
    }

    public void setIncludePole(String includePole) {
        this.includePole = includePole;
    }

    protected Coordinate generateLowerLeft() throws GridCoverageException {
        Coordinate lowerLeft = new Coordinate();
        try {
            if ("degree".equals(spacingUnit)) {
                switch (firstGridPointCorner) {
                case LowerLeft: {
                    lowerLeft.y = la1;
                    lowerLeft.x = lo1;
                    break;
                }
                case UpperLeft: {
                    lowerLeft.y = la1 - dy * (ny - 1);
                    lowerLeft.x = lo1;
                    break;
                }
                case UpperRight: {
                    lowerLeft.y = la1 - dy * (ny - 1);
                    lowerLeft.x = lo1 - dx * (nx - 1);
                    break;
                }
                case LowerRight: {
                    lowerLeft.y = la1;
                    lowerLeft.x = lo1 - dx * (nx - 1);
                    break;
                }
                }
            } else if (Corner.LowerLeft.equals(firstGridPointCorner)) {
                lowerLeft.y = la1;
                lowerLeft.x = lo1;
            } else {
                if (getCrs() == null) {
                    throw new GridCoverageException("CRS is null.");
                }

                Unit<Length> spacingUnitObj = SimpleUnitFormat
                        .getInstance(SimpleUnitFormat.Flavor.Default)
                        .parseProductUnit(spacingUnit, new ParsePosition(0))
                        .asType(Length.class);
                if (spacingUnitObj.isCompatible(SI.METRE)) {
                    UnitConverter converter = spacingUnitObj
                            .getConverterTo(SI.METRE);
                    double dxMeter = converter.convert(dx);
                    double dyMeter = converter.convert(dy);
                    MathTransform fromLatLon = MapUtil
                            .getTransformFromLatLon(getCrs());
                    MathTransform toLatLon = fromLatLon.inverse();
                    double[] lonLat = { lo1, la1 };
                    double[] lonLatInMeters = new double[2];
                    fromLatLon.transform(lonLat, 0, lonLatInMeters, 0, 1);

                    switch (firstGridPointCorner) {
                    case LowerLeft: {
                        // LL point is already correct
                        break;
                    }
                    case UpperLeft: {
                        // longitude point is already correct
                        lonLatInMeters[1] -= (ny - 1) * dyMeter;
                        break;
                    }
                    case UpperRight: {
                        lonLatInMeters[0] -= (nx - 1) * dxMeter;
                        lonLatInMeters[1] -= (ny - 1) * dyMeter;
                        break;
                    }
                    case LowerRight: {
                        // latitude point is already correct
                        lonLatInMeters[0] -= (nx - 1) * dxMeter;
                        break;
                    }
                    }
                    toLatLon.transform(lonLatInMeters, 0, lonLat, 0, 1);
                    lowerLeft.x = lonLat[0];
                    lowerLeft.y = lonLat[1];
                } else {
                    throw new GridCoverageException(
                            "Cannot convert " + spacingUnit + " to meters");
                }
            }
        } catch (Exception e) {
            throw new GridCoverageException(
                    "Cannot determine LowerLeft point of grid", e);
        }
        lowerLeft.x = MapUtil.correctLon(lowerLeft.x);
        return lowerLeft;
    }

    protected void generateGeometry() throws GridCoverageException {
        if ("degree".equals(spacingUnit)) {
            // lower left is cell center, we want cell corners.
            Coordinate lowerLeft = generateLowerLeft();
            double minLat = lowerLeft.y - dy / 2;
            double maxLat = minLat + dy * ny;
            double minLon = lowerLeft.x - dx / 2;
            if (dx * nx <= 360) {
                // Do not correct lon if larger than worldwide, most notably the
                // grid range for ECMWF-LowRes goes from -181.25 to 181.25 but
                // if you correct you end up at 178.75 to 538.75 which doesn't
                // work very well
                minLon = MapUtil.correctLon(minLon);
            }
            double maxLon = minLon + dx * nx;
            if (dx * nx == 360) {
                // Grids that wrap around the world need to be shrunk slightly
                // to account for inaccuracies when converting between degrees
                // and radians which can result in an invalid envelope.
                maxLon -= 1.0E-12;
                minLon += 1.0E-12;
            }

            // Normalize the range by shifting 360 degrees to bring the range
            // within +/-360 degree as much as possible. For example the
            // Canadian-NH model gets calculated as 179.7 to 540.3 but it
            // works better to use -180.3 to 180.3.
            while (minLon > 0 && maxLon > 360) {
                minLon -= 360;
                maxLon -= 360;
            }
            // Normalize the low end.
            while (minLon < -360 && maxLon < 0) {
                minLon += 360;
                maxLon += 360;
            }
            try {
                geometry = MapUtil.createGeometry(minLat, minLon, maxLat,
                        maxLon);
            } catch (Exception e) {
                throw new GridCoverageException("Error creating geometry", e);
            }
        } else {
            try {
                Unit<?> spacingUnitObj = SimpleUnitFormat
                        .getInstance(SimpleUnitFormat.Flavor.Default)
                        .parseProductUnit(spacingUnit, new ParsePosition(0));
                if (spacingUnitObj.isCompatible(SI.METRE)) {
                    Coordinate lowerLeft = generateLowerLeft();
                    UnitConverter converter = spacingUnitObj
                            .asType(Length.class).getConverterTo(SI.METRE);
                    geometry = MapUtil.createGeometry(crs, lowerLeft.y,
                            lowerLeft.x, converter.convert(dx),
                            converter.convert(dy), nx, ny);
                } else {
                    throw new GridCoverageException(
                            "Unable to convert " + spacingUnit
                                    + " to meters while creating geometry!");
                }
            } catch (Exception e) {
                throw new GridCoverageException("Error creating geometry", e);
            }
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (this.getClass() != obj.getClass()) {
            return false;
        }
        GridCoverage other = (GridCoverage) obj;
        if (Double.doubleToLongBits(dx) != Double.doubleToLongBits(other.dx)) {
            return false;
        }
        if (Double.doubleToLongBits(dy) != Double.doubleToLongBits(other.dy)) {
            return false;
        }
        if (Double.doubleToLongBits(la1) != Double
                .doubleToLongBits(other.la1)) {
            return false;
        }
        if (Double.doubleToLongBits(lo1) != Double
                .doubleToLongBits(other.lo1)) {
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
        if (spacingUnit == null) {
            if (other.spacingUnit != null) {
                return false;
            }
        } else if (!spacingUnit.equals(other.spacingUnit)) {
            return false;
        }
        if (firstGridPointCorner != other.firstGridPointCorner) {
            return false;
        }
        return true;
    }

    /**
     * Compare coverages to see if they are equivelant within a certain
     * tolerance
     *
     * @param other
     * @return true to indicate the coverages should be treated as equals, false
     *         if they are too different.
     */
    public boolean spatialEquals(GridCoverage other) {
        if (!this.getClass().equals(other.getClass())) {
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
        if (spacingUnit == null) {
            if (other.spacingUnit != null) {
                return false;
            }
        } else if (!spacingUnit.equals(other.spacingUnit)) {
            return false;
        }
        if (firstGridPointCorner != other.firstGridPointCorner) {
            return false;
        }
        double spacingUnitTolerance = getSpacingUnitTolerance();
        if (Math.abs(dx - other.dx) > spacingUnitTolerance) {
            return false;
        } else if (Math.abs(dy - other.dy) > spacingUnitTolerance) {
            return false;
        } else if (Math.abs(la1 - other.la1) > SPATIAL_TOLERANCE_DEG) {
            return false;
        } else if (Math.abs(MapUtil.correctLon(lo1)
                - MapUtil.correctLon(other.lo1)) > SPATIAL_TOLERANCE_DEG) {
            return false;
        }
        return true;
    }

    public double getSpacingUnitTolerance() {
        if ("degree".equals(spacingUnit)) {
            return SPATIAL_TOLERANCE_DEG;
        } else {
            Unit<Length> spacingUnitObj = SimpleUnitFormat
                    .getInstance(SimpleUnitFormat.Flavor.Default)
                    .parseProductUnit(spacingUnit, new ParsePosition(0))
                    .asType(Length.class);
            if (spacingUnitObj.isCompatible(MetricPrefix.KILO(SI.METRE))) {
                UnitConverter converter = MetricPrefix.KILO(SI.METRE)
                        .getConverterTo(spacingUnitObj);
                return converter.convert(SPATIAL_TOLERANCE_KM);
            } else {
                return SPATIAL_TOLERANCE_KM;
            }
        }
    }

    /**
     * Unique key containing the spatial attributes of this coverage.
     *
     * @return
     */
    public String spatialKey() {
        StringBuilder key = new StringBuilder(96);
        key.append(getProjectionType().replace(" ", "_"));
        key.append(DataURI.SEPARATOR);
        key.append(nx);
        key.append(DataURI.SEPARATOR);
        key.append(ny);
        key.append(DataURI.SEPARATOR);
        key.append(dx);
        key.append(DataURI.SEPARATOR);
        key.append(dy);
        key.append(DataURI.SEPARATOR);
        key.append(lo1);
        key.append(DataURI.SEPARATOR);
        key.append(la1);
        return key.toString();
    }

    public GridCoverage(GridCoverage coverage) {
        this.id = coverage.id;
        this.name = coverage.name;
        this.description = coverage.description;
        this.geometry = coverage.geometry;
        this.crsWKT = coverage.crsWKT;
        this.la1 = coverage.la1;
        this.lo1 = coverage.lo1;
        this.firstGridPointCorner = coverage.firstGridPointCorner;
        this.crs = coverage.crs;
        this.gridGeometry = coverage.gridGeometry;
        this.nx = coverage.nx;
        this.ny = coverage.ny;
        this.dx = coverage.dx;
        this.dy = coverage.dy;
        this.spacingUnit = coverage.spacingUnit;
        this.scanMode = coverage.scanMode;
        this.resolution = coverage.resolution;
        this.includePole = coverage.includePole;
    }

    /**
     * Clone this GridCoverage.
     */
    @Override
    public GridCoverage clone() throws CloneNotSupportedException {
        GridCoverage clone = (GridCoverage) super.clone();

        if (this.geometry != null) {
            clone.setGeometry((Geometry) this.geometry.clone());
        }

        return clone;
    }

}
