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

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.gridcoverage.subgrid.TrimUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Defines a Mercator grid coverage. This class is generally used to describe
 * grids described by GDS Template 10 as specified in Table 3.1
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 09/10/2012   DR 15270    D. Friedman Fix subgrid model name handling.
 * Jan 17, 2014 2125        rjpeter     Removed invalid @Table annotation.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MercatorGridCoverage extends GridCoverage {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MercatorGridCoverage.class);

    private static final long serialVersionUID = 3140441023975157052L;

    /** The name of the projection */
    public static final String PROJECTION_TYPE = "Mercator";

    /** The minor axis of the earth */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double minorAxis;

    /** The major axis of the earth */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double majorAxis;

    /**
     * latitude at which the Mercator projection intersects the Earth (Latitude
     * where Di and Dj are specified)
     */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double latin;

    /** Latitude of the last grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Double la2;

    /** Longitude of the last grid point */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private Double lo2;

    @Override
    public int generateHash() {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(super.generateHash());
        hashBuilder.append(latin);
        return hashBuilder.toHashCode();
    }

    @Override
    public void initialize() throws GridCoverageException {
        double meridian = 0;
        if ((la2 == null) || (lo2 == null)) {
            initializeSecondCorner();
        }

        switch (firstGridPointCorner) {
        case LowerLeft: // fall through
        case UpperLeft: {
            double maxLon = lo2;
            if (lo2 <= lo1) {
                maxLon += 360;
            }
            meridian = (lo1 + maxLon) / 2;
            break;
        }
        case LowerRight: // fall through
        case UpperRight: {
            double minLon = lo2;
            if (lo2 >= lo1) {
                minLon -= 360;
            }
            meridian = (lo1 - minLon) / 2;
            break;
        }
        }

        if (meridian > 180) {
            meridian -= 360;
        }
        if (meridian < -180) {
            meridian += 360;
        }
        crs = MapUtil.constructMercator(majorAxis, minorAxis, latin, meridian);
        crsWKT = crs.toWKT();
        generateGeometry();
    }

    private void initializeSecondCorner() throws GridCoverageException {

        // Since the CRS has not been produced yet, we create a dummy CRS for
        // calculation purposes
        CoordinateReferenceSystem dummyCrs = MapUtil.constructMercator(
                majorAxis, minorAxis, latin, 0);
        try {
            // Get the transforms to be used to convert between meters and
            // lat/lon
            MathTransform fromLatLon = MapUtil.getTransformFromLatLon(dummyCrs);
            MathTransform toLatLon = fromLatLon.inverse();

            // Use la1 and lo1 to specify the first point
            DirectPosition2D firstPosition = new DirectPosition2D();

            fromLatLon.transform(new DirectPosition2D(lo1, la1), firstPosition);

            // move firstPosition from cell center to cell corner
            firstPosition.x -= 0.5 * dx * 1000;
            firstPosition.y -= 0.5 * dy * 1000;

            // Determine the other corner point using the given dx,dy,nx, and
            // ny in meters
            DirectPosition2D position = null;
            switch (firstGridPointCorner) {
            case LowerLeft:
                position = new DirectPosition2D(firstPosition.x
                        + (dx * 1000 * nx), firstPosition.y + (dy * 1000 * ny));
                break;
            case UpperLeft:
                position = new DirectPosition2D(firstPosition.x
                        + (dx * 1000 * nx), firstPosition.y - (dy * 1000 * ny));
                break;
            case LowerRight:
                position = new DirectPosition2D(firstPosition.x
                        - (dx * 1000 * nx), firstPosition.y - (dy * 1000 * ny));
                break;
            case UpperRight:
                position = new DirectPosition2D(firstPosition.x
                        - (dx * 1000 * nx), firstPosition.y - (dy * 1000 * ny));
                break;
            default:
                throw new GridCoverageException(
                        "Inavalid grid point corner specified: "
                                + this.firstGridPointCorner);
            }
            // Convert the corner points from meters to lat/lon
            DirectPosition2D cornerPosition = new DirectPosition2D();
            toLatLon.transform(position, cornerPosition);
            lo2 = cornerPosition.x;
            la2 = cornerPosition.y;
        } catch (Exception e) {
            throw new GridCoverageException(
                    "Error calculating la2/lo2 for mercator projection!", e);
        }
    }

    @Override
    public GridCoverage trim(SubGrid subGrid) {
        MercatorGridCoverage rval = new MercatorGridCoverage();
        rval.description = this.description;
        rval.dx = this.dx;
        rval.dy = this.dy;
        rval.spacingUnit = this.spacingUnit;
        rval.latin = this.latin;
        rval.majorAxis = this.majorAxis;
        rval.minorAxis = this.minorAxis;

        try {
            Unit<?> spacingUnitObj = Unit.valueOf(spacingUnit);
            if (spacingUnitObj.isCompatible(SI.METRE)) {
                UnitConverter converter = spacingUnitObj
                        .getConverterTo(SI.METRE);
                double dxMeter = converter.convert(dx);
                double dyMeter = converter.convert(dy);
                MathTransform fromLatLon = MapUtil
                        .getTransformFromLatLon(getCrs());
                MathTransform toLatLon = fromLatLon.inverse();

                try {
                    TrimUtil.trimMeterSpace(getLowerLeftLat(),
                            getLowerLeftLon(), subGrid, this.nx, this.ny,
                            dxMeter, dyMeter, fromLatLon, toLatLon, true);
                } catch (GridCoverageException e) {
                    statusHandler.handle(Priority.WARN, "Grid coverage ["
                            + this.getName() + "] not applicable to this site");
                    return null;
                }

                rval.firstGridPointCorner = Corner.LowerLeft;
                rval.lo1 = subGrid.getLowerLeftLon();
                rval.la1 = subGrid.getLowerLeftLat();
                rval.lo2 = subGrid.getUpperRightLon();
                rval.la2 = subGrid.getUpperRightLat();
                rval.nx = subGrid.getNX();
                rval.ny = subGrid.getNY();
                rval.setName(SUBGRID_TOKEN + this.getId());
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Error creating sub grid definition [" + this.name
                                + "], units are not compatible with meter ["
                                + spacingUnit + "]");
                rval = null;
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating sub grid definition", e);
            rval = null;
        }

        return rval;
    }

    @Override
    public String getProjectionType() {
        return PROJECTION_TYPE;
    }

    /**
     * Gets latin
     * 
     * @return The latin latitude value
     */
    public double getLatin() {
        return latin;
    }

    /**
     * Sets latin
     * 
     * @param latin
     *            The latin latitude value
     */
    public void setLatin(double latin) {
        this.latin = latin;
    }

    public Double getLa2() {
        return la2;
    }

    public void setLa2(Double la2) {
        this.la2 = la2;
    }

    public Double getLo2() {
        return lo2;
    }

    public void setLo2(Double lo2) {
        this.lo2 = lo2;
    }

    /**
     * Gets the minor axis
     * 
     * @return The minor axis value
     */
    public double getMinorAxis() {
        return minorAxis;
    }

    /**
     * Sets the minor axis
     * 
     * @param minorAxis
     *            The minor axis value
     */
    public void setMinorAxis(double minorAxis) {
        this.minorAxis = minorAxis;
    }

    /**
     * Gets the major axis
     * 
     * @return The major axis value
     */
    public double getMajorAxis() {
        return majorAxis;
    }

    /**
     * Sets the major axis
     * 
     * @param majorAxis
     *            The major axis value
     */
    public void setMajorAxis(double majorAxis) {
        this.majorAxis = majorAxis;
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
        if (!super.equals(obj)) {
            return false;
        }
        MercatorGridCoverage other = (MercatorGridCoverage) obj;
        if (Double.doubleToLongBits(latin) != Double
                .doubleToLongBits(other.latin)) {
            return false;
        }
        if (Double.doubleToLongBits(majorAxis) != Double
                .doubleToLongBits(other.majorAxis)) {
            return false;
        }
        if (Double.doubleToLongBits(minorAxis) != Double
                .doubleToLongBits(other.minorAxis)) {
            return false;
        }
        return true;
    }

    @Override
    public boolean spatialEquals(GridCoverage other) {
        if (super.spatialEquals(other)) {
            MercatorGridCoverage otherMercator = (MercatorGridCoverage) other;
            if (Math.abs(latin - otherMercator.latin) > SPATIAL_TOLERANCE) {
                return false;
            }
            return true;
        }
        return false;
    }

    @Override
    public String spatialKey() {
        StringBuilder key = new StringBuilder(96);
        key.append(super.spatialKey());
        key.append(DataURI.SEPARATOR);
        key.append(latin);
        return key.toString();
    }

    public MercatorGridCoverage() {

    }

    public MercatorGridCoverage(MercatorGridCoverage coverage) {
        super(coverage);
        this.minorAxis = coverage.minorAxis;
        this.majorAxis = coverage.majorAxis;
        this.latin = coverage.latin;
        this.la2 = coverage.la2;
        this.lo2 = coverage.lo2;
    }
}
