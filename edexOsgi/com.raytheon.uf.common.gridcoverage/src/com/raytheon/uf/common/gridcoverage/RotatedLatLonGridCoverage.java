package com.raytheon.uf.common.gridcoverage;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;


/**
 * Defines a Rotated Latitude/Longitude grid coverage.
 *
 * This class is generally used to describe grids defined by GRIB2
 * Grid Definition Template 3.1.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/11/26                  tiffanym    Initial creation for rotated lat/lon grids (RRFS)       
 *
 * </pre>
 *
 * @author tiffanym
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RotatedLatLonGridCoverage extends GridCoverage {

    private static final long serialVersionUID = 1L;

    /** The name of the projection. */
    public static final String PROJECTION_TYPE = "RotatedLatLon";

    /**
     * Latitude of the southern pole of projection as encoded in GRIB2
     * Template 3.1.
     */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double southPoleLat;

    /**
     * Longitude of the southern pole of projection as encoded in GRIB2
     * Template 3.1.
     */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double southPoleLon;

    /**
     * Angle of rotation about the new polar axis as encoded in GRIB2
     * Template 3.1.
     */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double rotationAngle;

    /**
     * Creates an empty RotatedLatLonGridCoverage.
     */
    public RotatedLatLonGridCoverage() {
    }

    /**
     * Copy constructor.
     *
     * @param coverage
     *            coverage to copy
     */
    public RotatedLatLonGridCoverage(RotatedLatLonGridCoverage coverage) {
        super(coverage);

        this.southPoleLat = coverage.southPoleLat;
        this.southPoleLon = coverage.southPoleLon;
        this.rotationAngle = coverage.rotationAngle;
    }

    @Override
    public void initialize() throws GridCoverageException {

        if (Double.compare(rotationAngle, 0.0) != 0) {
            throw new GridCoverageException(
                    "Rotated latitude/longitude grids with a non-zero "
                            + "rotation angle are not currently supported. "
                            + "Rotation angle: " + rotationAngle);
        }

        crs = MapUtil.constructRotatedPole(
                MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS,
                southPoleLat,
                southPoleLon);

        if (crs == null) {
            throw new GridCoverageException(
                    "Unable to construct rotated latitude/longitude CRS "
                            + "for south pole latitude [" + southPoleLat
                            + "] and longitude [" + southPoleLon + "]");
        }

        crsWKT = crs.toWKT();

        /*
         * la1/lo1 and dx/dy are in the rotated coordinate system.
         * Convert the cell-edge rectangle from rotated coordinates into
         * geographic lon/lat before storing the GridCoverage geometry.
         */
        double minX = MapUtil.correctLon(lo1) - (dx / 2.0);
        double minY = la1 - (dy / 2.0);

        double maxX = minX + (dx * nx);
        double maxY = minY + (dy * ny);
        
        double centralMeridian = MapUtil.correctLon(southPoleLon);

        try {
            geometry = MapUtil.createGeometryFromNative(
                    crs, minX, minY, maxX, maxY,centralMeridian);
        } catch (Exception e) {
            throw new GridCoverageException(
                    "Unable to create geometry for rotated latitude/longitude grid",
                    e);
        }
    }

    @Override
    public String getProjectionType() {
        return PROJECTION_TYPE;
    }

    @Override
    protected GridCoverage cloneImplCrsParameters(SubGrid subGrid) {
        RotatedLatLonGridCoverage rval = new RotatedLatLonGridCoverage();

        rval.southPoleLat = this.southPoleLat;
        rval.southPoleLon = this.southPoleLon;
        rval.rotationAngle = this.rotationAngle;

        return rval;
    }

    /**
     * Get the latitude of the southern pole of projection.
     *
     * @return southern pole latitude
     */
    public double getSouthPoleLat() {
        return southPoleLat;
    }

    /**
     * Set the latitude of the southern pole of projection.
     *
     * @param southPoleLat
     *            southern pole latitude
     */
    public void setSouthPoleLat(double southPoleLat) {
        this.southPoleLat = southPoleLat;
    }

    /**
     * Get the longitude of the southern pole of projection.
     *
     * @return southern pole longitude
     */
    public double getSouthPoleLon() {
        return southPoleLon;
    }

    /**
     * Set the longitude of the southern pole of projection.
     *
     * @param southPoleLon
     *            southern pole longitude
     */
    public void setSouthPoleLon(double southPoleLon) {
        this.southPoleLon = southPoleLon;
    }

    /**
     * Get the angle of rotation about the new polar axis.
     *
     * @return rotation angle
     */
    public double getRotationAngle() {
        return rotationAngle;
    }

    /**
     * Set the angle of rotation about the new polar axis.
     *
     * @param rotationAngle
     *            rotation angle
     */
    public void setRotationAngle(double rotationAngle) {
        this.rotationAngle = rotationAngle;
    }

    @Override
    public RotatedLatLonGridCoverage clone()
            throws CloneNotSupportedException {
        return (RotatedLatLonGridCoverage) super.clone();
    }
}