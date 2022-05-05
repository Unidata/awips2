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

import java.util.Arrays;

import javax.persistence.Entity;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Defines a Lat/Lon grid coverage. This class is generally used to describe
 * grids described by GDS Template 0 as specified in Table 3.1
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
 * Mar 04, 2015 3959        rjpeter     Update for grid based subgridding.
 * Sep 17, 2015 4696        nabowle     Add clone().
 * Aug 28, 2017 6378        bsteffen    Remove deprecated use of getLowerLeftLon
 *
 * </pre>
 *
 * @author bphillip
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class LatLonGridCoverage extends GridCoverage {
    private static final long serialVersionUID = 8371251040172233074L;

    /** The name of the projection */
    public static final String PROJECTION_TYPE = "LatLon";

    /** Latitude of the last grid point */
    @Transient
    @XmlElement
    private double la2;

    /** Longitude of the last grid point */
    @Transient
    @XmlElement
    private double lo2;

    /** True if the grid is thinned; false otherwise. */
    @Transient
    @XmlElement(required = false)
    private boolean isThin;

    /** The number of points per parallel. */
    @Transient
    @XmlElement(required = false)
    private int[] parallels;

    @Override
    public void initialize() throws GridCoverageException {
        // lower left is cell center, we want cell corners.
        double minLon = getLowerLeft().x - (dx / 2);
        double maxLon = minLon + (dx * nx);

        double centralMeridian = (minLon + maxLon) / 2.0;
        if ((dx * nx) <= 360) {
            centralMeridian = MapUtil.correctLon(centralMeridian);
        } else {
            // For almost all map projections geotools will clip all math
            // transforms to be within +-180 of the central meridian. For grids
            // that wrap around the world more than once this is a problem. When
            // the central Meridian is 0.0 then geotools does not do this
            // clipping, which works much better.
            centralMeridian = 0.0;
        }
        crs = MapUtil.constructEquidistantCylindrical(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                centralMeridian, 0);

        crsWKT = crs.toWKT();
        generateGeometry();
    }

    @Override
    public String getProjectionType() {
        return PROJECTION_TYPE;
    }

    @Override
    public Integer getNx() {
        return nx;
    }

    @Override
    public Integer getNy() {
        return ny;
    }

    @Override
    protected GridCoverage cloneImplCrsParameters(SubGrid subGrid) {
        LatLonGridCoverage rval = new LatLonGridCoverage();
        return rval;
    }

    /**
     * Gets la2
     *
     * @return The la2 latitude value
     */
    public double getLa2() {
        return la2;
    }

    /**
     * Sets la2
     *
     * @param la2
     *            The la2 latitude value
     */
    public void setLa2(double la2) {
        this.la2 = la2;
    }

    /**
     * Gets lo2
     *
     * @return The lo2 latitude value
     */
    public double getLo2() {
        return lo2;
    }

    /**
     * Sets lo2
     *
     * @param lo2
     *            The lo2 longitude value
     */
    public void setLo2(double lo2) {
        this.lo2 = lo2;
    }

    public boolean isThin() {
        return isThin;
    }

    public void setThin(boolean isThin) {
        this.isThin = isThin;
    }

    public int[] getParallels() {
        return parallels;
    }

    public void setParallels(int[] parallels) {
        this.parallels = parallels;
    }

    public LatLonGridCoverage() {

    }

    public LatLonGridCoverage(LatLonGridCoverage coverage) {
        super(coverage);
        this.la2 = coverage.la2;
        this.lo2 = coverage.lo2;
        this.isThin = coverage.isThin;
        this.parallels = coverage.parallels;
    }

    @Override
    public LatLonGridCoverage clone() throws CloneNotSupportedException {
        LatLonGridCoverage clone = (LatLonGridCoverage) super.clone();

        if (this.parallels != null) {
            clone.setParallels(
                    Arrays.copyOf(this.parallels, this.parallels.length));
        }

        return clone;
    }
}
