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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.GridGeometryWrapChecker;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Defines a Polar Stereographic grid coverage. This class is generally used to
 * describe grids described by GDS Template 20 as specified in Table 3.1
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
 * Jun 24, 2016  ASM18440 dfriedman   Fix spatial tolerance for degree values.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PolarStereoGridCoverage extends GridCoverage {
    private static final long serialVersionUID = 2640862310607194072L;

    /** The name of the projection */
    public static final String PROJECTION_TYPE = "Polar Stereographic";

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

    /** Orientation of the grid */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double lov;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private double lad = 60.0;

    @Override
    public int generateHash() {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(super.generateHash());
        hashBuilder.append(lov);
        hashBuilder.append(lad);
        return hashBuilder.toHashCode();
    }

    @Override
    public void initialize() throws GridCoverageException {
        crs = MapUtil.constructNorthPolarStereo(majorAxis, minorAxis, lad, lov);
        crsWKT = crs.toWKT();
        generateGeometry();
    }

    @Override
    protected GridCoverage cloneImplCrsParameters(SubGrid subGrid) {
        PolarStereoGridCoverage rval = new PolarStereoGridCoverage();
        rval.lov = this.lov;
        rval.majorAxis = this.majorAxis;
        rval.minorAxis = this.minorAxis;
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.gridcoverage.GridCoverage#getWorldWrapCount()
     */
    @Override
    public int getWorldWrapCount() {
        /* PolarStereographic grids cannot wrap */
        return GridGeometryWrapChecker.NO_WRAP;
    }

    @Override
    public String getProjectionType() {
        return PROJECTION_TYPE;
    }

    /**
     * Gets lov
     * 
     * @return The lov longitude value
     */
    public double getLov() {
        return lov;
    }

    /**
     * Sets lov
     * 
     * @param lov
     *            The lov longitude value
     */
    public void setLov(double lov) {
        this.lov = lov;
    }

    /**
     * @return the lad
     */
    public double getLad() {
        return lad;
    }

    /**
     * @param lad
     *            the lad to set
     */
    public void setLad(double lad) {
        this.lad = lad;
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
        PolarStereoGridCoverage other = (PolarStereoGridCoverage) obj;
        if (Double.doubleToLongBits(dx) != Double.doubleToLongBits(other.dx)) {
            return false;
        }
        if (Double.doubleToLongBits(dy) != Double.doubleToLongBits(other.dy)) {
            return false;
        }
        if (Double.doubleToLongBits(la1) != Double.doubleToLongBits(other.la1)) {
            return false;
        }
        if (Double.doubleToLongBits(lo1) != Double.doubleToLongBits(other.lo1)) {
            return false;
        }
        if (Double.doubleToLongBits(lov) != Double.doubleToLongBits(other.lov)) {
            return false;
        }
        if (Double.doubleToLongBits(lad) != Double.doubleToLongBits(other.lad)) {
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
        return true;
    }

    @Override
    public boolean spatialEquals(GridCoverage other) {
        if (super.spatialEquals(other)) {
            PolarStereoGridCoverage otherPolar = (PolarStereoGridCoverage) other;
            if (Math.abs(lad - otherPolar.lad) > SPATIAL_TOLERANCE_DEG) {
                return false;
            } else if (Math.abs(lov - otherPolar.lov) > SPATIAL_TOLERANCE_DEG) {
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
        key.append(lov);
        key.append(DataURI.SEPARATOR);
        key.append(lad);
        return key.toString();
    }

    public PolarStereoGridCoverage() {

    }

    public PolarStereoGridCoverage(PolarStereoGridCoverage coverage) {
        super(coverage);
        this.minorAxis = coverage.minorAxis;
        this.majorAxis = coverage.majorAxis;
        this.lov = coverage.lov;
        this.lad = coverage.lad;
    }
}
