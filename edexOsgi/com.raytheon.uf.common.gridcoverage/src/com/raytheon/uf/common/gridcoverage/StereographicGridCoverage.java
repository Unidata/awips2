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
 * Stereographic Coverage used by radar data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2010  #4473     rjpeter     Initial creation
 * Mar 04, 2015  3959     rjpeter     Update for grid based subgridding.
 * Jun 24, 2016  ASM18440 dfriedman   Fix spatial tolerance for degree values.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@Entity
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class StereographicGridCoverage extends GridCoverage {
    private static final long serialVersionUID = -3420227375272208743L;

    /** Orientation of the grid */
    @Column
    @XmlElement
    @DynamicSerializeElement
    private double lov;

    @Column
    @XmlElement
    @DynamicSerializeElement
    private double lad;

    public double getLov() {
        return lov;
    }

    public void setLov(double lov) {
        this.lov = lov;
    }

    public double getLad() {
        return lad;
    }

    public void setLad(double lad) {
        this.lad = lad;
    }

    @Override
    public String getProjectionType() {
        return "Stereographic";
    }

    @Override
    public void initialize() throws GridCoverageException {
        crs = MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
                MapUtil.AWIPS_EARTH_RADIUS, lad, lov);
        crsWKT = crs.toWKT();
        generateGeometry();
    }

    @Override
    protected GridCoverage cloneImplCrsParameters(SubGrid subGrid) {
        StereographicGridCoverage rval = new StereographicGridCoverage();
        rval.lov = this.lov;
        rval.lad = this.lad;
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.gridcoverage.GridCoverage#getWorldWrapCount()
     */
    @Override
    public int getWorldWrapCount() {
        /* Stereographic grids cannot wrap */
        return GridGeometryWrapChecker.NO_WRAP;
    }

    @Override
    public int generateHash() {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(super.generateHash());
        hashBuilder.append(lov);
        hashBuilder.append(lad);
        return hashBuilder.toHashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!super.equals(obj)) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        StereographicGridCoverage other = (StereographicGridCoverage) obj;
        if (Double.doubleToLongBits(lad) != Double.doubleToLongBits(other.lad)) {
            return false;
        }
        if (Double.doubleToLongBits(lov) != Double.doubleToLongBits(other.lov)) {
            return false;
        }
        return true;
    }

    @Override
    public boolean spatialEquals(GridCoverage other) {
        if (super.spatialEquals(other)) {
            StereographicGridCoverage otherStereo = (StereographicGridCoverage) other;
            if (Math.abs(lad - otherStereo.lad) > SPATIAL_TOLERANCE_DEG) {
                return false;
            } else if (Math.abs(lov - otherStereo.lov) > SPATIAL_TOLERANCE_DEG) {
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
}
