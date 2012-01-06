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
package com.raytheon.viz.grid.spatial;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grib.spatial.projections.GridCoverage;
import com.raytheon.uf.common.dataplugin.grib.subgrid.SubGrid;
import com.raytheon.uf.common.dataplugin.grib.subgrid.SubGridDef;

/**
 * Stereographic Coverage used by radar data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2010  #4473     rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class StereographicCoverage extends GridCoverage {
    private Integer nx;

    private Integer ny;

    @Override
    public void generateName() {
    }

    @Override
    public String getProjectionType() {
        return "Stereographic";
    }

    @Override
    public void initialize() throws GribException {
    }

    @Override
    public GridCoverage trim(SubGridDef subGridDef, SubGrid subGrid) {
        // NOT SUPPORTED
        return null;
    }

    @Override
    public Integer getNx() {
        return nx;
    }

    @Override
    public Integer getNy() {
        return ny;
    }

    public void setNx(Integer nx) {
        this.nx = nx;
    }

    public void setNy(Integer ny) {
        this.ny = ny;
    }

    @Override
    public int generateHash() {
        HashCodeBuilder hashBuilder = new HashCodeBuilder();
        hashBuilder.append(nx);
        hashBuilder.append(ny);
        hashBuilder.append(gridGeometry);
        hashBuilder.append(crs);
        return hashBuilder.toHashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        StereographicCoverage other = (StereographicCoverage) obj;
        if (nx == null) {
            if (other.nx != null)
                return false;
        } else if (!nx.equals(other.nx))
            return false;
        if (ny == null) {
            if (other.ny != null)
                return false;
        } else if (!ny.equals(other.ny))
            return false;
        if (gridGeometry == null) {
            if (other.gridGeometry != null)
                return false;
        } else if (!gridGeometry.equals(other.gridGeometry))
            return false;
        if (crs == null) {
            if (other.crs != null)
                return false;
        } else if (!crs.equals(other.crs))
            return false;
        return true;
    }
}
