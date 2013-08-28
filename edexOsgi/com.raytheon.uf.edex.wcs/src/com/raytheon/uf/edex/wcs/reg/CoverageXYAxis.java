/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wcs.reg;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;

/**
 * Metadata for coverage X and Y axis
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class CoverageXYAxis {

    private final GridGeometry2D gridGeometry;

    private final ReferencedEnvelope envelope;

    /**
     * @param gridGeometry
     * @param crs
     */
    public CoverageXYAxis(GridGeometry2D gridGeometry,ReferencedEnvelope envelope) {
        this.gridGeometry = gridGeometry;
        this.envelope = envelope;
    }

    /**
     * @return the gridGeometry
     */
    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    /**
     * @return the envelope
     */
    public ReferencedEnvelope getEnvelope() {
        return envelope;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((envelope == null) ? 0 : envelope.hashCode());
        result = prime * result
                + ((gridGeometry == null) ? 0 : gridGeometry.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        CoverageXYAxis other = (CoverageXYAxis) obj;
        if (envelope == null) {
            if (other.envelope != null)
                return false;
        } else if (!envelope.equals(other.envelope))
            return false;
        if (gridGeometry == null) {
            if (other.gridGeometry != null)
                return false;
        } else if (!gridGeometry.equals(other.gridGeometry))
            return false;
        return true;
    }

}
