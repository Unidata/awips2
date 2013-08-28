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


/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class CoverageDimensions {

    private final CoverageXYAxis xyAxis;

    private final CoverageZAxis zAxis;

    private final CoverageTAxis tAxis;

    /**
     * @param xyAxis
     * @param zAxis
     * @param times
     */
    public CoverageDimensions(CoverageXYAxis xyAxis, CoverageZAxis zAxis,
            CoverageTAxis tAxis) {
        this.xyAxis = xyAxis;
        this.zAxis = zAxis;
        this.tAxis = tAxis;
    }

    /**
     * @return the xyAxis
     */
    public CoverageXYAxis getXyAxis() {
        return xyAxis;
    }

    /**
     * @return the zAxis
     */
    public CoverageZAxis getZAxis() {
        return zAxis;
    }

    /**
     * @return the tAxis
     */
    public CoverageTAxis getTAxis() {
        return tAxis;
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
        result = prime * result + ((tAxis == null) ? 0 : tAxis.hashCode());
        result = prime * result + ((xyAxis == null) ? 0 : xyAxis.hashCode());
        result = prime * result + ((zAxis == null) ? 0 : zAxis.hashCode());
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
        CoverageDimensions other = (CoverageDimensions) obj;
        if (tAxis == null) {
            if (other.tAxis != null)
                return false;
        } else if (!tAxis.equals(other.tAxis))
            return false;
        if (xyAxis == null) {
            if (other.xyAxis != null)
                return false;
        } else if (!xyAxis.equals(other.xyAxis))
            return false;
        if (zAxis == null) {
            if (other.zAxis != null)
                return false;
        } else if (!zAxis.equals(other.zAxis))
            return false;
        return true;
    }

}
