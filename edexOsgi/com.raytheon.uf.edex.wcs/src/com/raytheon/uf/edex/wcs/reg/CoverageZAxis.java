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

import java.util.Arrays;

/**
 * Metadata for coverage Z axis
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
public class CoverageZAxis {

    private final double[] value;

    private final String units;

    private final String reference;

    private final boolean upIsPositive;

    /**
     * @param value
     * @param units
     * @param upIsPositive
     */
    public CoverageZAxis(double[] value, String units, String reference,
            boolean upIsPositive) {
        this.value = value;
        this.units = units;
        this.reference = reference;
        this.upIsPositive = upIsPositive;
    }

    /**
     * @return the value
     */
    public double[] getValue() {
        return value;
    }

    /**
     * @return the units
     */
    public String getUnits() {
        return units;
    }

    /**
     * @return the upIsPositive
     */
    public boolean isUpIsPositive() {
        return upIsPositive;
    }

    /**
     * @return the reference
     */
    public String getReference() {
        return reference;
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
                + ((reference == null) ? 0 : reference.hashCode());
        result = prime * result + ((units == null) ? 0 : units.hashCode());
        result = prime * result + (upIsPositive ? 1231 : 1237);
        result = prime * result + Arrays.hashCode(value);
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
        CoverageZAxis other = (CoverageZAxis) obj;
        if (reference == null) {
            if (other.reference != null)
                return false;
        } else if (!reference.equals(other.reference))
            return false;
        if (units == null) {
            if (other.units != null)
                return false;
        } else if (!units.equals(other.units))
            return false;
        if (upIsPositive != other.upIsPositive)
            return false;
        if (!Arrays.equals(value, other.value))
            return false;
        return true;
    }

}
