/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.common.nc4.cf;

/**
 * CF Conventions as described for NetCDF library 1.6
 * Place holder for convention descriptions and names
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 29, 2013            ekladstrup     Initial creation
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0	
 */

public enum CFConventions {

    CF16("CF-1.6");

    /**
     * @param value
     */
    private CFConventions(final String value) {
        this.value = value;
    }

    private final String value;

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString() {
        return value;
    }

}
