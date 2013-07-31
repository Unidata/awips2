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
import java.util.Date;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class CoverageTAxis {

    private final Date[] times;

    /**
     * @param times
     */
    public CoverageTAxis(Date[] times) {
        this.times = times;
    }

    /**
     * @return the times
     */
    public Date[] getTimes() {
        return times;
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
        result = prime * result + Arrays.hashCode(times);
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
        CoverageTAxis other = (CoverageTAxis) obj;
        if (!Arrays.equals(times, other.times))
            return false;
        return true;
    }

}
