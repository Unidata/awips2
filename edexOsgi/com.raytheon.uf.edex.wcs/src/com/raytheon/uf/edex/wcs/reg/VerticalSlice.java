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
 * Apr 2, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class VerticalSlice implements Comparable<VerticalSlice> {

    private final IDataRecordFetcher record;

    private final double level;

    /**
     * @param record
     * @param level
     */
    public VerticalSlice(IDataRecordFetcher record, double level) {
        this.record = record;
        this.level = level;
    }

    /**
     * @return the record
     */
    public IDataRecordFetcher getRecord() {
        return record;
    }

    /**
     * @return the level
     */
    public double getLevel() {
        return level;
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
        long temp;
        temp = Double.doubleToLongBits(level);
        result = prime * result + (int) (temp ^ (temp >>> 32));
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
        VerticalSlice other = (VerticalSlice) obj;
        if (Double.doubleToLongBits(level) != Double
                .doubleToLongBits(other.level))
            return false;
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(VerticalSlice o) {
        if (o == null) {
            return 1;
        }
        return Double.compare(this.level, o.level);
    }

}
