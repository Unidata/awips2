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
package com.raytheon.viz.xdat;


/**
 * PC Data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 2, 2011  9209       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class XdatPcData implements Comparable<XdatPcData> {
    /** Location ID */
    private String lid = null;

    /** Actual precip value */
    private double value = 0;

    public XdatPcData() {

    }

    public XdatPcData(String lid, double value) {
        this.lid = lid;
        this.value = value;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the value
     */
    public double getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(double value) {
        this.value = value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(XdatPcData otherObj) {
        if (this.equals(otherObj)) {
            return 0;
        }
        
        if (this.getValue() > otherObj.getValue()) {
            return -1;
        } else if (this.getValue() == otherObj.getValue()) {
            // if values are equal then sort by location id
            return this.getLid().compareTo(otherObj.getLid());
        } else {
            return 1;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof XdatPcData) {
            XdatPcData other = (XdatPcData) obj;
            if ((other.getLid().equals(this.getLid())) &&
                    (other.getValue() == this.getValue())) {
                return true;
            }
        }
        return false;
    }
}
