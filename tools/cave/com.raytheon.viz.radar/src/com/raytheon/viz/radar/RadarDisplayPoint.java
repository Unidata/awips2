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
package com.raytheon.viz.radar;

/**
 * Represents an i,j point on the Radar screen. Used to sort and maintain the
 * relationship of radar data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 2000       askripsk     Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class RadarDisplayPoint {

    private int i;

    private int j;

    /**
     * @return the i
     */
    public int getI() {
        return i;
    }

    /**
     * @param i
     *            the i to set
     */
    public void setI(int i) {
        this.i = i;
    }

    /**
     * @return the j
     */
    public int getJ() {
        return j;
    }

    /**
     * @param j
     *            the j to set
     */
    public void setJ(int j) {
        this.j = j;
    }

    @Override
    public boolean equals(Object o) {
        boolean rval = false;

        RadarDisplayPoint that = (RadarDisplayPoint) o;

        if (this == that) {
            rval = true;
        } else if (i == that.i && j == that.j) {
            rval = true;
        }

        return rval;

    }

    @Override
    public int hashCode() {
        return ((Integer) i).hashCode() + 31 * ((Integer) j).hashCode();
    }

    @Override
    public String toString() {
        return String.format("i: %s j: %s", i, j);
    }
}
