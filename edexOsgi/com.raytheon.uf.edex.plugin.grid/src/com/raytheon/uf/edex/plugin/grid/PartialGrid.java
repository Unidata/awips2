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
package com.raytheon.uf.edex.plugin.grid;

/**
 * Designates that the messageData of the GridRecord is only a partial grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2015 4868       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class PartialGrid {
    public static final String KEY = PartialGrid.class.getSimpleName();

    private int nx;

    private int ny;

    private int xOffset;

    private int yOffset;

    /**
     * @return the nx
     */
    public int getNx() {
        return nx;
    }

    /**
     * @param nx
     *            the nx to set
     */
    public void setNx(int nx) {
        this.nx = nx;
    }

    /**
     * @return the ny
     */
    public int getNy() {
        return ny;
    }

    /**
     * @param ny
     *            the ny to set
     */
    public void setNy(int ny) {
        this.ny = ny;
    }

    /**
     * @return the xOffset
     */
    public int getxOffset() {
        return xOffset;
    }

    /**
     * @param xOffset
     *            the xOffset to set
     */
    public void setxOffset(int xOffset) {
        this.xOffset = xOffset;
    }

    /**
     * @return the yOffset
     */
    public int getyOffset() {
        return yOffset;
    }

    /**
     * @param yOffset
     *            the yOffset to set
     */
    public void setyOffset(int yOffset) {
        this.yOffset = yOffset;
    }
}
