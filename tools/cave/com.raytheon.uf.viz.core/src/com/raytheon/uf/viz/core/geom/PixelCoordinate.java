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
package com.raytheon.uf.viz.core.geom;

/**
 * This class implements an x, y, z pixel space coordinate.
 * 
 * <pre>
 * 
 *                 SOFTWARE HISTORY
 *                
 *                 Date            Ticket#     Engineer    Description
 *                 ------------ ----------  ----------- --------------------------
 *                 8/08/08                     bgonzale    Initial Creation.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1
 */
public class PixelCoordinate {

    private static final int XIndex = 0;

    private static final int YIndex = 1;

    private static final int ZIndex = 2;

    private double[] xyzCoordinate;

    /**
     * Initialization Constructor.
     * 
     * @param xCoordinate
     */
    public PixelCoordinate(double xCoordinate) {
        this(xCoordinate, 0.0, 0.0);
    }

    /**
     * Initialization Constructor.
     * 
     * @param xCoordinate
     * @param yCoordinate
     */
    public PixelCoordinate(double xCoordinate, double yCoordinate) {
        this(xCoordinate, yCoordinate, 0.0);
    }

    /**
     * Initialization Constructor.
     * 
     * @param xCoordinate
     * @param yCoordinate
     */
    public PixelCoordinate(float xCoordinate, double yCoordinate) {
        this(xCoordinate, yCoordinate, 0.0);
    }

    /**
     * Initialization Constructor.
     * 
     * @param xCoordinate
     * @param yCoordinate
     */
    public PixelCoordinate(int xCoordinate, int yCoordinate) {
        this(xCoordinate, yCoordinate, 0.0);
    }

    /**
     * Initialization Constructor.
     * 
     * @param xCoordinate
     * @param yCoordinate
     * @param zCoordinate
     */
    public PixelCoordinate(double xCoordinate, double yCoordinate,
            double zCoordinate) {
        xyzCoordinate = new double[] { xCoordinate, yCoordinate, zCoordinate };
    }

    /**
     * Initialization Constructor.
     * 
     * @param xyzCoordinate
     *            a double array of the x, y, and z coordinates. It is order
     *            specific.
     */
    public PixelCoordinate(double[] xyzCoordinate) {
        this.xyzCoordinate = new double[3];

        if (xyzCoordinate != null) {
            // get passed in coordinates
            for (int i = 0; i < xyzCoordinate.length; ++i) {
                this.xyzCoordinate[i] = xyzCoordinate[i];
            }

            // initialize any coordinates not passed in
            for (int i = xyzCoordinate.length; i < this.xyzCoordinate.length; ++i) {
                this.xyzCoordinate[i] = 0.0;
            }
        }
    }

    /**
     * "Copy" Constructor.
     * 
     * @param p
     */
    public PixelCoordinate(PixelCoordinate p) {
        this(p.getX(), p.getY(), p.getZ());
    }

    public double[] getCoordinateArray() {
        return xyzCoordinate;
    }

    public double getX() {
        return xyzCoordinate[XIndex];
    }

    public double getY() {
        return xyzCoordinate[YIndex];
    }

    public double getZ() {
        return xyzCoordinate[ZIndex];
    }

    public void addToX(double d) {
        xyzCoordinate[XIndex] += d;
    }

    public void addToY(double d) {
        xyzCoordinate[YIndex] += d;
    }

    public void addToZ(double d) {
        xyzCoordinate[ZIndex] += d;
    }
}
