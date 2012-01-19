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
package com.raytheon.uf.viz.core;

/**
 * Abstract drawable object, contains basic information needed to render
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AbstractDrawableObject {

    public final DrawableBasics basics = new DrawableBasics();

    /**
     * Set the x,y coordinates to draw the string at
     * 
     * @param x
     * @param y
     */
    public void setCoordinates(double x, double y) {
        setCoordinates(x, y, 0);
    }

    /**
     * Set the x,y,z coordinates to draw the string at
     * 
     * @param x
     * @param y
     * @param z
     */
    public void setCoordinates(double x, double y, double z) {
        this.basics.x = x;
        this.basics.y = y;
        this.basics.z = z;
    }
}
