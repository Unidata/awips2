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

import javax.vecmath.Vector3d;

import com.raytheon.uf.viz.core.geom.Ray;

public interface IExtent extends Cloneable {

    /**
     * Get the min coordinate for the extent
     * 
     * @return
     */
    public Vector3d getMin();

    /**
     * Get the max coordinate for the extent
     * 
     * @return
     */
    public Vector3d getMax();

    public double getMaxX();

    public double getMaxY();

    public double getMinX();

    public double getMinY();

    public double getWidth();

    public double getHeight();

    /**
     * 
     * @param factor
     */
    public void scale(double factor);

    /**
     * 
     * @param factor
     * @param xCenter
     * @param yCenter
     */
    public void scaleAndBias(double factor, double xCenter, double yCenter);

    public double[] getCenter();

    public double getScale();

    /**
     * 
     * @param shiftX
     * @param shiftY
     */
    public void shift(double shiftX, double shiftY);

    /**
     * 
     * @param start
     * @param end
     */
    public void shift(Ray start, Ray end);

    /**
     * Determine if the two extents intersect
     * 
     * @param e
     * @return
     */
    public boolean intersects(IExtent e);

    /**
     * Returns the intersection of the extents
     * 
     * @param e
     * @return
     */
    public IExtent intersection(IExtent e);

    /**
     * Determine if the pixel is inside the extent
     * 
     * @param pixel
     * @return
     */
    public boolean contains(double[] pixel);

    /**
     * Get the enclosing radius
     * 
     * @return
     */
    public double getRadius();

    /**
     * Reset the extent to previous state
     * 
     * @return
     */
    public void reset();

    public IExtent clone();

}
