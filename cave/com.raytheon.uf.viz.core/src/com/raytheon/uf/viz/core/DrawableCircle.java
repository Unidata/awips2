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

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

/**
 * Class that stores information about how to draw a circle or arc of a circle
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

public class DrawableCircle extends AbstractDrawableObject {

    /**
     * fixed world size, when zooming the circle size will appear to grow/shrink
     * on the screen. Mutually exclusive with {@link #screenRadius}
     */
    public Double radius = null;

    /**
     * fixed screen size ( in pixels ), when zooming the proper radius will be
     * calculated so the circle size stays the same on the screen. Mutually
     * exclusive with {@link #radius}
     */
    public Double screenRadius = null;

    /** The width of the circle's line (non-filled) */
    public float lineWidth = 1.0f;

    /** The line style of the circle's line (non-filled) */
    public LineStyle lineStyle = LineStyle.SOLID;

    /** Whether the circle is filled or not */
    public boolean filled = false;

    /** The number of points to use for the circle */
    public int numberOfPoints = 360;

    /** Start azimuth of the circle in degrees (0.0) */
    public float startAzimuth = 0.0f;

    /** End azimuth of the circle in degrees (360.0) */
    public float endAzimuth = 360.0f;

    /**
     * If the start/end azimuth are < 360 def apart, this will specify if the
     * sides of the arc should be drawn
     */
    public boolean includeSides = true;

}
