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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;

/**
 * Drawable line object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DrawableLine extends AbstractDrawableObject {

    public List<double[]> points = new ArrayList<double[]>();

    public LineStyle lineStyle = LineStyle.SOLID;

    public float width = 1.0f;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractDrawableObject#setCoordinates(double,
     * double, double)
     */
    @Override
    public void setCoordinates(double x, double y, double z) {
        super.setCoordinates(x, y, z);
        points.clear();
        addPoint(x, y, z);
    }

    /**
     * @param x
     * @param y
     */
    public void addPoint(double x, double y) {
        addPoint(x, y, 0);
    }

    /**
     * @param x
     * @param y
     * @param z
     */
    public void addPoint(double x, double y, double z) {
        points.add(new double[] { x, y, z });
    }

}
