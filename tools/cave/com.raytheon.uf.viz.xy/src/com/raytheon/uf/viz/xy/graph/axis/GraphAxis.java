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
package com.raytheon.uf.viz.xy.graph.axis;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Default axis class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GraphAxis implements IAxis {

    private boolean drawAxis = true;

    private Coordinate[] coords = new Coordinate[2];

    private LineStyle lineStyle = null;

    private double value;

    @Override
    public Coordinate[] getCoordinates() {
        return coords;
    }

    @Override
    public boolean isDrawAxis() {
        return drawAxis;
    }

    @Override
    public void setDrawAxis(boolean drawAxis) {
        this.drawAxis = drawAxis;
    }

    @Override
    public void setEndLoc(Coordinate end) {
        coords[1] = end;
    }

    @Override
    public void setStartLoc(Coordinate start) {
        coords[0] = start;
    }

    @Override
    public LineStyle getLineStyle() {
        return lineStyle;
    }

    @Override
    public void setLineStyle(LineStyle style) {
        this.lineStyle = style;
    }

    @Override
    public double getDiscreteValue() {
        return value;
    }

    @Override
    public void setDiscreteValue(double val) {
        value = val;
    }

}
