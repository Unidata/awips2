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
package com.raytheon.viz.core.graphing.xy;

import java.util.ArrayList;

import com.raytheon.viz.core.graphing.GraphUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * List of xy data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 16, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class XYDataList implements IInspectableXYList {

    protected ArrayList<XYData> data;

    public XYDataList() {
        data = new ArrayList<XYData>();
    }

    /**
     * @return the data
     */
    public ArrayList<XYData> getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(ArrayList<XYData> data) {
        this.data = data;
    }

    /**
     * add all the items from one XYDataList into this list
     * 
     * @param dat
     */
    public void addAll(XYDataList dat) {
        data.addAll(dat.getData());
    }

    @Override
    public double[] inspectXY(Coordinate coord) {
        double resultY = Double.NaN;
        double xVal = coord.x;
        XYData closestBelow = null;
        XYData closestAbove = null;
        Double exactMatch = null;
        double lowerDiff = Double.POSITIVE_INFINITY;
        double upperDiff = Double.NEGATIVE_INFINITY;
        for (XYData d : data) {
            double dx = GraphUtil.getNumberRepresentation(d.getX());
            double diff = xVal - dx;
            if (diff == 0) {
                exactMatch = ((Number) d.getY()).doubleValue();
                break;
            }
            if (diff > 0 && diff < lowerDiff) {
                lowerDiff = diff;
                closestBelow = d;
            }
            if (diff < 0 && diff > upperDiff) {
                upperDiff = diff;
                closestAbove = d;
            }
        }

        if (exactMatch != null) {
            resultY = exactMatch;
        } else if (closestBelow != null && closestAbove != null) {
            double x1 = GraphUtil.getNumberRepresentation(closestBelow.getX());
            double x2 = GraphUtil.getNumberRepresentation(closestAbove.getX());
            double y1 = GraphUtil.getNumberRepresentation(closestBelow.getY());
            double y2 = GraphUtil.getNumberRepresentation(closestAbove.getY());
            resultY = ((xVal - x1) * (y2 - y1) / (x2 - x1)) + y1;
        }

        // TODO maybe support determing x value
        double resultX = Double.NaN;

        return new double[] { resultX, resultY };
    }

    public void dispose() {
        for (XYData item : data) {
            if (item instanceof XYImageData) {
                ((XYImageData) item).dispose();
            }
        }
    }
}
