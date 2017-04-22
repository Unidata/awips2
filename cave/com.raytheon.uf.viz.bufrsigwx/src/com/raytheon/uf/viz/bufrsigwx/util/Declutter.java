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
package com.raytheon.uf.viz.bufrsigwx.util;

import java.util.ArrayList;
import java.util.Collection;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;


/**
 * Determines positioning for text boxes so they do not overlap
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 25, 2009 3099       bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class Declutter {

    IExtent extent;

    public Declutter(IExtent extent) {
        this.extent = extent;
    }

    private Polygon getBox(Coordinate center, double[] dimensions) {
        Coordinate[] coords = new Coordinate[5];
        double x1 = center.x - dimensions[0] / 2;
        double x2 = center.x + dimensions[0] / 2;
        double y1 = center.y - dimensions[1] / 2;
        double y2 = center.y + dimensions[1] / 2;
        coords[0] = coords[4] = new Coordinate(x1, y1);
        coords[1] = new Coordinate(x1, y2);
        coords[2] = new Coordinate(x2, y2);
        coords[3] = new Coordinate(x2, y1);
        GeometryFactory factory = new GeometryFactory();
        LinearRing ring = factory.createLinearRing(coords);
        return factory.createPolygon(ring, null);
    }

    private Collection<Polygon> boxes = new ArrayList<Polygon>();
    
    public double[] infoBoxForPolygon2(IWireframeShape shape, Polygon polygon,
            double[] dimensions) throws VizException {
        Coordinate center = polygon.getCentroid().getCoordinate();
        Coordinate lastCoord = center;
        double boxRadius = Math.sqrt(dimensions[0] * dimensions[0]
                + dimensions[1] * dimensions[1]);
        double distance = boxRadius * 0.8;
        for (Coordinate curCoord : polygon.getCoordinates()) {
            Coordinate thisCoord = curCoord;
            for (int j = 0; j < 2; ++j) {
                if (lastCoord != center && lastCoord != curCoord) {
                    thisCoord = new Coordinate((curCoord.x + lastCoord.x) / 2,
                            (curCoord.y + lastCoord.y) / 2);
                    lastCoord = curCoord;
                } else {
                    thisCoord = curCoord;
                }
                if (!extent.contains(new double[] { thisCoord.x, thisCoord.y })) {
                    continue;
                }
                double angle = Math.atan2(center.y - thisCoord.y, thisCoord.x
                        - center.x);

                Coordinate boxCenter = new Coordinate(thisCoord.x
                        + Math.cos(angle) * distance, thisCoord.y
                        - Math.sin(angle) * distance);
                Polygon box = getBox(boxCenter, dimensions);
                boolean flag = true;
                for (Coordinate coord : box.getCoordinates()) {
                    if (!extent.contains(new double[] { coord.x, coord.y })) {
                        flag = false;
                        break;
                    }
                }
                if (!flag) {
                    continue;
                }
                if (polygon.intersects(box)) {
                    continue;
                }
                for (Polygon oldBox : boxes) {
                    if (oldBox.intersects(box)) {
                        flag = false;
                        break;
                    }
                }
                if (flag) {
                    Coordinate[] coords = box.getCoordinates();
                    Coordinate[] edgePoints = new Coordinate[4];
                    edgePoints[0] = new Coordinate(coords[1].x, boxCenter.y);
                    edgePoints[1] = new Coordinate(boxCenter.x, coords[1].y);
                    edgePoints[2] = new Coordinate(coords[3].x, boxCenter.y);
                    edgePoints[3] = new Coordinate(boxCenter.x, coords[3].y);
                    Coordinate bestEdge = edgePoints[0];
                    double bestDistance = Double.MAX_VALUE;
                    for (Coordinate edgePoint : edgePoints) {
                        double dis = thisCoord.distance(edgePoint);
                        if (dis < bestDistance) {
                            bestEdge = edgePoint;
                            bestDistance = dis;
                        }
                    }
                    boxes.add(box);
                    double[][] line = new double[5][];
                    for (int i = 0; i < 5; i++) {
                        line[i] = new double[] { coords[i].x, coords[i].y };
                    }
                    shape.addLineSegment(new double[][] {
                            { bestEdge.x, bestEdge.y },
                            { thisCoord.x, thisCoord.y } });
                    shape.addLineSegment(line);
                    Coordinate ul = coords[0];
                    return new double[] { ul.x, ul.y };
                }
            }
        }
        return new double[0];
    }

}
