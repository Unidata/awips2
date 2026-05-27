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

import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Polygon;

/**
 * Determines positioning for text boxes so they do not overlap
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 25, 2009 3099       bsteffen     Initial creation
 * Sep 19, 2016 5886       tgurney      No longer use wireframe shapes
 * Mar 19, 2018 7234       njensen      Change getBox()  arg from Polygon to Geometry
 *
 * </pre>
 *
 * @author bsteffen
 */
public class Declutter {

    /**
     * Dumb container that stores the location and dimensions of a text box,
     * with an additional line coming off of it.
     */
    public static class TextBoxData {
        public PixelExtent box;

        public double[] textLoc;

        public DrawableLine line;
    }

    private IExtent extent;

    public Declutter(IExtent extent) {
        this.extent = extent;
    }

    private static Polygon getBox(Coordinate center, double[] dimensions) {
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

    private Collection<Polygon> boxes = new ArrayList<>();

    public TextBoxData infoBoxForPolygon2(Geometry polygon,
            double[] boxDimensions) {
        Coordinate center = polygon.getCentroid().getCoordinate();
        Coordinate lastCoord = center;
        double boxRadius = Math.sqrt(boxDimensions[0] * boxDimensions[0]
                + boxDimensions[1] * boxDimensions[1]);
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
                if (!extent
                        .contains(new double[] { thisCoord.x, thisCoord.y })) {
                    continue;
                }
                double angle = Math.atan2(center.y - thisCoord.y,
                        thisCoord.x - center.x);

                Coordinate boxCenter = new Coordinate(
                        thisCoord.x + Math.cos(angle) * distance,
                        thisCoord.y - Math.sin(angle) * distance);
                Polygon box = getBox(boxCenter, boxDimensions);
                boolean createTextBox = true;
                for (Coordinate coord : box.getCoordinates()) {
                    if (!extent.contains(new double[] { coord.x, coord.y })) {
                        createTextBox = false;
                        break;
                    }
                }
                if (!createTextBox) {
                    continue;
                }
                if (polygon.intersects(box)) {
                    continue;
                }
                for (Polygon oldBox : boxes) {
                    if (oldBox.intersects(box)) {
                        createTextBox = false;
                        break;
                    }
                }
                if (createTextBox) {
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
                    TextBoxData textBox = new TextBoxData();
                    textBox.line = new DrawableLine();
                    textBox.line.addPoint(bestEdge.x, bestEdge.y);
                    textBox.line.addPoint(thisCoord.x, thisCoord.y);
                    textBox.box = new PixelExtent(coords[0].x, coords[2].x,
                            coords[1].y, coords[3].y);
                    textBox.textLoc = new double[] { coords[0].x, coords[0].y };
                    return textBox;
                }
            }
        }
        return null;
    }

}
