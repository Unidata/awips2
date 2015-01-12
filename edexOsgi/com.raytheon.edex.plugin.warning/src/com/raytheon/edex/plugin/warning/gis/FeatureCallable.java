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
package com.raytheon.edex.plugin.warning.gis;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryCollectionIterator;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * This generates the clipped geometry between feature and CWA features.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2014 3352       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class FeatureCallable implements Callable<Geometry> {

    private final IUFStatusHandler statusHandler;

    /**
     * Assumed to be GeomIntersectonCallable futures for getting the
     * intersections between a feature and associated cwaFeatures.
     */
    private final List<Future<Geometry>> geomFutures;

    /**
     * Constructor.
     * 
     * @param jobNo
     * @param queue
     * @param jobQueue
     */
    public FeatureCallable(List<Future<Geometry>> geomFutures,
            IUFStatusHandler statusHandler) {
        super();
        this.geomFutures = geomFutures;
        this.statusHandler = statusHandler;
    }

    @Override
    public Geometry call() throws InterruptedException, ExecutionException {
        Geometry multiPolygon = null;
        Geometry[] clippedGeoms = null;
        multiPolygon = null;
        clippedGeoms = new Geometry[geomFutures.size()];

        // Wait for all intersections to finish.
        for (int index = 0; index < clippedGeoms.length; ++index) {
            Future<Geometry> future = geomFutures.get(index);
            Geometry clippedGeom = null;
            clippedGeom = future.get();
            clippedGeoms[index] = clippedGeom;
            if (clippedGeom instanceof GeometryCollection) {
                GeometryCollection gc = (GeometryCollection) clippedGeom;
                if (multiPolygon != null) {
                    multiPolygon = multiPolygon
                            .union(convertToMultiPolygon(gc));
                } else {
                    multiPolygon = convertToMultiPolygon(gc);
                }
            }
        }

        Geometry result = null;
        if (multiPolygon != null) {
            result = multiPolygon;
        } else if (clippedGeoms[clippedGeoms.length - 1] != null) {
            result = clippedGeoms[clippedGeoms.length - 1];
        }

        return result;
    }

    /**
     * Convert a GeometryCollection to a MultiPolygon.
     * 
     * @param gc
     */
    private MultiPolygon convertToMultiPolygon(GeometryCollection gc) {
        GeometryCollectionIterator iter = new GeometryCollectionIterator(gc);
        Set<Polygon> polygons = new HashSet<Polygon>();
        MultiPolygon mp = null;
        iter.next();
        while (iter.hasNext()) {
            Object o = iter.next();
            if (o instanceof MultiPolygon) {
                if (mp == null) {
                    mp = (MultiPolygon) o;
                } else {
                    mp = (MultiPolygon) mp.union((MultiPolygon) o);
                }
            } else if (o instanceof Polygon) {
                polygons.add((Polygon) o);
            } else if ((o instanceof LineString) || (o instanceof Point)) {
                LinearRing lr = null;
                Coordinate[] coords = null;
                if (o instanceof LineString) {
                    Coordinate[] cs = ((LineString) o).getCoordinates();
                    if (cs.length < 4) {
                        coords = new Coordinate[4];
                        for (int j = 0; j < cs.length; j++) {
                            coords[j] = new Coordinate(cs[j]);
                        }
                        for (int j = cs.length; j < 4; j++) {
                            coords[j] = new Coordinate(cs[3 - j]);
                        }
                    } else {
                        coords = new Coordinate[cs.length + 1];
                        for (int j = 0; j < cs.length; j++) {
                            coords[j] = new Coordinate(cs[j]);
                        }
                        coords[cs.length] = new Coordinate(cs[0]);
                    }
                } else {
                    coords = new Coordinate[4];
                    for (int i = 0; i < 4; i++) {
                        coords[i] = ((Point) o).getCoordinate();
                    }
                }
                lr = (((Geometry) o).getFactory()).createLinearRing(coords);
                Polygon poly = (new GeometryFactory()).createPolygon(lr, null);
                polygons.add(poly);
            } else {
                statusHandler.handle(Priority.WARN,
                        "Unprocessed Geometry object: "
                                + o.getClass().getName());
            }
        }
        if ((mp == null) && (polygons.size() == 0)) {
            return null;
        }
        if (polygons.size() > 0) {
            Polygon[] p = polygons.toArray(new Polygon[0]);
            if (mp != null) {
                mp = (MultiPolygon) mp.union(new MultiPolygon(p, gc
                        .getFactory()));
            } else {
                mp = new MultiPolygon(p, gc.getFactory());
            }
        }
        return mp;
    }
}