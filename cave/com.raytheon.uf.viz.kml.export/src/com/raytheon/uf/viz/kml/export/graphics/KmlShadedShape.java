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
package com.raytheon.uf.viz.kml.export.graphics;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

import de.micromata.opengis.kml.v_2_2_0.Folder;
import de.micromata.opengis.kml.v_2_2_0.Geometry;
import de.micromata.opengis.kml.v_2_2_0.LinearRing;
import de.micromata.opengis.kml.v_2_2_0.Placemark;
import de.micromata.opengis.kml.v_2_2_0.Polygon;

/**
 * Implementation of shaded shape that can create a generator for making KML
 * filled polygons. KML has no concept of a fill pattern so it is completly
 * ignored and all polygons are filled with color.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlShadedShape implements IShadedShape {

    private Map<RGB, List<Polygon>> polygons = new HashMap<RGB, List<Polygon>>();

    private final GeneralGridGeometry gridGeometry;

    public KmlShadedShape(GeneralGridGeometry gridGeometry) {
        this.gridGeometry = gridGeometry;
    }

    @Override
    public void compile() {

    }

    @Override
    public boolean isMutable() {
        return true;
    }

    @Override
    public boolean isDrawable() {
        return true;
    }

    @Override
    public void dispose() {
        polygons.clear();
    }

    @Override
    public void reset() {
        polygons.clear();
    }

    @Override
    public void addPolygon(LineString[] lineString, RGB color) {
        Polygon p = new Polygon();
        LinearRing outer = p.createAndSetOuterBoundaryIs()
                .createAndSetLinearRing();
        for (Coordinate c : lineString[0].getCoordinates()) {
            outer.addToCoordinates(c.x, c.y);
        }
        for (int i = 1; i < lineString.length; i += 1) {
            LinearRing inner = p.createAndAddInnerBoundaryIs()
                    .createAndSetLinearRing();
            for (Coordinate c : lineString[i].getCoordinates()) {
                inner.addToCoordinates(c.x, c.y);
            }
        }
        List<Polygon> polygons = this.polygons.get(color);
        if (polygons == null) {
            polygons = new ArrayList<Polygon>();
            this.polygons.put(color, polygons);
        }
        polygons.add(p);
    }

    @Override
    public void addPolygonPixelSpace(LineString[] contours, RGB color) {
        try {
            MathTransform transform = TransformFactory.gridToLatLon(
                    gridGeometry, PixelInCell.CELL_CENTER);
            double[] loc = new double[2];
            LineString[] newContours = new LineString[contours.length];
            for (int i = 0; i < contours.length; i += 1) {
                List<Coordinate> newCoordinates = new ArrayList<Coordinate>();
                for (Coordinate c : contours[i].getCoordinates()) {
                    loc = new double[] { c.x, c.y };
                    transform.transform(loc, 0, loc, 0, 1);
                    newCoordinates.add(new Coordinate(loc[0], loc[1]));
                }
                newContours[i] = contours[i].getFactory().createLineString(
                        newCoordinates.toArray(new Coordinate[0]));
            }
            addPolygon(newContours, color);
        } catch (TransformException e) {
            throw new IllegalStateException(e);
        } catch (FactoryException e) {
            throw new IllegalStateException(e);
        }

    }

    @Override
    public void setFillPattern(byte[] pattern) {

    }

    public static class Generator extends KmlFeatureGenerator {

        private final double alpha;

        private final Map<RGB, List<Polygon>> polygons;

        public Generator(double alpha, KmlShadedShape shape) {
            this.alpha = alpha;
            this.polygons = new HashMap<RGB, List<Polygon>>(shape.polygons);
        }

        @Override
        public void addFeature(KmlOutputManager outputManager) {
            if (polygons.size() == 1) {
                Entry<RGB, List<Polygon>> entry = polygons.entrySet()
                        .iterator().next();
                outputManager.addFeature(getPlacemark(outputManager,
                        entry.getKey(), entry.getValue()));
            } else if (!polygons.isEmpty()) {
                Folder folder = new Folder();
                folder.setName("Shaded Shapes");
                for (Entry<RGB, List<Polygon>> entry : polygons.entrySet()) {
                    folder.addToFeature(getPlacemark(outputManager,
                            entry.getKey(), entry.getValue()));
                }
                outputManager.addFeature(folder);
            }
        }

        private Placemark getPlacemark(KmlOutputManager out, RGB color,
                List<Polygon> polygons) {
            Placemark placemark = new Placemark();
            placemark.setName("Shaded Shape");

            de.micromata.opengis.kml.v_2_2_0.Style kmlStyle = new de.micromata.opengis.kml.v_2_2_0.Style();

            kmlStyle.createAndSetPolyStyle().withFill(true)
                    .withColor(toColorStr(alpha, color));
            kmlStyle.createAndSetLineStyle()
                    .withColor(toColorStr(alpha, color));
            placemark.setStyleUrl(out.getStyleUrl(kmlStyle));
            if (polygons.size() == 1) {
                placemark.setGeometry(polygons.get(0));
            } else {
                placemark.createAndSetMultiGeometry().setGeometry(
                        new ArrayList<Geometry>(polygons));
            }
            return placemark;
        }

    }

}
