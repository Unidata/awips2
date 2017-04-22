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
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;
import com.vividsolutions.jts.geom.Coordinate;

import de.micromata.opengis.kml.v_2_2_0.Feature;
import de.micromata.opengis.kml.v_2_2_0.Folder;
import de.micromata.opengis.kml.v_2_2_0.Geometry;
import de.micromata.opengis.kml.v_2_2_0.LabelStyle;
import de.micromata.opengis.kml.v_2_2_0.LineString;
import de.micromata.opengis.kml.v_2_2_0.LineStyle;
import de.micromata.opengis.kml.v_2_2_0.Placemark;
import de.micromata.opengis.kml.v_2_2_0.Point;
import de.micromata.opengis.kml.v_2_2_0.Style;

/**
 * 
 * Converts a wireframe shape into KML LineStrings and Labels.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlWireframeShape implements IWireframeShape {

    private List<Geometry> segments = new ArrayList<Geometry>();

    private Map<String, List<Geometry>> labels = new HashMap<String, List<Geometry>>();

    private final GeneralGridGeometry gridGeometry;

    public KmlWireframeShape(GeneralGridGeometry gridGeometry) {
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
        segments.clear();
        labels.clear();
    }

    @Override
    public void reset() {
        segments.clear();
        labels.clear();

    }

    @Override
    public void addLineSegment(Coordinate[] latLong) {
        LineString line = new LineString();
        for (Coordinate c : latLong) {
            line.addToCoordinates(round(c.x), round(c.y));
        }
        segments.add(line);

    }

    @Override
    public void addLineSegment(double[][] screenCoordinates) {
        try {
            MathTransform transform = TransformFactory.gridToLatLon(
                    gridGeometry, PixelInCell.CELL_CENTER);
            LineString line = new LineString();
            double[] out = new double[2];
            for (int i = 0; i < screenCoordinates.length; i += 1) {
                transform.transform(screenCoordinates[i], 0, out, 0, 1);
                line.addToCoordinates(round(out[0]), round(out[1]));
            }
            segments.add(line);
        } catch (TransformException e) {
            throw new IllegalStateException(e);
        } catch (FactoryException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * round a latitude or longitude, rounding can cut the size of the generated
     * kml and only moves the actual line by less than a meter.
     * 
     * @param value
     * @return
     */
    private double round(double value) {
        return ((long) (value * 100000)) / 100000.0;
    }

    @Override
    public void addLabel(String label, double[] screenCoordinate) {
        try {
            MathTransform transform = TransformFactory.gridToLatLon(
                    gridGeometry, PixelInCell.CELL_CENTER);
            double[] out = new double[2];
            transform.transform(screenCoordinate, 0, out, 0, 1);
            Point point = new Point();
            point.addToCoordinates(round(out[0]), round(out[1]));
            List<Geometry> points = labels.get(label);
            if (points == null) {
                points = new ArrayList<Geometry>();
                labels.put(label, points);
            }
            points.add(point);
        } catch (TransformException e) {
            throw new IllegalStateException(e);
        } catch (FactoryException e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    public void clearLabels() {
        labels.clear();
    }

    @Override
    public void allocate(int points) {
    }

    public static class Generator extends KmlFeatureGenerator {

        private final List<Geometry> segments;

        private final Map<String, List<Geometry>> labels;

        private final float alpha;

        private final RGB color;

        private final float lineWidth;

        public Generator(KmlWireframeShape shape, float alpha, RGB color,
                float lineWidth) {
            this.segments = new ArrayList<Geometry>(shape.segments);
            this.labels = new HashMap<String, List<Geometry>>(shape.labels);
            this.alpha = alpha;
            this.color = color;
            this.lineWidth = lineWidth;
        }

        @Override
        public void addFeature(KmlOutputManager outputManager) {
            if (segments.isEmpty() && labels.isEmpty()) {
                return;
            }
            String styleUrl = outputManager.getStyleUrl(getStyle());
            Feature lineFeature = getLineFeature(styleUrl);
            Feature labelFeature = getLabelFeature(styleUrl);
            if (lineFeature == null) {
                outputManager.addFeature(labelFeature);
            } else if (labelFeature == null) {
                outputManager.addFeature(lineFeature);
            } else {
                Folder folder = new Folder();
                folder.setName("LinesAndLabels");
                folder.addToFeature(lineFeature);
                folder.addToFeature(labelFeature);
                outputManager.addFeature(folder);
            }
        }

        private Style getStyle() {
            Style style = new de.micromata.opengis.kml.v_2_2_0.Style();
            LineStyle lineStyle = style.createAndSetLineStyle();
            LabelStyle labelStyle = style.createAndSetLabelStyle();
            style.createAndSetIconStyle().setScale(0);
            String colorStr = toColorStr(alpha, color);
            lineStyle.setColor(colorStr);
            lineStyle.setWidth(lineWidth);
            labelStyle.setColor(colorStr);
            labelStyle.setScale(1.0);
            return style;
        }

        private Feature getLineFeature(String styleUrl) {
            if (segments.isEmpty()) {
                return null;
            }
            Placemark placemark = new Placemark();
            placemark.setName("Lines");
            placemark.setStyleUrl(styleUrl);
            if (segments.size() == 1) {
                placemark.setGeometry(segments.get(0));
            } else {
                placemark.createAndSetMultiGeometry().setGeometry(
                        new ArrayList<Geometry>(segments));
            }
            return placemark;
        }

        private Feature getLabelFeature(String styleUrl) {
            if (labels.size() == 1) {
                for (Entry<String, List<Geometry>> entry : labels.entrySet()) {
                    return createLabelPlacemark(entry.getKey(),
                            entry.getValue(), styleUrl);
                }
            } else if (!labels.isEmpty()) {
                Folder folder = new Folder();
                folder.setName("Labels");
                for (Entry<String, List<Geometry>> entry : labels.entrySet()) {
                    Placemark placemark = createLabelPlacemark(entry.getKey(),
                            entry.getValue(), styleUrl);
                    folder.addToFeature(placemark);
                }
                return folder;
            }
            return null;
        }

        private Placemark createLabelPlacemark(String label,
                List<Geometry> points, String styleUrl) {
            Placemark placemark = new Placemark();
            placemark.setName(label);
            placemark.setStyleUrl(styleUrl);
            if (points.size() == 1) {
                placemark.setGeometry(points.get(0));
            } else {
                placemark.createAndSetMultiGeometry().setGeometry(points);
            }
            return placemark;
        }

    }

}
