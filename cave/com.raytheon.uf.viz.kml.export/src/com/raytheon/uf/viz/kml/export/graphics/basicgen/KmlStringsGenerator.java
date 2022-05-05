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
package com.raytheon.uf.viz.kml.export.graphics.basicgen;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

import de.micromata.opengis.kml.v_2_2_0.Coordinate;
import de.micromata.opengis.kml.v_2_2_0.IconStyle;
import de.micromata.opengis.kml.v_2_2_0.LabelStyle;
import de.micromata.opengis.kml.v_2_2_0.MultiGeometry;
import de.micromata.opengis.kml.v_2_2_0.Placemark;
import de.micromata.opengis.kml.v_2_2_0.Point;

/**
 * Generates KML Placemark Labels for DrawableStrings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlStringsGenerator extends KmlFeatureGenerator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlStringsGenerator.class);

    private final Collection<DrawableString> parameters;

    public KmlStringsGenerator(Collection<DrawableString> parameters) {
        this.parameters = parameters;
    }

    @Override
    public void addFeature(KmlOutputManager outputManager) {
        Collection<DrawableString> parameters = mergeDuplicatePoints();
        if (parameters.size() > 1) {
            outputManager = outputManager.createFolder("Labels");
        }
        Map<String, Placemark> redundantPlacemarks = new HashMap<String, Placemark>();
        for (DrawableString dstring : parameters) {
            RGB color = dstring.getColors()[0];
            for (RGB dcolor : dstring.getColors()) {
                if (!dcolor.equals(color)) {
                    statusHandler.handle(Priority.INFO,
                            "Multicolor labels will be one color in kml");
                }
                break;
            }
            String colorStr = toColorStr(dstring.basics.alpha, color);
            StringBuilder text = new StringBuilder(dstring.getText()[0]);
            for (int i = 1; i < dstring.getText().length; i++) {
                text.append(" ");
                text.append(dstring.getText()[i]);
            }
            Placemark placemark = redundantPlacemarks.get(text.toString()
                    + colorStr + dstring.magnification);
            if (placemark == null) {
                // google earth handles multiple points in a single placemark
                // faster than multiple placemarks with a single point, so
                // combine wherever possible
                de.micromata.opengis.kml.v_2_2_0.Style kmlStyle = new de.micromata.opengis.kml.v_2_2_0.Style();
                LabelStyle kmlLabelStyle = kmlStyle.createAndSetLabelStyle();
                IconStyle kmlIconStyle = kmlStyle.createAndSetIconStyle();
                kmlLabelStyle.setColor(colorStr);
                double magnifiaction = dstring.magnification;
                if (dstring.font != null) {
                    magnifiaction *= dstring.font.getMagnification();
                }
                kmlLabelStyle.setScale(magnifiaction);
                kmlIconStyle.setScale(0);
                placemark = new Placemark();
                placemark.setName(text.toString());
                placemark.setStyleUrl(outputManager.getStyleUrl(kmlStyle));
                outputManager.addFeature(placemark);
                redundantPlacemarks.put(text.toString() + colorStr
                        + dstring.magnification, placemark);
            }
            Point point = null;
            if (placemark.getGeometry() == null) {
                point = placemark.createAndSetPoint();
            } else if (placemark.getGeometry() instanceof Point) {
                point = (Point) placemark.getGeometry();
                MultiGeometry multi = placemark.createAndSetMultiGeometry();
                multi.addToGeometry(point);
                point = multi.createAndAddPoint();
            } else {
                MultiGeometry multi = (MultiGeometry) placemark.getGeometry();
                point = multi.createAndAddPoint();
            }
            try {
                Coordinate loc = transformToLatLon(dstring.basics.x,
                        dstring.basics.y);
                point.addToCoordinates(loc.getLongitude(), loc.getLatitude());
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

    }

    private Collection<DrawableString> mergeDuplicatePoints() {
        Map<Coordinate, DrawableString> pointMap = new HashMap<Coordinate, DrawableString>();
        for (DrawableString dstring : parameters) {
            Coordinate c = new Coordinate(dstring.basics.x, dstring.basics.y);
            if (pointMap.containsKey(c)) {
                DrawableString inMap = pointMap.get(c);
                DrawableString dstring2 = inMap;
                // try determine which of the two dtrings would be considered
                // "first"
                if (dstring2.verticallAlignment != dstring.verticallAlignment) {
                    if (dstring2.verticallAlignment == VerticalAlignment.BOTTOM
                            || dstring.verticallAlignment == VerticalAlignment.TOP) {
                        DrawableString tmp = dstring2;
                        dstring2 = dstring;
                        dstring = tmp;
                    }
                } else if (dstring2.horizontalAlignment != dstring.horizontalAlignment) {
                    if (dstring2.horizontalAlignment == HorizontalAlignment.RIGHT
                            || dstring.horizontalAlignment == HorizontalAlignment.LEFT) {
                        DrawableString tmp = dstring2;
                        dstring2 = dstring;
                        dstring = tmp;
                    }
                }
                String[] text1 = dstring.getText();
                String[] text2 = dstring2.getText();
                RGB[] colors1 = dstring.getColors();
                RGB[] colors2 = dstring2.getColors();
                String[] text = Arrays.copyOf(text1, text1.length
                        + text2.length);
                System.arraycopy(text2, 0, text, text1.length, text2.length);
                RGB[] colors = Arrays.copyOf(colors1, colors1.length
                        + colors2.length);
                System.arraycopy(colors2, 0, colors, colors1.length,
                        colors2.length);
                inMap.setText(text, colors);
            } else {
                pointMap.put(c, dstring);
            }
        }
        return pointMap.values();
    }

}
