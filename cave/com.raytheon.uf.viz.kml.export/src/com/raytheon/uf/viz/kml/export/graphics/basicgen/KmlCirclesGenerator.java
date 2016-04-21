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

import java.util.ArrayList;
import java.util.List;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

import de.micromata.opengis.kml.v_2_2_0.Placemark;

/**
 * Generates KML polygons for DrawableCircles.
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

public class KmlCirclesGenerator extends KmlFeatureGenerator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlLinesGenerator.class);

    private final DrawableCircle[] circles;

    public KmlCirclesGenerator(DrawableCircle[] circles) {
        this.circles = circles;
    }

    @Override
    public void addFeature(KmlOutputManager outputManager) {
        if (circles.length > 1) {
            outputManager = outputManager.createFolder("Circles");
        }
        for (DrawableCircle circle : circles) {
            String colorStr = toColorStr(circle.basics.alpha,
                    circle.basics.color);
            de.micromata.opengis.kml.v_2_2_0.Style kmlStyle = new de.micromata.opengis.kml.v_2_2_0.Style();
            kmlStyle.createAndSetLabelStyle().setScale(0);
            kmlStyle.createAndSetIconStyle().setScale(0);
            if (circle.filled) {
                kmlStyle.createAndSetPolyStyle().withFill(true)
                        .withColor(colorStr);
            } else {
                kmlStyle.createAndSetLineStyle().withWidth(circle.lineWidth)
                        .withColor(colorStr);
            }
            Placemark placemark = new Placemark();
            placemark.setName("Circle");
            placemark.setStyleUrl(outputManager.getStyleUrl(kmlStyle));
            List<double[]> pts = new ArrayList<double[]>();
            double step = 360.0 / (circle.numberOfPoints);
            double radius = circle.radius == null ? circle.screenRadius
                    : circle.radius;
            try {
                for (double i = 0; i <= circle.numberOfPoints; i++) {
                    double[] pt = new double[2];
                    pt[0] = circle.basics.x + radius
                            * Math.cos(Math.toRadians(i * step));
                    pt[1] = circle.basics.y + radius
                            * Math.sin(Math.toRadians(i * step));
                    pts.add(pt);
                }
                if (circle.filled) {
                    placemark.createAndSetPolygon()
                            .createAndSetOuterBoundaryIs()
                            .createAndSetLinearRing()
                            .withCoordinates(transformToLatLon(pts));

                } else {
                    placemark.createAndSetLineString().withCoordinates(
                            transformToLatLon(pts));
                }
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
            outputManager.addFeature(placemark);
        }
    }

}
