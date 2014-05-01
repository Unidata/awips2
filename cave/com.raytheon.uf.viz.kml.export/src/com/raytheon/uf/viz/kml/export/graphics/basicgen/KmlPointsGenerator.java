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

import java.awt.BasicStroke;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.Collection;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

import de.micromata.opengis.kml.v_2_2_0.Coordinate;
import de.micromata.opengis.kml.v_2_2_0.IconStyle;
import de.micromata.opengis.kml.v_2_2_0.LabelStyle;
import de.micromata.opengis.kml.v_2_2_0.MultiGeometry;
import de.micromata.opengis.kml.v_2_2_0.Placemark;
import de.micromata.opengis.kml.v_2_2_0.Point;

/**
 * Generates KML point icons.
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

public class KmlPointsGenerator extends KmlFeatureGenerator {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlStringsGenerator.class);

    private final Collection<double[]> locations;

    private final RGB color;

    private final PointStyle pointStyle;

    private final float magnification;

    public KmlPointsGenerator(Collection<double[]> locations, RGB color,
            PointStyle pointStyle, float magnification) {
        super();
        this.locations = locations;
        this.color = color;
        this.pointStyle = pointStyle;
        this.magnification = magnification;
    }

    @Override
    public void addFeature(KmlOutputManager outputManager) {
        BufferedImage image = new BufferedImage(15, 15,
                BufferedImage.TYPE_INT_ARGB);
        Graphics graphics = image.getGraphics();
        if (graphics instanceof Graphics2D) {
            ((Graphics2D) graphics).setStroke(new BasicStroke(3.0f));
        }
        graphics.setColor(new java.awt.Color(0, 0, 0, 0));
        graphics.fillRect(0, 0, 24, 24);
        graphics.setColor(new java.awt.Color(color.red, color.green, color.blue));
        switch (pointStyle) {
        case NONE:
            graphics.dispose();
            return;
        case SQUARE:
            graphics.fillRect(1, 1, 13, 13);
            break;
        case CIRCLE:
            graphics.drawOval(1, 1, 13, 13);
            break;
        case CROSS:
            graphics.drawLine(7, 0, 7, 14);
            graphics.drawLine(0, 7, 14, 7);
            break;
        case DASH:
            graphics.drawLine(0, 7, 14, 7);
            break;
        case POINT:
            graphics.fillOval(5, 5, 5, 5);
            break;
        case BOX:
            graphics.drawRect(1, 1, 13, 13);
            break;
        case STAR:
            graphics.drawLine(7, 0, 7, 14);
        case X:
            graphics.drawLine(0, 0, 14, 14);
            graphics.drawLine(0, 14, 14, 0);
            break;
        case DISC:
        default:
            graphics.fillOval(1, 1, 13, 13);
            break;
        }

        graphics.dispose();
        de.micromata.opengis.kml.v_2_2_0.Style kmlStyle = new de.micromata.opengis.kml.v_2_2_0.Style();
        LabelStyle kmlLabelStyle = kmlStyle.createAndSetLabelStyle();
        IconStyle kmlIconStyle = kmlStyle.createAndSetIconStyle();
        kmlIconStyle.createAndSetIcon().setHref(outputManager.addImage(image));
        kmlLabelStyle.setScale(0);
        kmlIconStyle.setScale(magnification * 0.5);
        Placemark placemark = new Placemark();
        placemark.setName(pointStyle.toString());
        placemark.setStyleUrl(outputManager.getStyleUrl(kmlStyle));
        try {
            if (locations.size() == 1) {
                Point p = placemark.createAndSetPoint();
                Coordinate loc = transformToLatLon(locations.iterator().next());
                p.addToCoordinates(loc.getLongitude(), loc.getLatitude());

            } else {
                MultiGeometry multi = placemark.createAndSetMultiGeometry();
                for (double[] location : locations) {
                    Point p = multi.createAndAddPoint();
                    Coordinate loc = transformToLatLon(location);
                    p.addToCoordinates(loc.getLongitude(), loc.getLatitude());
                }
            }
            outputManager.addFeature(placemark);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (FactoryException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }
}
