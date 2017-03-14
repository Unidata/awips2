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

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

import de.micromata.opengis.kml.v_2_2_0.Coordinate;
import de.micromata.opengis.kml.v_2_2_0.LinearRing;
import de.micromata.opengis.kml.v_2_2_0.Placemark;
import de.micromata.opengis.kml.v_2_2_0.Style;

/**
 * Generates kml Polygon for both filled and not filled rectangles.
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

public class KmlRectGenerator extends KmlFeatureGenerator {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlRectGenerator.class);

    private final IExtent pe;

    private final RGB color;

    private final double alpha;

    private final float lineWidth;

    private final boolean filled;

    public KmlRectGenerator(IExtent pe, RGB color, double alpha,
            float lineWidth, boolean filled) {
        this.pe = pe;
        this.color = color;
        this.alpha = alpha;
        this.lineWidth = lineWidth;
        this.filled = filled;
    }

    @Override
    public void addFeature(KmlOutputManager outputManager) {
        Placemark placemark = new Placemark();
        placemark.setName("Rectangle");
        Style style = new de.micromata.opengis.kml.v_2_2_0.Style();
        style.createAndSetLabelStyle().setScale(0);
        style.createAndSetIconStyle().setScale(0);
        style.createAndSetLineStyle().withWidth(lineWidth)
                .withColor(toColorStr(alpha, color));
        style.createAndSetPolyStyle().withFill(filled)
                .withColor(toColorStr(alpha, color));

        try {
            LinearRing ring = placemark.createAndSetPolygon()
                    .createAndSetOuterBoundaryIs().createAndSetLinearRing();
            Coordinate corner = transformToLatLon(pe.getMinX(), pe.getMinY());
            ring.addToCoordinates(corner.getLongitude(), corner.getLatitude());
            corner = transformToLatLon(pe.getMaxX(), pe.getMinY());
            ring.addToCoordinates(corner.getLongitude(), corner.getLatitude());
            corner = transformToLatLon(pe.getMaxX(), pe.getMaxY());
            ring.addToCoordinates(corner.getLongitude(), corner.getLatitude());
            corner = transformToLatLon(pe.getMinX(), pe.getMaxY());
            ring.addToCoordinates(corner.getLongitude(), corner.getLatitude());
            corner = transformToLatLon(pe.getMinX(), pe.getMinY());
            ring.addToCoordinates(corner.getLongitude(), corner.getLatitude());

            placemark.setStyleUrl(outputManager.getStyleUrl(style));
            outputManager.addFeature(placemark);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (FactoryException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

}
