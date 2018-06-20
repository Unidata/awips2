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

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.kml.export.KmlFeatureGenerator;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

import de.micromata.opengis.kml.v_2_2_0.Placemark;

/**
 * Generates KML line strings for DrawableLine.
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

public class KmlLinesGenerator extends KmlFeatureGenerator {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlLinesGenerator.class);

    private final DrawableLine[] lines;

    public KmlLinesGenerator(DrawableLine[] lines) {
        super();
        this.lines = lines;
    }

    @Override
    public void addFeature(KmlOutputManager outputManager) {
        if (lines.length > 1) {
            outputManager = outputManager.createFolder("Lines");
        }
        for (DrawableLine line : lines) {
            de.micromata.opengis.kml.v_2_2_0.Style kmlStyle = new de.micromata.opengis.kml.v_2_2_0.Style();
            kmlStyle.createAndSetLabelStyle().setScale(0);
            kmlStyle.createAndSetIconStyle().setScale(0);
            kmlStyle.createAndSetLineStyle()
                    .withWidth(line.width)
                    .withColor(toColorStr(line.basics.alpha, line.basics.color));
            Placemark placemark = new Placemark();
            placemark.setName("Line");
            placemark.setStyleUrl(outputManager.getStyleUrl(kmlStyle));
            try {
                placemark.createAndSetLineString().withCoordinates(
                        transformToLatLon(line.points));
                outputManager.addFeature(placemark);
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

}
