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
package com.raytheon.uf.viz.kml.export.graphics.ext.radar;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.projection.RadarProjectionFactory;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsTarget;
import com.raytheon.uf.viz.kml.export.graphics.ext.KmlMesh;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Creates a KMLMesh by constructing a RadialBinCRS based CrigGeometry object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 6, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlRadialMeshExtension extends
        GraphicsExtension<KmlGraphicsTarget> implements IRadialMeshExtension {

    @Override
    public int getCompatibilityValue(KmlGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

    @Override
    public IMesh constructMesh(RadarRecord record,
            GeneralGridGeometry targetGeometry) throws VizException {
        GridGeometry2D imageGeometry;
        try {
            imageGeometry = RadarProjectionFactory
                    .constructGridGeometry(new Coordinate(
                            record.getLongitude(), record.getLatitude()),
                            record.getAngleData(), record.getGateResolution(),
                            record.getTrueElevationAngle(),
                            record.getNumBins(), true);
        } catch (FactoryException e) {
            throw new VizException(e);
        }
        return new KmlMesh(imageGeometry);
    }

}
