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
package com.raytheon.uf.viz.kml.export.graphics.ext;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsTarget;

/**
 * 
 * Creates meshes.
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
public class KmlMapMeshExtension extends GraphicsExtension<KmlGraphicsTarget>
        implements IMapMeshExtension {

    @Override
    public IMesh constructMesh(GridGeometry2D imageGeometry,
            GeneralGridGeometry targetGeometry) throws VizException {
        GridEnvelope2D range = imageGeometry.getGridRange2D();
        Envelope2D envelope = imageGeometry.getEnvelope2D();
        range.x = 0;
        range.y = 0;
        imageGeometry = new GridGeometry2D((GridEnvelope) range, envelope);
        return new KmlMesh(imageGeometry);
    }

    @Override
    public IMesh constructMesh(GridGeometry2D imageGeometry,
            IDescriptor targetDescriptor) throws VizException {
        return constructMesh(imageGeometry, targetDescriptor.getGridGeometry());
    }

    @Override
    public int getCompatibilityValue(KmlGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

}
