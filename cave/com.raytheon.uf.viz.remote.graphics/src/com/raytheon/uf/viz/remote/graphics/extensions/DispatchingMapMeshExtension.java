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
package com.raytheon.uf.viz.remote.graphics.extensions;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.events.mesh.CreateMeshEvent;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingMesh;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingMapMeshExtension extends
        GraphicsExtension<DispatchGraphicsTarget> implements IMapMeshExtension {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.map.IMapMeshExtension#constructMesh(org.geotools
     * .coverage.grid.GridGeometry2D,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public IMesh constructMesh(GridGeometry2D imageGeometry,
            IDescriptor targetDescriptor) throws VizException {
        return constructMesh(imageGeometry, targetDescriptor.getGridGeometry());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.map.IMapMeshExtension#constructMesh(org.geotools
     * .coverage.grid.GridGeometry2D,
     * org.geotools.coverage.grid.GeneralGridGeometry)
     */
    @Override
    public IMesh constructMesh(GridGeometry2D imageGeometry,
            GeneralGridGeometry targetGeometry) throws VizException {
        IMesh targetMesh = target.getWrappedObject()
                .getExtension(IMapMeshExtension.class)
                .constructMesh(imageGeometry, targetGeometry);
        DispatchingMesh mesh = new DispatchingMesh(targetMesh,
                target.getDispatcher());

        // Send mesh creation event
        CreateMeshEvent event = RemoteGraphicsEventFactory.createEvent(
                CreateMeshEvent.class, mesh);
        event.setImageGeometry(imageGeometry);
        event.setTargetGeometry(targetGeometry);
        target.dispatch(event);

        return mesh;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension#
     * getCompatibilityValue(com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public int getCompatibilityValue(DispatchGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

}
