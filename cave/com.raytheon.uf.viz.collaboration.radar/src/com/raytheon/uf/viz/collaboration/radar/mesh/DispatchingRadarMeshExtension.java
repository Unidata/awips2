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
package com.raytheon.uf.viz.collaboration.radar.mesh;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;
import com.raytheon.uf.viz.remote.graphics.events.RemoteGraphicsEventFactory;
import com.raytheon.uf.viz.remote.graphics.objects.DispatchingMesh;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension;

/**
 * Radar mesh extension implementation which creates dispatching mesh that
 * forwards key events
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 13, 2012           mschenke    Initial creation
 * Jun 24, 2014  3072     bsteffen    Remove RadarRecord dependency for Radial
 *                                    Mesh
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingRadarMeshExtension extends
        GraphicsExtension<DispatchGraphicsTarget> implements
        IRadialMeshExtension {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.radar.rsc.image.IRadialMeshExtension#constructMesh(com
     * .raytheon.viz.radar.rsc.image.IRadialMeshExtension.RadialMeshData,
     * org.geotools.coverage.grid.GeneralGridGeometry)
     */
    @Override
    public IMesh constructMesh(RadialMeshData meshData,
            GeneralGridGeometry targetGeometry) throws VizException {
        DispatchingMesh wrapping = new DispatchingMesh(target
                .getWrappedObject().getExtension(IRadialMeshExtension.class)
                .constructMesh(meshData, targetGeometry),
                target.getDispatcher());
        CreateRadarRadialMesh create = RemoteGraphicsEventFactory.createEvent(
                CreateRadarRadialMesh.class, wrapping);
        create.setMeshData(meshData);
        create.setTargetGeometry(targetGeometry);
        target.dispatch(create);
        return wrapping;
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
