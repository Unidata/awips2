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
package com.raytheon.uf.viz.collaboration.radar.rendering;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.display.Activator;
import com.raytheon.uf.viz.collaboration.display.rsc.rendering.CollaborationRenderingHandler;
import com.raytheon.uf.viz.collaboration.radar.mesh.CreateRadarRadialMesh;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.radar.rsc.image.IRadialMeshExtension;

/**
 * Rendering handler for radar graphics extension objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 17, 2012           mschenke    Initial creation
 * Jun 24, 2014  3072     bsteffen    Remove RadarRecord dependency for Radial
 *                                    Mesh
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RadarGraphicsExtRenderingHandler extends
        CollaborationRenderingHandler {

    @Subscribe
    public void createRadarMesh(CreateRadarRadialMesh event) {
        int meshId = event.getObjectId();
        IGraphicsTarget target = getGraphicsTarget();
        try {
            dataManager.putRenderableObject(
                    meshId,
                    target.getExtension(IRadialMeshExtension.class)
                            .constructMesh(event.getMeshData(),
                                    event.getTargetGeometry()));
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }
}
