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
package com.raytheon.uf.viz.collaboration.ui.rsc.rendering;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.remote.graphics.events.DisposeObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.rendering.BeginFrameEvent;

/**
 * Handles general rendering events, begin frame and dispose
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GeneralRenderingHandler extends CollaborationRenderingHandler {

    /**
     * General dispose of a renderable object event
     * 
     * @param event
     */
    @Subscribe
    public void disposeRenderable(DisposeObjectEvent event) {
        dataManager.dispose(event.getObjectId());
    }

    /**
     * Begin frame event, modifies the target extent
     * 
     * @param event
     */
    @Subscribe
    public void handleBeginFrame(BeginFrameEvent event) {
        PaintProperties paintProps = getPaintProperties();
        IGraphicsTarget target = getTarget();
        double[] center = event.getExtentCenter();
        IExtent copy = paintProps.getView().getExtent().clone();
        if (center != null) {
            double[] currCenter = copy.getCenter();
            copy.shift(center[0] - currCenter[0], center[1] - currCenter[1]);
            copy.scaleAndBias(event.getExtentFactor() / copy.getScale(),
                    center[0], center[1]);
            target.updateExtent(copy);
            event.setExtentCenter(null);
            target.setNeedsRefresh(true);
        }
    }

}
