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

package com.raytheon.uf.viz.d2d.ui.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Decrease the blending amount of a resource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Mar 11, 2007             chammack    Initial Creation.
 *  Nov  3, 2009 3457        bsteffen    Updated to change blend on all DisplayPanes, not just the active ones.
 * 
 * </pre>
 * 
 * @author cnh
 * @version 1
 */

public class DecreaseBlendAmount extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);
        // Determine if there are blended resources loaded
        boolean hasBlended = false;
        for (IDisplayPane pane : editor.getDisplayPanes()) {
            IDescriptor mapDescriptor = pane.getDescriptor();
            ResourceList rscs = mapDescriptor.getResourceList();
            for (ResourcePair rp : rscs) {
                if (rp.getResource().getCapabilities()
                        .hasCapability(BlendableCapability.class)) {
                    hasBlended = true;
                    break;
                }
            }
            if (hasBlended) {
                break;
            }
        }

        int blendAmtToUse = -1;

        for (IDisplayPane pane : editor.getDisplayPanes()) {
            IDescriptor mapDescriptor = pane.getDescriptor();
            ResourceList rscs = mapDescriptor.getResourceList();
            for (ResourcePair rp : rscs) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (hasBlended) {
                    if (!rsc.getCapabilities().hasCapability(
                            BlendableCapability.class))
                        continue;
                    int blendAmt = blendAmtToUse;
                    if (blendAmtToUse == -1) {
                        blendAmtToUse = blendAmt = rsc.getCapability(
                                BlendableCapability.class).getAlphaStep();
                    }

                    if (blendAmt > 0) {
                        blendAmt--;
                    }
                    rsc.getCapability(BlendableCapability.class).setAlphaStep(
                            blendAmt);
                    rsc.getCapability(ImagingCapability.class).setAlpha(
                            (float) blendAmt
                                    / (float) BlendableCapability.BLEND_MAX);
                } else {
                    if (!rsc.hasCapability(ImagingCapability.class)) {
                        continue;
                    }

                    ImagingCapability cap = rsc
                            .getCapability(ImagingCapability.class);

                    int blendAmt = blendAmtToUse;
                    if (blendAmtToUse == -1) {
                        blendAmtToUse = blendAmt = (int) (cap.getAlpha() * BlendableCapability.BLEND_MAX);
                    }

                    if (blendAmt > 0) {
                        blendAmt--;
                    }
                    cap.setAlpha((float) blendAmt
                            / (float) BlendableCapability.BLEND_MAX);
                }
            }
        }
        editor.refresh();

        return null;
    }

}
