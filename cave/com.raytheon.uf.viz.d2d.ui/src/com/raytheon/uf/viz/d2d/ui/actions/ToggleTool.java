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
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * 
 * Action that toggles the first available resource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Feb 23, 2007             chammack    Initial Creation.
 *  Nov  3, 2009 3457        bsteffen    Updated to change blend on all DisplayPanes, not just the active ones.
 *  Aug 30, 2013 DR 16555    D. Friedman Prevent NPE.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */

public class ToggleTool extends AbstractTool {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);
        for (IDisplayPane pane : editor.getDisplayPanes()) {
            IDescriptor mapDescriptor = pane.getDescriptor();
            ResourceList rscs = mapDescriptor.getResourceList();
            for (ResourcePair rp : rscs) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null && rsc.getCapabilities().hasCapability(
                        BlendableCapability.class)) {
                    rsc.getCapability(BlendableCapability.class).toggle();
                }
            }
        }
        editor.refresh();
        return null;
    }

}
