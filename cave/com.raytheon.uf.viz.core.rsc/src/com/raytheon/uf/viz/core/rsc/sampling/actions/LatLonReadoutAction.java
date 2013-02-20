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
package com.raytheon.uf.viz.core.rsc.sampling.actions;

import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.sampling.LatLonReadoutResource;
import com.raytheon.uf.viz.core.sampling.ISamplingResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Enable or Disable Lat/Lon display on an editor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2007             chammack    Initial Creation.
 * Jan 28, 2013   14465    snaples     Updated run() method to set sampling false when disabling readout.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class LatLonReadoutAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    private final String actionText;

    private boolean sampled = false;

    private boolean hasLatLonReadout = false;

    public LatLonReadoutAction() {
        this.actionText = "Lat/Lon Readout";
    }

    public LatLonReadoutAction(String actionText) {
        this.actionText = actionText;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.actions.GenericAction#run()
     */
    @Override
    public void run() {
        if (hasLatLonReadout) {
            // remove resource
            for (IDisplayPane pane : container.getDisplayPanes()) {
                List<LatLonReadoutResource> rscs = pane.getDescriptor()
                        .getResourceList()
                        .getResourcesByTypeAsType(LatLonReadoutResource.class);
                for (LatLonReadoutResource rsc : rscs) {
                    pane.getDescriptor().getResourceList().removeRsc(rsc);
                }
                List<ISamplingResource> samplers = pane.getDescriptor()
                .getResourceList()
                .getResourcesByTypeAsType(ISamplingResource.class);
                for (ISamplingResource sampler : samplers) {
                    if (sampled) {
                        break;
                    } else {
                        sampler.setSampling(false);
                    }
                }
            }
        } else {
            // add resource
            sampled = false;
            for (IDisplayPane pane : container.getDisplayPanes()) {
                pane.getDescriptor()
                        .getResourceList()
                        .add(ResourcePair
                                .constructSystemResourcePair(new GenericResourceData(
                                        LatLonReadoutResource.class)));
                pane.getDescriptor().getResourceList()
                        .instantiateResources(pane.getDescriptor(), true);
                List<ISamplingResource> samplers = pane.getDescriptor()
                        .getResourceList()
                        .getResourcesByTypeAsType(ISamplingResource.class);
                for (ISamplingResource sampler : samplers) {
                    if (sampler.isSampling()) {
                        sampled = true;
                        break;
                    } else {
                        sampler.setSampling(true);
                    }
                }
            }
        }
        container.refresh();
    }

    @Override
    public void setContainer(IDisplayPaneContainer container) {
        super.setContainer(container);
        if (container != null) {
            IDisplayPane activePane = container.getActiveDisplayPane();
            if (activePane != null) {
                List<LatLonReadoutResource> rscs = activePane.getDescriptor()
                        .getResourceList()
                        .getResourcesByTypeAsType(LatLonReadoutResource.class);
                hasLatLonReadout = rscs.size() > 0;
            }
        }
        setChecked(hasLatLonReadout);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return actionText;
    }
}
