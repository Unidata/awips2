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
package com.raytheon.uf.viz.d2d.ui.map.actions;

import java.util.List;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.d2d.core.sampling.ID2DSamplingResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Turns on all panel sampling for d2d
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AllPanelSampleAction extends AbstractRightClickAction {

    private boolean sampling = false;

    public AllPanelSampleAction() {
        super("All Panel Sample");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        for (IDisplayPane pane : container.getDisplayPanes()) {
            List<ID2DSamplingResource> rscs = pane.getDescriptor()
                    .getResourceList()
                    .getResourcesByTypeAsType(ID2DSamplingResource.class);
            for (ID2DSamplingResource rsc : rscs) {
                rsc.setAllPanelSampling(!sampling);
            }
        }
        container.refresh();
    }

    @Override
    public void setContainer(IDisplayPaneContainer container) {
        super.setContainer(container);
        sampling = false;
        if (container != null) {
            IDisplayPane activePane = container.getActiveDisplayPane();
            if (activePane != null) {
                List<ID2DSamplingResource> rscs = activePane.getDescriptor()
                        .getResourceList()
                        .getResourcesByTypeAsType(ID2DSamplingResource.class);
                for (ID2DSamplingResource rsc : rscs) {
                    sampling |= rsc.isAllPanelSampling();
                    if (sampling) {
                        break;
                    }
                }
            }
        }
        setChecked(sampling);
    }

}
