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
import com.raytheon.uf.viz.core.sampling.ISamplingResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Enable or Disable Sampling on an editor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class SampleAction extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    private String actionText;

    private boolean sampling = false;

    public SampleAction() {
        this("Sample");
    }

    public SampleAction(String actionText) {
        super(AS_CHECK_BOX);
        this.actionText = actionText;

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        for (IDisplayPane pane : container.getDisplayPanes()) {
            List<ISamplingResource> rscs = pane.getDescriptor()
                    .getResourceList()
                    .getResourcesByTypeAsType(ISamplingResource.class);
            for (ISamplingResource rsc : rscs) {
                rsc.setSampling(!sampling);
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
                List<ISamplingResource> rscs = activePane.getDescriptor()
                        .getResourceList()
                        .getResourcesByTypeAsType(ISamplingResource.class);
                for (ISamplingResource rsc : rscs) {
                    sampling |= rsc.isSampling();
                    if (sampling) {
                        break;
                    }
                }
            }
        }
        setChecked(sampling);
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
