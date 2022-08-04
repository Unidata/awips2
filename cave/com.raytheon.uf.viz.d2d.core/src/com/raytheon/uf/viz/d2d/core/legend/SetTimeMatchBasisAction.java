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
package com.raytheon.uf.viz.d2d.core.legend;

import org.eclipse.jface.action.IAction;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Time match basis action, sets the selected resource as the time match basis
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 28, 2010           mschenke  Initial creation
 * Apr 16, 2018  7261     bsteffen  Allow selection of individual blended
 *                                  resources.
 * Jan 30, 2019  7652     bsteffen  Allow selection of group resources that
 *                                  contain resources to be time matched.
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class SetTimeMatchBasisAction extends AbstractRightClickAction {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SetTimeMatchBasisAction.class);

    public SetTimeMatchBasisAction() {
        super("Time Match Basis", IAction.AS_CHECK_BOX);
    }

    @Override
    public AbstractVizResource<?, ?> getSelectedRsc() {
        AbstractVizResource<?, ?> rsc = super.getSelectedRsc();
        if (rsc != null && rsc.isTimeAgnostic()
                && rsc instanceof IResourceGroup) {
            ResourceList list = ((IResourceGroup) rsc).getResourceList();
            for (ResourcePair rp : list) {
                AbstractVizResource<?, ?> testRsc = rp.getResource();
                if (testRsc != null && !testRsc.isTimeAgnostic()) {
                    rsc = testRsc;
                    break;
                }
            }
        }
        return rsc;
    }

    @Override
    public void run() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        if (rsc != null) {
            try {
                IDescriptor descriptor = rsc.getDescriptor();
                D2DTimeMatcher tm = (D2DTimeMatcher) descriptor
                        .getTimeMatcher();
                tm.changeTimeMatchBasis(rsc);
                tm.redoTimeMatching(descriptor);

                for (IDisplayPane pane : container.getDisplayPanes()) {
                    if (pane.getDescriptor() != descriptor) {
                        pane.getDescriptor().getTimeMatcher()
                                .redoTimeMatching(pane.getDescriptor());
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error redoing time matching", e);
            }
        }
    }

    @Override
    public boolean isEnabled() {
        return !isChecked();
    }

    @Override
    public boolean isChecked() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        AbstractVizResource<?, ?> basis = ((D2DTimeMatcher) rsc.getDescriptor()
                .getTimeMatcher()).getTimeMatchBasis();

        return basis == rsc;
    }

    @Override
    public boolean isHidden() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        AbstractTimeMatcher tm = rsc.getDescriptor().getTimeMatcher();
        if (tm instanceof D2DTimeMatcher) {
            // If on D2DTimeMatcher, hide only if time agnostic resource
            return rsc.isTimeAgnostic();
        }
        return true;
    }

}
