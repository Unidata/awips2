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
package com.raytheon.viz.core.contours.cmenu;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.viz.core.contours.ILoadableAsArrows;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 6, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ConvertToArrows extends AbstractRightClickAction {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        System.out.println("Converting image to Arrows");
        try {
            IGraphicsTarget activeTarget = container.getActiveDisplayPane()
                    .getTarget();

            AbstractVizResource<?, ?> arrowRsc = ((ILoadableAsArrows) this
                    .getSelectedRsc()).getArrowResource();
            this.getDescriptor().getResourceList().add(arrowRsc);
        } catch (VizException e) {
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "Error converting to imagery",
                    "Error converting contour resource to imagery resource",
                    new Status(Status.ERROR,
                            com.raytheon.viz.core.contours.Activator.PLUGIN_ID,
                            "Error creating imagery resource", e));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Load as Arrows";
    }

    @Override
    public boolean isHidden() {
        if (getSelectedRsc() instanceof ILoadableAsArrows) {
            return !((ILoadableAsArrows) getSelectedRsc()).isArrowVector();
        }
        return false;
    }

    @Override
    public boolean isEnabled() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        AbstractResourceData rrd = rsc.getResourceData();
        if (rsc != null && rrd != null) {
            for (ResourcePair rp : rsc.getDescriptor().getResourceList()) {
                AbstractVizResource<?, ?> rsc2 = rp.getResource();
                if (rsc2 != null
                        && rsc2 != rsc
                        && rrd.equals(rsc2.getResourceData()) == true
                        && rsc2.getCapability(DisplayTypeCapability.class)
                                .getDisplayType() == DisplayType.ARROW) {
                    return false;
                }
            }
            return true;
        } else {
            return false;
        }
    }
}
