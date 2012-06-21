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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.status.StatusConstants;
import com.raytheon.viz.core.contours.ILoadableAsImage;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * ConvertToImagery
 * 
 * Action to create an imagery resource from a contoru resource
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Oct 24, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ConvertToImagery extends AbstractRightClickAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ConvertToImagery.class);

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        try {
            IGraphicsTarget activeTarget = container.getActiveDisplayPane()
                    .getTarget();
            ILoadableAsImage rsc = ((ILoadableAsImage) this.getSelectedRsc());

            AbstractVizResource<?, ?> imageryRsc = rsc.getImageryResource();

            ResourceProperties rp = this.getDescriptor().getResourceList()
                    .getProperties(imageryRsc);
            imageryRsc.getCapability(ImagingCapability.class).setBrightness(
                    0.5f);
            this.getDescriptor().getResourceList().add(imageryRsc);
        } catch (VizException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error converting contour resource to imagery resource", e);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText() {
        return "Load as Image";
    }

    @Override
    public boolean isHidden() {
        if (getSelectedRsc() instanceof ILoadableAsImage) {
            return !((ILoadableAsImage) getSelectedRsc()).isLoadableAsImage();
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
                                .getDisplayType() == DisplayType.IMAGE) {
                    return false;
                }
            }
            return true;
        } else {
            return false;
        }
    }

}
