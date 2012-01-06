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

package com.raytheon.viz.awipstools.ui.action;

import java.util.ArrayList;
import java.util.Collection;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.awipstools.capabilities.RangeRingsOverlayCapability;
import com.raytheon.viz.awipstools.capabilityInterfaces.IRangeableResource;
import com.raytheon.viz.core.rsc.BestResResource;
import com.raytheon.viz.core.rsc.BlendedResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * Tool action for a range rings overlay with ring elevation and radius.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  12-03-09     #3698      bgonzale    Range rings overlay with labels 
 *                                      for radius in km and miles and
 *                                      labels for elevation in ftMSL and
 *                                      kmAgl.
 * 
 * </pre>
 * 
 * @author bgonzale
 */
public class RangeRingsOverlayRightClickAction extends AbstractRightClickAction {

    /**
     * 
     */
    public RangeRingsOverlayRightClickAction() {
        super("Range Rings");
        setChecked(true);
    }

    @Override
    public boolean isChecked() {
        Collection<AbstractVizResource<AbstractResourceData, IDescriptor>> resources = getRangeableResources(this.getTopMostSelectedResource());

        for (AbstractVizResource<AbstractResourceData, IDescriptor> rangeableResource : resources) {
            RangeRingsOverlayCapability overlayCapability = (RangeRingsOverlayCapability) rangeableResource
                    .getCapability(RangeRingsOverlayCapability.class);

            /*
             * For now, Since the resources in a blended resource are not
             * separately selectable only check the first. When we have
             * separately selectable resources in a blended resource, then this
             * check may need to change.
             */
            return overlayCapability.isOverlayLayerPainted();
        }

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        try {
            ColorableCapability colorableCapability = this.getSelectedRsc()
                    .getCapability(ColorableCapability.class);
            OutlineCapability outlineCapability = this.getSelectedRsc()
                    .getCapability(OutlineCapability.class);
            RangeRingsOverlayCapability overlayCapability = null;
            Collection<AbstractVizResource<AbstractResourceData, IDescriptor>> resources = getRangeableResources(this.getTopMostSelectedResource());

            for (AbstractVizResource<AbstractResourceData, IDescriptor> rangeableResource : resources) {
                RangeRingsOverlayCapability oc = (RangeRingsOverlayCapability) rangeableResource
                        .getCapability(RangeRingsOverlayCapability.class);
                /*
                 * For resources in a blended resource share the
                 * overlay capability and only update the first resource.
                 */
                if (overlayCapability == null) {
                    overlayCapability = oc;
                    overlayCapability.updatePaintStatus(rangeableResource,
                            colorableCapability, outlineCapability);
                } else {
                    if (oc != overlayCapability) {
                        rangeableResource.getCapabilities().addCapability(
                                overlayCapability);
                    }
                }
            }
        } catch (VizException e) {
            throw new RuntimeException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private static Collection<AbstractVizResource<AbstractResourceData, IDescriptor>> getRangeableResources(AbstractVizResource<?, ?> rsc) {
        Collection<AbstractVizResource<AbstractResourceData, IDescriptor>> resources = new ArrayList<AbstractVizResource<AbstractResourceData, IDescriptor>>();
        
        if (rsc instanceof IRangeableResource) {
            resources
                    .add((AbstractVizResource<AbstractResourceData, IDescriptor>) rsc);
        } else if (rsc instanceof BlendedResource) {
            ResourceList rl = ((BlendedResource) rsc)
                    .getResourceList();

            for (int i = 0; i < rl.size(); ++i) {
                ResourcePair rp = rl.get(i);
                resources.addAll(getRangeableResources(rp.getResource()));
            }
        } else if (rsc instanceof BestResResource) {
            ResourceList rl = ((BestResResource) rsc)
                    .getResourceList();
            for (int i = 0; i < rl.size(); ++i) {
                ResourcePair rp = rl.get(i);
                resources.addAll(getRangeableResources(rp.getResource()));
            }
        }
        return resources;
    }
}
