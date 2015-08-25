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
package com.raytheon.uf.viz.daylight.transition;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.ProcedureXmlManager;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceGroup;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.uf.viz.daylight.transition.resource.DaylightTransitionBlendedResource;
import com.raytheon.uf.viz.daylight.transition.resource.DaylightTransitionBlendedResourceData;
import com.raytheon.viz.core.rsc.BlendedResource;
import com.raytheon.viz.core.rsc.BlendedResourceData;
import com.raytheon.viz.satellite.rsc.SatBlendedResource;
import com.raytheon.viz.satellite.rsc.SatResource;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Action which converts a {@link BlendedResource} into a
 * {@link DaylightTransitionBlendedResource}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 28, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DaylightTransitionAction extends AbstractRightClickAction {

    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(DaylightTransitionAction.class);

    public DaylightTransitionAction() {
        super(AS_CHECK_BOX);
    }

    @Override
    public boolean isHidden() {
        BlendedResource resource = (BlendedResource) selectedRsc.getResource();
        for (ResourcePair pair : resource.getResourceList()) {
            if (!(pair.getResource() instanceof SatResource)
                    && !(pair.getResource() instanceof SatBlendedResource)) {
                return true;
            }
        }
        return false;
    }

    private DaylightTransitionBlendedResourceData getResourceData() {
        if (selectedRsc.getResource() instanceof DaylightTransitionBlendedResource) {
            return ((DaylightTransitionBlendedResource) selectedRsc
                    .getResource()).getResourceData();
        }
        return null;
    }

    @Override
    public boolean isChecked() {
        DaylightTransitionBlendedResourceData data = getResourceData();
        if (data != null) {
            return true;
        }
        return false;
    }

    @Override
    public String getText() {
        return "Daylight Transition";
    }

    @Override
    public void run() {
        ProcedureXmlManager jaxb = ProcedureXmlManager.getInstance();
        /* Use jaxb serialization to safely make a clone of the resource. */
        ResourceGroup group = new ResourceGroup();
        group.getResourceList().add(selectedRsc);
        try {
            String baseXml = jaxb.marshal(group);
            group = jaxb.unmarshal(ResourceGroup.class, baseXml);
        } catch (SerializationException e) {
            logger.error("Unable to initialize Daylight Transition.", e);
        }
        ResourcePair pair = group.getResourceList().get(0);

        AbstractVizResource<?, ?> oldResource = selectedRsc.getResource();
        if (oldResource instanceof DaylightTransitionBlendedResource) {
            BlendedResourceData resourceData = ((DaylightTransitionBlendedResourceData) pair
                    .getResourceData()).toBlended();
            selectedRsc.setResourceData(resourceData);
        } else {
            BlendedResourceData resourceData = new DaylightTransitionBlendedResourceData(
                    (BlendedResourceData) pair.getResourceData());
            selectedRsc.setResourceData(resourceData);
        }
        selectedRsc.setLoadProperties(pair.getLoadProperties());
        selectedRsc.setResource(null);
        try {
            selectedRsc.instantiateResource(oldResource.getDescriptor());
        } catch (VizException e) {
            logger.error("Unable to initialize Daylight Transition.", e);
            selectedRsc.setResource(oldResource);
            return;
        }
        oldResource.dispose();
        TimeMatchingJob.scheduleTimeMatch(oldResource.getDescriptor());
        return;
    }

}
