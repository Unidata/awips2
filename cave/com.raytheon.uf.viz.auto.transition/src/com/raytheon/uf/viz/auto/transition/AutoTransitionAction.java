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
package com.raytheon.uf.viz.auto.transition;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.ProcedureXmlManager;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceGroup;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogatable;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.viz.core.rsc.BlendedResource;
import com.raytheon.viz.core.rsc.BlendedResourceData;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * 
 * Action which converts a {@link BlendedResource} into a
 * {@link AutoTransitionResource}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 09, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AutoTransitionAction extends AbstractRightClickAction {

    private static final transient IUFStatusHandler logger = UFStatus
            .getHandler(AutoTransitionAction.class);

    public AutoTransitionAction() {
        super(AS_CHECK_BOX);
    }

    @Override
    public boolean isHidden() {
        BlendedResource resource = (BlendedResource) selectedRsc.getResource();
        for (ResourcePair pair : resource.getResourceList()) {
            if (pair.getResource() instanceof Interrogatable) {
                return false;
            }
        }
        return true;
    }

    private AutoTransitionResourceData getResourceData() {
        if (selectedRsc.getResource() instanceof AutoTransitionResource) {
            return ((AutoTransitionResource) selectedRsc.getResource())
                    .getResourceData();
        }
        return null;
    }

    @Override
    public boolean isChecked() {
        AutoTransitionResourceData data = getResourceData();
        if (data != null) {
            return data.isAutomaticSelection();
        }
        return false;
    }


    @Override
    public String getText() {
        return "Automatic Transition";
    }

    @Override
    public void run() {
        AutoTransitionResourceData data = getResourceData();
        if (data != null) {
            data.setAutomaticSelection(!data.isAutomaticSelection());
            return;
        }
        ProcedureXmlManager jaxb = ProcedureXmlManager.getInstance();
        // Put the base resource in a group so it can be serialized
        ResourceGroup group = new ResourceGroup();
        group.getResourceList().add(selectedRsc);
        try {
            String baseXml = jaxb.marshal(group);
            group = jaxb.unmarshal(ResourceGroup.class, baseXml);
        } catch (SerializationException e) {
            logger.error("Unable to initialize Automatic Transition.", e);
        }
        ResourcePair pair = group.getResourceList().get(0);

        AbstractVizResource<?, ?> oldResource = selectedRsc.getResource();
        AutoTransitionResourceData resourceData = new AutoTransitionResourceData(
                (BlendedResourceData) pair.getResourceData());

        selectedRsc.setResourceData(resourceData);
        selectedRsc.setLoadProperties(pair.getLoadProperties());
        selectedRsc.setResource(null);
        try {
            selectedRsc.instantiateResource(oldResource.getDescriptor());
        } catch (VizException e) {
            logger.error("Unable to initialize Automatic Transition.", e);
            selectedRsc.setResource(oldResource);
            return;
        }
        oldResource.dispose();
        TimeMatchingJob.scheduleTimeMatch(oldResource.getDescriptor());
    }


}
