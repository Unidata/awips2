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
package com.raytheon.uf.viz.collaboration.display.rsc;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;

/**
 * Listener that manages a system resource and readds it when it is removed
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SelfAddingSystemResourceListener implements RemoveListener {

    protected IDescriptor descriptor;

    private ResourcePair resource;

    public SelfAddingSystemResourceListener(AbstractResourceData resourceData,
            IDescriptor descriptor) throws VizException {
        this.descriptor = descriptor;
        addResource(resourceData);
        descriptor.getResourceList().addPostRemoveListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener#notifyRemove
     * (com.raytheon.uf.viz.core.drawables.ResourcePair)
     */
    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        if (rp == resource) {
            addResource(rp.getResourceData());
        }
    }

    /**
     * 
     */
    private void addResource(AbstractResourceData resourceData)
            throws VizException {
        resource = ResourcePair.constructSystemResourcePair(resourceData);
        resource.instantiateResource(descriptor, true);
        descriptor.getResourceList().add(resource);
    }

    public void dispose() {
        ResourceList resourceList = descriptor.getResourceList();
        resourceList.removePostRemoveListener(this);
        resourceList.remove(resource);
    }

}
