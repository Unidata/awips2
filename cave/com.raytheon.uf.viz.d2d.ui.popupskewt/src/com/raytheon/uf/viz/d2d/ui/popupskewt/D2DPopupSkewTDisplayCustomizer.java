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
package com.raytheon.uf.viz.d2d.ui.popupskewt;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.viz.cloudheight.rsc.CloudHeightResource;
import com.raytheon.uf.viz.cloudheight.rsc.CloudHeightResourceData;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.IInitListener;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.npp.viirs.rsc.VIIRSResourceData;
import com.raytheon.uf.viz.ui.popupskewt.rsc.PopupSkewTResource;
import com.raytheon.uf.viz.ui.popupskewt.rsc.PopupSkewTResourceData;
import com.raytheon.viz.satellite.rsc.SatBlendedResourceData;
import com.raytheon.viz.satellite.rsc.SatResourceData;
import com.raytheon.viz.ui.perspectives.IRenderableDisplayCustomizer;

/**
 * Customizes the {@link IRenderableDisplay} by adding the
 * {@link CloudHeightResource} and {@link PopupSkewTResource} as system
 * resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 31, 2013  2190     mschenke     Initial creation
 * Mar 20, 2014  2932     bsteffen     Better support of blended resources.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class D2DPopupSkewTDisplayCustomizer implements
        IRenderableDisplayCustomizer {

    private static final String RESOURCE_NAME = "Radar Popup SkewT";

    private static final String CONTEXT_MENU_NAME = "Sample Cloud Heights/Radar Skew T";

    private static Set<Class<?>> COMPATIBLE_CLASSES = new HashSet<Class<?>>();
    static {
        COMPATIBLE_CLASSES.add(SatResourceData.class);
        COMPATIBLE_CLASSES.add(SatBlendedResourceData.class);
        COMPATIBLE_CLASSES.add(VIIRSResourceData.class);
    }

    /** List of listeners we have for renderable displays */
    private List<D2DPopupSkewTResourceListener> listeners = new ArrayList<D2DPopupSkewTResourceListener>();

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.perspectives.IRenderableDisplayCustomizer#
     * customizeDisplay(com.raytheon.uf.viz.core.drawables.IRenderableDisplay)
     */
    @Override
    public synchronized void customizeDisplay(IRenderableDisplay display) {
        boolean add = true;
        for (D2DPopupSkewTResourceListener listener : listeners) {
            if (display == listener.getDisplay()) {
                add = false;
                break;
            }
        }

        if (add) {
            listeners.add(new D2DPopupSkewTResourceListener(display));
        }
    }

    @Override
    public void uncustomizeDisplay(IRenderableDisplay display) {
        D2DPopupSkewTResourceListener toRemove = null;
        for (D2DPopupSkewTResourceListener listener : listeners) {
            if (listener.getDisplay() == display) {
                toRemove = listener;
                break;
            }
        }
        if (toRemove != null) {
            toRemove.dispose();
            listeners.remove(toRemove);
        }
    }

    private static class D2DPopupSkewTResourceListener implements AddListener,
            RemoveListener, IInitListener {

        private IRenderableDisplay display;

        private boolean resourcesAdded = false;

        private ResourcePair popupSkewTResource;

        private ResourcePair cloudHeightResource;

        public D2DPopupSkewTResourceListener(IRenderableDisplay display) {
            this.display = display;
            IDescriptor descriptor = display.getDescriptor();
            ResourceList list = descriptor.getResourceList();
            if (hasCompatibleResource(list)) {
                addResources(descriptor);
            }
            list.addPostAddListener(this);
            list.addPostRemoveListener(this);
        }

        public void dispose() {
            ResourceList list = display.getDescriptor().getResourceList();
            list.removePostAddListener(this);
            list.removePostRemoveListener(this);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener#notifyRemove
         * (com.raytheon.uf.viz.core.drawables.ResourcePair)
         */
        @Override
        public synchronized void notifyRemove(ResourcePair rp)
                throws VizException {
            if (resourcesAdded) {
                if (rp.getResource() != null) {
                    rp.getResource().unregisterListener(this);
                }
                if (isCompatibleResource(rp)) {
                    IDescriptor descriptor = display.getDescriptor();
                    if (hasCompatibleResource(descriptor.getResourceList()) == false) {
                        removeResources(descriptor);
                    }
                }
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.core.rsc.ResourceList.AddListener#notifyAdd(com
         * .raytheon .uf.viz.core.drawables.ResourcePair)
         */
        @Override
        public synchronized void notifyAdd(ResourcePair rp) throws VizException {
            if (!resourcesAdded) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null) {
                    rsc.registerListener(this);
                    if (rsc.getStatus() == ResourceStatus.INITIALIZED
                            && isCompatibleResource(rp)) {
                        addResources(display.getDescriptor());
                    }
                }
            }
        }

        private boolean hasCompatibleResource(ResourceList list) {
            for (ResourcePair rp : list) {
                if (isCompatibleResource(rp)) {
                    return true;
                }
            }
            return false;
        }

        private boolean isCompatibleResource(ResourcePair rp) {
            AbstractResourceData resourceData = rp.getResourceData();
            if (resourceData != null) {
                if (COMPATIBLE_CLASSES
                        .contains(rp.getResourceData().getClass())) {
                    return CloudHeightResource.isValidContributor(rp
                            .getResource());
                } else if (resourceData instanceof IResourceGroup) {
                    IResourceGroup group = (IResourceGroup) resourceData;
                    for (ResourcePair internalPair : group.getResourceList()) {
                        if (isCompatibleResource(internalPair)) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        private synchronized void addResources(IDescriptor descriptor) {
            if (!resourcesAdded) {
                ResourceList list = descriptor.getResourceList();
                cloudHeightResource = constructCloudHeightResource();
                list.add(cloudHeightResource);

                popupSkewTResource = constructPopupSkewTResource();
                list.add(popupSkewTResource);

                list.instantiateResources(descriptor, true);
                resourcesAdded = true;
            }
        }

        private synchronized void removeResources(IDescriptor descriptor) {
            if (resourcesAdded) {
                if (cloudHeightResource != null) {
                    descriptor.getResourceList().remove(cloudHeightResource);
                    cloudHeightResource = null;
                }
                if (popupSkewTResource != null) {
                    descriptor.getResourceList().remove(popupSkewTResource);
                    popupSkewTResource = null;
                }
                resourcesAdded = false;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.core.rsc.IInitListener#inited(com.raytheon.uf
         * .viz.core.rsc.AbstractVizResource)
         */
        @Override
        public synchronized void inited(AbstractVizResource<?, ?> rsc) {
            if (!resourcesAdded && CloudHeightResource.isValidContributor(rsc)) {
                addResources(rsc.getDescriptor());
            }
        }

        public IRenderableDisplay getDisplay() {
            return display;
        }

    }

    private static ResourcePair constructCloudHeightResource() {
        return ResourcePair
                .constructSystemResourcePair(new CloudHeightResourceData());
    }

    private static ResourcePair constructPopupSkewTResource() {
        PopupSkewTResourceData prd = new PopupSkewTResourceData();
        prd.setContextMenuName(CONTEXT_MENU_NAME);
        prd.setResourceName(RESOURCE_NAME);
        prd.setSystem(true);
        return ResourcePair.constructSystemResourcePair(prd);
    }
}
