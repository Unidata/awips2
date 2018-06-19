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
package com.raytheon.uf.viz.hpe;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.IInitListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.hpe.rsc.HpeLabelResourceData;
import com.raytheon.uf.viz.hpe.util.HpeUtils;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;
import com.raytheon.viz.ui.perspectives.IRenderableDisplayCustomizer;

/**
 * This class listens to existing resources and adds the HPE "legend" text as
 * needed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 5, 2014     3026    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class D2DHpeDisplayCustomizer implements IRenderableDisplayCustomizer {

    /** List of listeners we have for renderable displays */
    private final List<D2DHpeResourceListener> listeners = new ArrayList<D2DHpeResourceListener>();

    @Override
    public void customizeDisplay(IRenderableDisplay display) {
        boolean add = true;
        for (D2DHpeResourceListener listener : listeners) {
            if (display == listener.getDisplay()) {
                add = false;
                break;
            }
        }

        if (add) {
            listeners.add(new D2DHpeResourceListener(display));
        }
    }

    @Override
    public void uncustomizeDisplay(IRenderableDisplay display) {
        D2DHpeResourceListener toRemove = null;
        for (D2DHpeResourceListener listener : listeners) {
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

    private static class D2DHpeResourceListener implements AddListener,
            RemoveListener, IInitListener {

        private final IRenderableDisplay display;

        private boolean resourcesAdded = false;

        private ResourcePair hpeResourcePair;

        public D2DHpeResourceListener(IRenderableDisplay display) {
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
            AbstractVizResource<?, ?> resource = rp.getResource();
            if (resource != null) {
                if (resource instanceof D2DGridResource) {
                    D2DGridResource rsc = (D2DGridResource) resource;
                    return HpeUtils.isHpe(rsc.getCurrentGridRecord());
                }
            }
            return false;
        }

        private synchronized void addResources(IDescriptor descriptor) {
            if (!resourcesAdded) {
                ResourceList list = descriptor.getResourceList();
                hpeResourcePair = constructHpeLabelResource();
                list.add(hpeResourcePair);

                list.instantiateResources(descriptor, true);
                resourcesAdded = true;

                // add hpe resource as a listener
                for (ResourcePair rp : list) {
                    AbstractVizResource<?, ?> resource = rp.getResource();
                    if (resource != null) {
                        if (resource instanceof D2DGridResource) {
                            D2DGridResource rsc = (D2DGridResource) resource;
                            rsc.getResourceData().addChangeListener(
                                    (IResourceDataChanged) hpeResourcePair
                                            .getResource());
                            return;
                        }
                    }
                }
            }
        }

        private synchronized void removeResources(IDescriptor descriptor) {
            if (resourcesAdded) {
                if (hpeResourcePair != null) {
                    ResourceList list = descriptor.getResourceList();
                    list.remove(hpeResourcePair);
                    for (ResourcePair rp : list) {
                        AbstractVizResource<?, ?> resource = rp.getResource();
                        if (resource != null) {
                            if (resource instanceof D2DGridResource) {
                                D2DGridResource rsc = (D2DGridResource) resource;
                                rsc.getResourceData().removeChangeListener(
                                        (IResourceDataChanged) hpeResourcePair
                                                .getResource());
                            }
                        }
                    }

                    hpeResourcePair = null;
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
            if (rsc instanceof D2DGridResource) {
                GridRecord gridRec = ((D2DGridResource) rsc)
                        .getCurrentGridRecord();
                if (!resourcesAdded && HpeUtils.isHpe(gridRec)) {
                    addResources(rsc.getDescriptor());
                }
            }
        }

        /**
         * Get the IRenderableDisplay
         * 
         * @return the display
         */
        public IRenderableDisplay getDisplay() {
            return display;
        }
    }

    private static ResourcePair constructHpeLabelResource() {
        return ResourcePair
                .constructSystemResourcePair(new HpeLabelResourceData());
    }
}
