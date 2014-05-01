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
package com.raytheon.viz.core.imagery;

import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.core.rsc.BlendedResourceData;

/**
 * Class thats blends image resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 22, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ImageCombiner implements AddListener {

    public static interface IImageCombinerListener {
        public void preferenceChanged(boolean newPref);
    }

    private IDescriptor descriptor;

    private AbstractVizResource<?, ?> lastCreatedResource = null;

    private static boolean combineImages = false;

    private static Set<IImageCombinerListener> listeners = new HashSet<IImageCombinerListener>();

    public ImageCombiner(IDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.ResourceList.AddListener#notifyAdd(com.raytheon
     * .uf.viz.core.drawables.ResourcePair)
     */

    @Override
    @SuppressWarnings("unchecked")
    public void notifyAdd(ResourcePair rp) throws VizException {
        // Check to see if resource is image. If image, check to see if we
        // already have an image resource loaded. If we do, check to see if that
        // resource is a blended image resource. If blended, swap out second
        // resource for new resource. If not blended, create blended resource,
        // add existing resource, then add newly added resource. If the image
        // coming in is a blended image, then replace then don't do any
        // combining at all. This only happens in map editors!
        if (combineImages) {
            AbstractVizResource rsc = rp.getResource();
            if (rsc != null && rsc != lastCreatedResource
                    && rsc.hasCapability(ImagingCapability.class)
                    && rsc.hasCapability(BlendableCapability.class) == false) {
                // We have a single image resource added, check to see if we
                // have another image resource
                AbstractVizResource combineWithResource = null;
                ResourcePair combineWithPair = null;
                for (ResourcePair pair : descriptor.getResourceList()) {
                    AbstractVizResource rsc2 = pair.getResource();
                    if (rsc2 != null
                            && rsc2 != rsc
                            && (rsc2.hasCapability(ImagingCapability.class) || rsc2
                                    .hasCapability(BlendableCapability.class))) {
                        combineWithResource = rsc2;
                        combineWithPair = pair;
                        break;
                    }
                }

                if (combineWithResource != null) {
                    // There is an resource to combine with, do some combining
                    if (combineWithResource
                            .hasCapability(BlendableCapability.class)) {
                        // Set to null before removing so resource doesn't get
                        // disposed. Remove just added resource from list
                        rp.setResource(null);
                        descriptor.getResourceList().remove(rp);
                        rp.setResource(rsc);

                        // Replace last resource with this one if size > 1
                        ResourceList rl = ((AbstractVizResource<?, ?>) combineWithResource)
                                .getCapability(BlendableCapability.class)
                                .getResourceList();
                        if (rl.size() > 1) {
                            ResourcePair rscPair = rl.get(rl.size() - 1);
                            rl.remove(rscPair);
                        }

                        ((AbstractVizResource<?, ?>) rsc).getCapability(
                                BlendedCapability.class).setBlendableResource(
                                combineWithPair);
                        rl.add(rp);
                    } else {
                        // Single image resource we are combining with:

                        // Set resource to null so they don't get disposed
                        rp.setResource(null);
                        combineWithPair.setResource(null);
                        descriptor.getResourceList().remove(rp);
                        descriptor.getResourceList().remove(combineWithPair);

                        // Reset the resources
                        combineWithPair.setResource(combineWithResource);
                        rp.setResource(rsc);

                        // create a blended resource
                        BlendedResourceData brd = new BlendedResourceData();
                        brd.getResourceList().add(combineWithPair);
                        brd.getResourceList().add(rp);

                        // Create the blended resource, set color same as
                        // existing resource
                        LoadProperties loadProps = new LoadProperties();
                        loadProps
                                .getCapabilities()
                                .addCapability(
                                        combineWithResource
                                                .getCapability(ColorableCapability.class));
                        rsc.getCapabilities().addCapability(
                                combineWithResource.getCapability(
                                        ColorableCapability.class).clone());

                        ResourcePair resourcePair = new ResourcePair();
                        resourcePair.setResourceData(brd);
                        resourcePair.setLoadProperties(loadProps);

                        // so we don't repeat this code when notifyAdd is called
                        // again
                        descriptor.getResourceList().add(resourcePair);
                        descriptor.getResourceList().instantiateResources(
                                descriptor, true);
                        this.lastCreatedResource = resourcePair.getResource();
                    }
                }
                // No other image resources, add normally
                // Next image will be combined with this resource
            }
            // if blended already or not image, do nothing just add it
        }
        // don't do anything if combineImages is not set
    }

    public static void setCombineImages(boolean combine) {
        combineImages = combine;
        for (IImageCombinerListener listener : listeners) {
            listener.preferenceChanged(combineImages);
        }
    }

    public static boolean isCombineImages() {
        return combineImages;
    }

    public static void addListener(IImageCombinerListener listener) {
        listeners.add(listener);
    }

    public static void removeListener(IImageCombinerListener listener) {
        listeners.remove(listener);
    }
}
