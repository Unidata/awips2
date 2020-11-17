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
package com.raytheon.uf.viz.d2d.core;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

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
import com.raytheon.uf.viz.core.rsc.groups.BlendedResource;
import com.raytheon.uf.viz.core.rsc.groups.BlendedResourceData;

/**
 * This class monitors the resources being added to a descriptor and when it is
 * active it will combine any image resources that are loaded into a single
 * {@link BlendedResource}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Dec 22, 2009  2126     mschenke  Initial creation
 * Sep 13, 2016  3241     bsteffen  Move to D2D core
 * Jun 14, 2017  6297     bsteffen  Make listeners thread safe.
 * Jan 04, 2018  6753     bsteffen  Add resources to blended group before removing.
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class ImageCombiner implements AddListener {

    public static interface IImageCombinerListener {
        public void preferenceChanged(boolean newPref);
    }

    private IDescriptor descriptor;

    private static boolean combineImages = false;

    private static Set<IImageCombinerListener> listeners = new CopyOnWriteArraySet<>();

    public ImageCombiner(IDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {
        if (!combineImages) {
            return;
        }
        /*
         * The new resource is eligible for combination if it is a non-blended
         * image.
         */
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (rsc == null || !rsc.hasCapability(ImagingCapability.class)
                || rsc.hasCapability(BlendableCapability.class)) {
            return;
        }

        /*
         * Search for another resource to combine with, either another image, or
         * an existing blended resource.
         */
        AbstractVizResource<?, ?> combineWithResource = null;
        ResourcePair combineWithPair = null;
        for (ResourcePair pair : descriptor.getResourceList()) {
            AbstractVizResource<?, ?> rsc2 = pair.getResource();
            if (rsc2 != null && rsc2 != rsc
                    && (rsc2.hasCapability(ImagingCapability.class)
                            || rsc2.hasCapability(BlendableCapability.class))) {
                combineWithResource = rsc2;
                combineWithPair = pair;
                break;
            }
        }

        if (combineWithPair == null) {
            return;
        }

        if (combineWithResource.hasCapability(BlendableCapability.class)) {
            /*
             * Update an existing blended resource, since blended resources
             * contain only two images if there is already more than one
             * resource then replace the last resource.
             */
            ResourceList rl = combineWithResource
                    .getCapability(BlendableCapability.class).getResourceList();
            if (rl.size() > 1) {
                ResourcePair rscPair = rl.get(rl.size() - 1);
                rl.remove(rscPair);
            }

            rsc.getCapability(BlendedCapability.class)
                    .setBlendableResource(combineWithPair);
            rl.add(rp);
        } else {
            /*
             * Create a new blended resource combining the two image resources
             */
            BlendedResourceData brd = new BlendedResourceData();
            brd.getResourceList().add(combineWithPair);
            brd.getResourceList().add(rp);

            /*
             * Both image resources and the blended resource holding them should
             * have the same color as the existing image resource.
             */
            LoadProperties loadProps = new LoadProperties();
            loadProps.getCapabilities().addCapability(combineWithResource
                    .getCapability(ColorableCapability.class).clone());
            rsc.getCapabilities().addCapability(combineWithResource
                    .getCapability(ColorableCapability.class).clone());

            ResourcePair resourcePair = new ResourcePair();
            resourcePair.setResourceData(brd);
            resourcePair.setLoadProperties(loadProps);

            /*
             * It is important to fully instantiate the blended resources before
             * removing the image resources from the descriptor. This prevents
             * threading issues that can occur if there is a brief period of
             * time where the image resources cannot be found within the
             * descriptor.
             */
            descriptor.getResourceList().add(resourcePair);
            descriptor.getResourceList().instantiateResources(descriptor, true);

            descriptor.getResourceList().remove(combineWithPair);
        }
        /*
         * Remove the new resource from the descriptor, it is now part of a
         * blended resource.
         */
        descriptor.getResourceList().remove(rp);
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
