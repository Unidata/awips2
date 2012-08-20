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
package com.raytheon.uf.viz.truecolor.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension.ImageProvider;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.GroupNamingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.truecolor.Activator;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.Channel;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.ITrueColorImage;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource group that operates on 3 resources, one assigned to each band of an
 * RGB image. This resource checks for the {@link ImageProvider} on the
 * {@link ImagingCapability}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TrueColorResourceGroup extends
        AbstractVizResource<TrueColorResourceGroupData, IDescriptor> implements
        IResourceGroup, IResourceDataChanged {

    public static class DisplayedChannelResource {

        private String displayName;

        public AbstractVizResource<?, ?> resource;

        public ChannelResource channel;

        private DisplayedChannelResource(ChannelResource cr,
                AbstractVizResource<?, ?> resource) {
            this.channel = cr;
            this.resource = resource;
            this.displayName = cr.getChannelName();
            if (displayName == null) {
                displayName = resource.getName();
            }
        }

        /**
         * Returns the display name of the channel
         * 
         * @return
         */
        public String getDisplayName() {
            return displayName;
        }

        /**
         * Checks if the resource is bound to the specified {@link Channel}
         * 
         * @param channel
         * @return
         */
        public boolean isChannel(Channel channel) {
            return this.channel.channels.contains(channel);
        }

        /**
         * Removes a channel from being assigned to the resource
         * 
         * @param channel
         */
        public void removeChannel(Channel channel) {
            this.channel.channels.remove(channel);
        }

        /**
         * Adds a channel to be assigned to the resource
         * 
         * @param channel
         */
        public void addChannel(Channel channel) {
            this.channel.channels.add(channel);
        }

        /**
         * Set the channels to be used by the channel resource
         * 
         * @param channels
         */
        public void setChannels(Channel[] channels) {
            channel.setChannels(channels);
        }
    }

    private static final String DEFAULT_NAME = "RGB Composite";

    private List<DisplayedChannelResource> displayedResources;

    private ITrueColorImage image;

    private boolean timeAgnostic = true;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public TrueColorResourceGroup(TrueColorResourceGroupData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IResourceGroup#getResourceList()
     */
    @Override
    public ResourceList getResourceList() {
        return resourceData.getResourceList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        resourceData.removeChangeListener(this);
        for (ResourcePair rp : getResourceList()) {
            rp.getResource().dispose();
        }
        image.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        Rectangle rect = paintProps.getCanvasBounds();
        IExtent extent = paintProps.getView().getExtent();
        image.setSize(new int[] { rect.width, rect.height });
        image.setImageExtent(extent);

        FramesInfo fi = paintProps.getFramesInfo();
        for (Channel c : Channel.values()) {
            List<DrawableImage> images = new ArrayList<DrawableImage>();
            for (DisplayedChannelResource dcr : displayedResources) {
                if (dcr.isChannel(c)) {
                    paintProps.setDataTime(fi.getTimeForResource(dcr.resource));
                    images.addAll(dcr.resource
                            .getCapability(ImagingCapability.class)
                            .getProvider().getImages(target, paintProps));
                }
            }
            image.setImages(c, images.toArray(new DrawableImage[images.size()]));
        }

        Coordinate ul = new Coordinate(extent.getMinX(), extent.getMaxY());
        Coordinate ur = new Coordinate(extent.getMaxX(), extent.getMaxY());
        Coordinate lr = new Coordinate(extent.getMaxX(), extent.getMinY());
        Coordinate ll = new Coordinate(extent.getMinX(), extent.getMinY());

        target.drawRaster(image, new PixelCoverage(ul, ur, lr, ll), paintProps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#isTimeAgnostic()
     */
    @Override
    public boolean isTimeAgnostic() {
        return timeAgnostic;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        // We will name the composite
        getCapability(GroupNamingCapability.class);

        displayedResources = new ArrayList<DisplayedChannelResource>();
        ResourceList resources = getResourceList();
        // Make sure multiple resources are not assinged to same channel
        Set<Channel> usedChannels = new HashSet<Channel>();
        for (ChannelResource cr : resourceData.getChannelResources()) {
            for (ResourcePair rp : resources) {
                if (cr.getResourceData() == rp.getResourceData()) {
                    DisplayedChannelResource displayedResource = new DisplayedChannelResource(
                            cr, rp.getResource());
                    AbstractVizResource<?, ?> resource = rp.getResource();
                    resource.init(target);

                    // Check resource for required capabilities
                    String error = null;
                    if (resource.hasCapability(ImagingCapability.class)) {
                        ImagingCapability imaging = resource
                                .getCapability(ImagingCapability.class);
                        if (imaging.getProvider() != null) {
                            if (resource
                                    .hasCapability(ColorMapCapability.class) == false) {
                                error = "does not have ColorMapCapability";
                            }
                        } else {
                            error = "does not have image provider set on the ImagingCapability";
                        }
                    } else {
                        error = "does not have the ImagingCapability";
                    }
                    if (error != null) {
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                displayedResource.getDisplayName()
                                        + " resource in true color composite "
                                        + error);
                        resources.remove(rp);
                    } else {
                        resource.getResourceData().addChangeListener(this);
                        // Force channels unique to a single resource
                        for (Channel c : usedChannels) {
                            if (displayedResource.isChannel(c)) {
                                // Notify with INFO message?
                                displayedResource.removeChannel(c);
                            }
                        }
                        usedChannels
                                .addAll(Arrays.asList(displayedResource.channel
                                        .getChannels()));
                        displayedResources.add(displayedResource);
                    }
                    break;
                }
            }
        }

        ITrueColorImagingExtension ext = target
                .getExtension(ITrueColorImagingExtension.class);
        image = ext.initializeRaster(new int[] { 0, 0 }, null);
        resourceData.addChangeListener(this);
        // Set initial ImagingCapability parameters
        resourceChanged(ChangeType.CAPABILITY,
                getCapability(ImagingCapability.class));

        // Every resource has to be time agnostic for us to be as well
        timeAgnostic = true;
        for (DisplayedChannelResource dr : displayedResources) {
            timeAgnostic &= dr.resource.isTimeAgnostic();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        String name = resourceData.getGroupName();
        if (name != null) {
            name += " ";
        }

        if (image != null) {
            String productNames = "";
            boolean first = true;
            for (Channel c : Channel.values()) {
                DrawableImage[] images = image.getImages(c);
                if (images != null && images.length > 0) {
                    name += c.name().substring(0, 1);
                    if (!first) {
                        productNames += "/";
                    }
                    for (DisplayedChannelResource rsc : displayedResources) {
                        if (rsc.isChannel(c)) {
                            productNames += rsc.getDisplayName();
                            break;
                        }
                    }
                    first = false;
                }
            }
            if (first == true) {
                name += DEFAULT_NAME;
            } else {
                name += ": " + productNames;
            }
        } else {
            name += DEFAULT_NAME;
        }
        return name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        for (ResourcePair rp : getResourceList()) {
            rp.getResource().project(crs);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof ImagingCapability) {
                ImagingCapability imaging = (ImagingCapability) object;
                image.setBrightness(imaging.getBrightness());
                image.setContrast(imaging.getContrast());
                image.setInterpolated(imaging.isInterpolationState());
            }
        }
        issueRefresh();
    }

    public Collection<DisplayedChannelResource> getChannelResources() {
        return displayedResources;
    }
}
