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

import java.util.Collection;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;

import javax.measure.converter.UnitConverter;

import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 06, 2012           mschenke    Initial creation
 * Apr 18, 2014  2947     bsteffen    Support unitless data.
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
            return this.channel.getChannel() == channel;
        }

        /**
         * @return
         */
        public Channel getChannel() {
            return channel.getChannel();
        }

    }

    private static final String DEFAULT_NAME = "RGB Composite";

    private Map<Channel, DisplayedChannelResource> displayedResources;

    private ITrueColorImage image;

    private boolean timeAgnostic = true;

    private String baseName;

    /**
     * Mapping to keep colormap parameters in sync with ChannelInfo in
     * resourceData
     */
    private Map<ColorMapParameters, ChannelInfo> channelInfoMap = new IdentityHashMap<ColorMapParameters, ChannelInfo>();

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
            DrawableImage[] images = null;
            DisplayedChannelResource dcr = displayedResources.get(c);
            if (dcr != null) {
                DataTime dcrTime = fi.getTimeForResource(dcr.resource);
                if (dcrTime != null) {
                    paintProps.setDataTime(fi.getTimeForResource(dcr.resource));
                    Collection<DrawableImage> dcrImages = dcr.resource
                            .getCapability(ImagingCapability.class)
                            .getProvider().getImages(target, paintProps);
                    if (dcrImages != null) {
                        images = dcrImages.toArray(new DrawableImage[dcrImages
                                .size()]);
                    }
                }
            }
            image.setImages(c, images);
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

        displayedResources = new HashMap<Channel, DisplayedChannelResource>();
        ResourceList resources = getResourceList();
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
                            } else if (resource.getCapability(
                                    ColorMapCapability.class)
                                    .getColorMapParameters() == null) {
                                error = "does not have ColorMapParameters set";
                            }
                        } else {
                            error = "does not have image provider set on the ImagingCapability";
                        }
                    } else {
                        error = "does not have the ImagingCapability";
                    }
                    if (cr.getChannel() == null) {
                        error = "is not tied to any channel";
                    } else if (displayedResources.containsKey(cr.getChannel())) {
                        error = "is tied to a channel already in use";
                    }

                    if (error == null) {
                        // No errors so far, check for ChannelInfo override
                        ColorMapParameters params = resource.getCapability(
                                ColorMapCapability.class)
                                .getColorMapParameters();
                        ChannelInfo ci = resourceData
                                .getChannelInfo(displayedResource.getChannel());
                        if (ci == null
                                || ci.getUnit().isCompatible(
                                        params.getDataUnit())) {
                            if (ci == null) {
                                ci = new ChannelInfo();
                                ci.setChannel(displayedResource.getChannel());
                                resourceData.setChannelInfo(ci);
                            } else {
                                params.setDisplayUnit(ci.getUnit());
                                params.setColorMapMin((float) params
                                        .getDisplayToDataConverter().convert(
                                                ci.getRangeMin()));
                                params.setColorMapMax((float) params
                                        .getDisplayToDataConverter().convert(
                                                ci.getRangeMax()));
                            }
                            channelInfoMap.put(params, ci);
                            resourceChanged(
                                    ChangeType.CAPABILITY,
                                    resource.getCapability(ColorMapCapability.class));
                        } else {
                            error = "is not compatible with custom ChannelInfo for Channel="
                                    + displayedResource.getChannel();
                        }
                    }

                    if (error != null) {
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                displayedResource.getDisplayName()
                                        + " resource in true color composite "
                                        + error);
                        resources.remove(rp);
                    } else {
                        resource.getResourceData().addChangeListener(this);
                        displayedResources.put(displayedResource.getChannel(),
                                displayedResource);
                    }
                    break;
                }
            }
        }

        if (displayedResources.size() == 0) {
            throw new VizException(
                    "No resources to draw in true color composite");
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
        for (DisplayedChannelResource dr : displayedResources.values()) {
            timeAgnostic &= dr.resource.isTimeAgnostic();
        }

        String groupName = resourceData.getGroupName();
        if (groupName == null) {
            groupName = "True Color Composite";
        }
        String channels = " (";
        for (Channel c : Channel.values()) {
            if (displayedResources.containsKey(c)) {
                channels += c.name().substring(0, 1);
            }
        }
        channels += "): ";
        baseName = groupName + channels;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        String name = baseName;
        boolean first = true;
        for (Channel c : Channel.values()) {
            DisplayedChannelResource dcr = displayedResources.get(c);
            if (dcr != null) {
                if (!first) {
                    name += "/";
                }
                first = false;

                boolean has = true;
                if (image != null && image.getImages(c) == null) {
                    has = false;
                }
                if (has) {
                    name += dcr.getDisplayName();
                } else {
                    String channelName = c.name();
                    name += "No " + channelName.substring(0, 1)
                            + channelName.substring(1).toLowerCase();
                }
            }
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
            } else if (object instanceof ColorMapCapability) {
                ColorMapParameters params = ((ColorMapCapability) object)
                        .getColorMapParameters();
                UnitConverter toDisplay = params
                        .getColorMapToDisplayConverter();
                if (toDisplay == null) {
                    toDisplay = UnitConverter.IDENTITY;
                }
                ChannelInfo ci = channelInfoMap.get(params);
                if (ci != null) {
                    ci.setRangeMin(toDisplay.convert(
                            params.getColorMapMin()));
                    ci.setRangeMax(toDisplay.convert(
                            params.getColorMapMax()));
                    ci.setUnit(params.getDisplayUnit());
                }
            }
        }
        issueRefresh();
    }

    public Collection<DisplayedChannelResource> getChannelResources() {
        return displayedResources.values();
    }
}
