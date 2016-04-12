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
import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
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
 * Sep 10, 2014  DR 17313 jgerth      Add inspect method
 * Jan 27, 2016  DR 17997 jgerth      Support for gamma control
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
    private Map<ColorMapCapability, ChannelInfo> channelInfoMap = new IdentityHashMap<ColorMapCapability, ChannelInfo>();

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
            Double gamma = getGamma(c);
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
            image.setImages(c, gamma, images);
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
                        if (imaging.getProvider() == null) {
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
                        ColorMapCapability cmapCap = resource
                                .getCapability(ColorMapCapability.class);
                        ColorMapParameters params = cmapCap
                                .getColorMapParameters();

                        ChannelInfo ci = resourceData
                                .getChannelInfo(displayedResource.getChannel());
                        if (ci == null) {
                            ci = new ChannelInfo();
                            ci.setChannel(displayedResource.getChannel());
                            if (params != null) {
                                ci.setUnit(params.getColorMapUnit());
                                ci.setRangeMin(params.getColorMapMin());
                                ci.setRangeMax(params.getColorMapMax());
                            }
                            resourceData.setChannelInfo(ci);
                        }

                        channelInfoMap.put(cmapCap, ci);

                        if (params != null && params.getColorMapUnit() == null) {
                            // no colormap units set, default to ChannelInfo
                            params.setColorMapUnit(ci.getUnit());
                        }

                        initializeParameters(ci, params);
                    }

                    if (error != null) {
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                displayedResource.getDisplayName()
                                        + " resource in true color composite "
                                        + error);
                        resources.remove(rp);
                    } else {
                        // Listener will handle case where params are changed
                        // after the fact and we will reinitialize
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
                ChannelInfo ci = channelInfoMap.get(object);
                if (ci != null) {
                    if (ci.getParameters() != params) {
                        // Reinitialize params from channel info
                        initializeParameters(ci, params);
                    } else {
                        ci.setRangeMin(toDisplay.convert(params
                                .getColorMapMin()));
                        ci.setRangeMax(toDisplay.convert(params
                                .getColorMapMax()));
                        ci.setUnit(params.getDisplayUnit());
                    }
                }
            }
        }
        issueRefresh();
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String label = "";
        for (Channel c : Channel.values()) {
            DisplayedChannelResource dcr = displayedResources.get(c);
            if (dcr != null) {
                String ri = dcr.resource.inspect(coord);
                label += c.name() + ": " + ri;
                if (dcr.resource.hasCapability(ColorMapCapability.class) && ri.replaceAll("[^\\d]", "").length() > 0) {
                    ColorMapParameters cmp = dcr.resource.getCapability(ColorMapCapability.class).getColorMapParameters();
                    UnitConverter uc = cmp.getColorMapToDisplayConverter();
                    if (uc == null)
                        uc = UnitConverter.IDENTITY;
                    double cmmax = uc.convert(cmp.getColorMapMax());
                    double cmmin = uc.convert(cmp.getColorMapMin());
                    String rirall;
                    if (ri.charAt(0) == ('-'))
                        rirall = "-" + (ri.substring(1) + "x").replaceAll("[^\\d.]", " ");
                    else
                        rirall = (ri + "x").replaceAll("[^\\d.]", " ");
                    try {
                        double value = Double.parseDouble(rirall.substring(0,rirall.indexOf(" ")));
                        double percent = ((value - cmmin) * 100.0 / (cmmax - cmmin));
                        if (percent < 0)
                            percent = 0.0;
                        else if (percent > 100)
                            percent = 100.0;
                        label += " (" + Math.round(percent) + "%)\n";
                    } catch (Exception e) {
                        label += "\n";
                    }
                } else
                    label += "\n";
            }
        }
        if (label.length() > 0)
            return label;
        else
            return "NO DATA";
    }

    private static void initializeParameters(ChannelInfo ci,
            ColorMapParameters params) {
        if (params != null) {
            ci.setParameters(params);
            if (ci.getUnit() != null && params.getColorMapUnit() != null
                    && ci.getUnit().isCompatible(params.getColorMapUnit())) {
                params.setDisplayUnit(ci.getUnit());
                UnitConverter displayToCmap = params
                        .getDisplayToColorMapConverter();
                params.setColorMapMin(
                        (float) displayToCmap.convert(ci.getRangeMin()),
                        true);
                params.setColorMapMax(
                        (float) displayToCmap.convert(ci.getRangeMax()),
                        true);
            } else {
                UnitConverter cmapToDisplay = params
                        .getColorMapToDisplayConverter();
                ci.setRangeMin(cmapToDisplay.convert(params.getColorMapMin()));
                ci.setRangeMax(cmapToDisplay.convert(params.getColorMapMax()));
                ci.setUnit(params.getDisplayUnit());
            }
        }
    }

    public Collection<DisplayedChannelResource> getChannelResources() {
        return displayedResources.values();
    }

    public double getGamma(Channel c) {
        return resourceData.getChannelInfo(c).getGamma();
    }

    public void setGamma(Channel c, double gamma) {
        resourceData.getChannelInfo(c).setGamma(gamma);
    }

}
