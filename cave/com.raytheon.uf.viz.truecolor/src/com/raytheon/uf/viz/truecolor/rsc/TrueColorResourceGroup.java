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

import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
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

        for (Channel c : Channel.values()) {
            ResourcePair rp = resourceData.getResource(c);
            if (rp != null) {
                image.setImages(c, getImages(rp, target, paintProps));
            }
        }

        Coordinate ul = new Coordinate(extent.getMinX(), extent.getMaxY());
        Coordinate ur = new Coordinate(extent.getMaxX(), extent.getMaxY());
        Coordinate lr = new Coordinate(extent.getMaxX(), extent.getMinY());
        Coordinate ll = new Coordinate(extent.getMinX(), extent.getMinY());

        target.drawRaster(image, new PixelCoverage(ul, ur, lr, ll), paintProps);
    }

    private DrawableImage[] getImages(ResourcePair rp, IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        paintProps.setDataTime(paintProps.getFramesInfo().getTimeForResource(
                rp.getResource()));
        ImagingCapability imaging = rp.getResource().getCapability(
                ImagingCapability.class);
        Collection<DrawableImage> images = imaging.getProvider().getImages(
                target, paintProps);
        return images.toArray(new DrawableImage[images.size()]);
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

        // Initialize them
        for (ResourcePair rp : getResourceList()) {
            AbstractVizResource<?, ?> resource = rp.getResource();
            resource.init(target);
            // Check resource for required capabilities
            String error = null;
            if (resource.hasCapability(ImagingCapability.class)) {
                ImagingCapability imaging = resource
                        .getCapability(ImagingCapability.class);
                if (imaging.getProvider() != null) {
                    if (resource.hasCapability(ColorMapCapability.class) == false) {
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
                        resourceData.getCompositeName(rp)
                                + " resource in true color composite " + error);
                resourceData.removeResource(rp);
            }
        }

        ITrueColorImagingExtension ext = target
                .getExtension(ITrueColorImagingExtension.class);
        image = ext.initializeRaster(new int[] { 0, 0 }, null);
        resourceData.addChangeListener(this);
        resourceChanged(ChangeType.CAPABILITY,
                getCapability(ImagingCapability.class));

        timeAgnostic = true;
        for (ResourcePair rp : getResourceList()) {
            // If any resource is not time agnostic, neither are we
            timeAgnostic &= rp.getResource().isTimeAgnostic();
        }
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
    }

}
