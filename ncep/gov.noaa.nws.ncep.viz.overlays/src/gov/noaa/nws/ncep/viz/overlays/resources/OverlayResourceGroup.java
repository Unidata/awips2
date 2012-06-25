/**
 * 
 */
package gov.noaa.nws.ncep.viz.overlays.resources;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;

/**
 * 
 * Provides a capability to support homogenous (same color, style, etc.) maps as
 * a single resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/08/09                ghull       Created from MapResourceGroup
 * 11/19/09                ghull       Incorporate to11d6 changes
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class OverlayResourceGroup extends
        AbstractVizResource<OverlayResourceGroupData, MapDescriptor> 
                     implements INatlCntrsResource {

    protected IGraphicsTarget lastTarget;

    protected OverlayResourceGroup(OverlayResourceGroupData data,
            LoadProperties loadProperties) {
        super(data, loadProperties);

        this.resourceData.getResourceList().addPreAddListener(
                new ResourceList.AddListener() {

                    @SuppressWarnings("unchecked")
                    @Override
                    public void notifyAdd(ResourcePair rp) throws VizException {
                        addListener((AbstractVizResource<?, MapDescriptor>) rp
                                .getResource());
                    }

                });

    }

    protected void addListener(AbstractVizResource<?, MapDescriptor> rsc)
            throws VizException {

        if (descriptor != null) {
            rsc.setDescriptor(descriptor);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IProjectableResource#project(org
     * .opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            rsc.project(mapData);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    public void disposeInternal() {
        lastTarget = null;
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            rp.getResource().dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    public void initInternal(IGraphicsTarget target) throws VizException {
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc != null) {
                rsc.init(target);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    @Override
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        int displayWidth = (int) (descriptor.getMapWidth() * paintProps
                .getZoomLevel());

        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, ?> resource = rp.getResource();
            ResourceProperties properties = rp.getProperties();

            if (properties.isDisplayable(displayWidth)) {
                PaintProperties newProps = new PaintProperties(paintProps);

                // keep these in sync
/*
                resource.getCapabilities().addCapability(
                        getCapability(ColorableCapability.class));
                resource.getCapabilities().addCapability(
                        getCapability(OutlineCapability.class));

                if (resource.hasCapability(ImagingCapability.class)) {
                    paintProps.setAlpha(resource.getCapability(
                            ImagingCapability.class).getAlpha());
                }
*/
                resource.paint(target, newProps);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#setDescriptor(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @SuppressWarnings("unchecked")
    @Override
    public void setDescriptor(MapDescriptor descriptor) {

        super.setDescriptor(descriptor);
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, MapDescriptor> r = (AbstractVizResource<?, MapDescriptor>) rp
                    .getResource();
            if (r != null) {
                r.setDescriptor(descriptor);
            }
        }
    }

	@Override
	public void resourceAttrsModified() {
        for (ResourcePair rp : this.resourceData.getResourceList()) {
        	INatlCntrsResource ncRsc = (INatlCntrsResource) rp.getResource();
        	ncRsc.resourceAttrsModified();
        }
	}
    
	@Override
    public String toString() {
        return this.resourceData.getResourceList().toString();
    }
}
