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
/**
 * 
 */
package com.raytheon.uf.viz.core.maps.rsc;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
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
 *                         randerso    Initial creation.
 * Feb 12, 2009            chammack    Rewrite for new resource model
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class MapResourceGroup extends
        AbstractMapResource<MapResourceGroupData, MapDescriptor> implements
        IResourceGroup {

    protected IGraphicsTarget lastTarget;

    protected MapResourceGroup(MapResourceGroupData data,
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
    protected void disposeInternal() {
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
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
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
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        int displayWidth = (int) (descriptor.getMapWidth() * paintProps
                .getZoomLevel());

        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, ?> resource = rp.getResource();
            ResourceProperties properties = rp.getProperties();

            if (properties.isDisplayable(displayWidth)) {
                PaintProperties newProps = new PaintProperties(paintProps);

                // keep these in sync

                resource.getCapabilities().addCapability(
                        getCapability(ColorableCapability.class));
                resource.getCapabilities().addCapability(
                        getCapability(OutlineCapability.class));

                if (resource.hasCapability(ImagingCapability.class)) {
                    paintProps.setAlpha(resource.getCapability(
                            ImagingCapability.class).getAlpha());
                }
                PaintStatus paintStatus = resource.paint(target, newProps);
                if (paintStatus != PaintStatus.PAINTED) {
                    updatePaintStatus(paintStatus);
                }
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

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.resourceData.getResourceList().toString();
    }

    @Override
    public ResourceList getResourceList() {
        return this.resourceData.getResourceList();
    }

}
