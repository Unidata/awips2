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
package com.raytheon.viz.pointdata.rsc;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * Resource data for plots
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2009             jsanchez    Initial creation
 * Jun 29, 2009     1934   jsanchez    Returned sample message for different plot resources.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class PlotBlendedResource extends
        AbstractVizResource<PlotBlendedResourceData, MapDescriptor> implements
        IResourceGroup, IResourceDataChanged {

    public PlotBlendedResource(PlotBlendedResourceData data,
            LoadProperties props) {
        super(data, props);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            rp.getResource().dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        for (ResourcePair rp : this.resourceData.resourceList) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            rsc.init(target);
            // If child resources have capabilities that this does not, steal
            // them
            for (AbstractCapability capability : rsc.getCapabilities()
                    .getCapabilityClassCollection()) {
                if (!hasCapability(capability.getClass())) {
                    this.getCapabilities().addCapability(capability);
                }
                capability.setResourceData(resourceData);
            }
        }

        // Spread my master capability set to all my children
        for (AbstractCapability capability : getCapabilities()
                .getCapabilityClassCollection()) {
            for (ResourcePair pair : getResourceList()) {
                AbstractVizResource<?, ?> rsc = pair.getResource();
                rsc.getCapabilities().addCapability(capability);
            }
        }

        resourceData.addChangeListener(this);

        target.setNeedsRefresh(true);
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
        for (ResourcePair rp : this.resourceData.resourceList) {
            AbstractVizResource rsc = rp.getResource();
            rsc.setDescriptor(descriptor);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        PaintProperties newProps = new PaintProperties(paintProps);
        AbstractVizResource<?, ?> rsc = null;
        for (int i = 0; i < resourceData.getResourceList().size(); i++) {
            rsc = resourceData.getResourceList().get(i).getResource();
            if (rsc.getProperties().isVisible()) {
                newProps.setDataTime(descriptor.getTimeForResource(rsc));
                PaintStatus paintStatus = rsc.paint(target, newProps);
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
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        for (ResourcePair rp : this.resourceData.resourceList) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            rsc.project(mapData);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IInspectableResource#inspect(com
     * .vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String rval = "";
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            String s = rsc.inspect(coord);
            if (s != null && !s.equals("NO DATA") && !s.equals("NO DATA")) {
                if (rval.length() > 0) {
                    rval += "\n";
                }
                rval += s;
            }

        }
        if (rval.length() == 0) {
            rval = "NO DATA";
        }
        return rval;
    }

    @Override
    public ResourceList getResourceList() {
        return this.resourceData.getResourceList();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            for (ResourcePair pair : getResourceList()) {
                pair.getResourceData().fireChangeListeners(type, object);
            }
        }

    }

}
