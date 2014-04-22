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
package com.raytheon.viz.core.rsc;

import java.util.ArrayList;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.AbstractDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * Resource for rendering the best resource from a list. For each frame time
 * the resource will find the first(in the order of the list in resource data)
 * resource with data available and render only that resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 05, 2010           mnash       Initial creation
 * Mar 12, 2014  2898     bsteffen    Clear times in resource data on dispose.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class BestResResource extends
        AbstractVizResource<BestResResourceData, AbstractDescriptor> implements
        IResourceDataChanged, IRefreshListener {

    private AbstractVizResource<?, ?> vizResource = null;

    private ResourceOrder highestResourceOrder = null;

    /**
     * @param data
     * @param props
     */
    public BestResResource(BestResResourceData data, LoadProperties props) {
        super(data, props);
        data.addChangeListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        DataTime displayedDate = descriptor.getTimeForResource(this);
        AbstractVizResource<?, ?> rsc = this.resourceData.getMap().get(
                displayedDate);
        if (rsc == null) {
            return "";
        } else {
            DataTime[] dts = descriptor.getTimeMatchingMap().get(this);
            descriptor.getTimeMatchingMap().put(rsc, dts);
            String name = rsc.getName();
            descriptor.getTimeMatchingMap().remove(rsc);
            return name;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        for (AbstractVizResource<?, ?> resource : resourceData.getRscs()) {
            if (resource != null) {
                resource.dispose();
            }
        }
        resourceData.getMap().clear();
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
        // If child resources have capabilities that this does not, steal them
        for (AbstractVizResource<?, ?> rcs : getResourceData().getRscs()) {
            for (AbstractCapability capability : rcs.getCapabilities()
                    .getCapabilityClassCollection()) {
                if (!hasCapability(capability.getClass())) {
                    this.getCapabilities().addCapability(capability);
                    capability.setResourceData(resourceData);
                }
            }
        }

        // Set the master set of capabilites for all children and add listeners
        // for new capabilities.
        for (AbstractVizResource<?, ?> rcs : getResourceData().getRscs()) {
            rcs.getLoadProperties().setCapabilities(getCapabilities());
            rcs.getResourceData().addChangeListener(new IResourceDataChanged() {

                @Override
                public void resourceChanged(ChangeType type, Object object) {
                    if (type == ChangeType.CAPABILITY) {
                        AbstractCapability capability = ((AbstractCapability) object);
                        capability.setResourceData(resourceData);
                        getCapabilities().addCapability(capability);
                        for (AbstractVizResource<?, ?> rcs : getResourceData()
                                .getRscs()) {
                            rcs.getLoadProperties().setCapabilities(
                                    getCapabilities());
                        }
                    }
                }

            });
        }

        for (AbstractVizResource<?, ?> resource : resourceData.getRscs()) {
            if (resource != null) {
                resource.init(target);
                resource.registerListener(this);
            }
        }

        this.dataTimes = new ArrayList<DataTime>(resourceData.getMap().keySet());

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
        vizResource = getBestResResource(paintProps.getDataTime());
        if (vizResource != null) {
            vizResource.paint(target, paintProps);
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
        for (int i = 0; i < getResourceData().getRscs().size(); i++) {
            getResourceData().getRscs().get(i).project(crs);
        }
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public void setDescriptor(AbstractDescriptor descriptor) {
        for (AbstractVizResource resource : resourceData.getRscs()) {
            if (resource != null) {
                resource.setDescriptor(descriptor);
            }
        }
        this.descriptor = descriptor;
    }

    @Override
    public DataTime[] getDataTimes() {
        return resourceData.getMap().keySet().toArray(new DataTime[] {});
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
        // ((IResourceDataChanged) vizResource).resourceChanged(type, object);
    }

    @Override
    public String inspect(ReferencedCoordinate latLon) throws VizException {
        if (vizResource != null) {
            Map<AbstractVizResource<?, ?>, DataTime[]> timeMap = descriptor
                    .getTimeMatchingMap();
            timeMap.put(vizResource, timeMap.get(this));
            return vizResource.inspect(latLon);
        } else {
            return "";
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#interrogate(com.raytheon
     * .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        if (vizResource != null) {
            return vizResource.interrogate(coord);
        } else {
            return null;
        }
    }

    @Override
    public ResourceOrder getResourceOrder() {
        if (highestResourceOrder == null) {
            ResourceProperties props = getProperties();
            String orderId = null;
            if (props != null) {
                orderId = props.getRenderingOrderId();
            }
            if (orderId != null) {
                highestResourceOrder = RenderingOrderFactory
                        .getRenderingOrder(orderId);
            } else {
                for (ResourcePair rp : getResourceList()) {
                    if (rp.getResource() != null) {
                        ResourceOrder order = rp.getResource()
                                .getResourceOrder();
                        if (highestResourceOrder == null
                                || highestResourceOrder.value < order.value) {
                            highestResourceOrder = order;
                        }
                    }
                }
            }
            if (highestResourceOrder == null) {
                highestResourceOrder = ResourceOrder.UNKNOWN;
            }
        }
        return highestResourceOrder;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#remove(com.raytheon.
     * uf.common.time.DataTime)
     */
    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);

        if (resourceData != null) {
            AbstractVizResource<?, ?> rsc = resourceData.getMap().remove(
                    dataTime);
            if (rsc != null) {
                rsc.remove(dataTime);
            }
        }
    }

    public AbstractVizResource<?, ?> getBestResResource(DataTime time) {
        return resourceData.getMap().get(time);
    }

    public ResourceList getResourceList() {
        return resourceData.getResourceList();
    }

    @Override
    public void unload(ResourceList list) {
        for (ResourcePair rp : resourceData.getResourceList()) {
            if (rp.getResource() != null) {
                descriptor.getTimeMatchingMap().remove(rp.getResource());
            }
        }
        super.unload(list);
    }

    @Override
    public void refresh() {
        issueRefresh();
    }

}
