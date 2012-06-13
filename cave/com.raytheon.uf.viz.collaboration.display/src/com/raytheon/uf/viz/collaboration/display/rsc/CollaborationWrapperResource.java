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
package com.raytheon.uf.viz.collaboration.display.rsc;

import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.Capabilities;
import com.raytheon.uf.viz.remote.graphics.DispatchGraphicsTarget;

/**
 * Collaboration resource that wraps an existing resource and extracts the
 * proper target to use
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationWrapperResource extends
        AbstractVizResource<AbstractResourceData, IDescriptor> {

    private AbstractVizResource<?, ?> wrappedResource;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public CollaborationWrapperResource(
            CollaborationWrapperResourceData resourceData,
            LoadProperties loadProperties,
            AbstractVizResource<?, ?> wrappedResource) {
        super(resourceData, loadProperties);
        this.wrappedResource = wrappedResource;
        wrappedResource.registerListener(new IRefreshListener() {
            @Override
            public void refresh() {
                issueRefresh();
            }
        });
    }

    /**
     * @return the wrappedResource
     */
    public AbstractVizResource<?, ?> getWrappedResource() {
        return wrappedResource;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        if (wrappedResource.getStatus() != ResourceStatus.DISPOSED) {
            wrappedResource.dispose();
        }
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
        if (target instanceof DispatchGraphicsTarget) {
            target = ((DispatchGraphicsTarget) target).getWrappedObject();
        }
        wrappedResource.paint(target, paintProps);
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
        if (wrappedResource.getStatus() == ResourceStatus.NEW) {
            if (target instanceof DispatchGraphicsTarget) {
                target = ((DispatchGraphicsTarget) target).getWrappedObject();
            }
            wrappedResource.init(target);
        }
    }

    /**
     * @param <C>
     * @param capability
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getCapability(java.lang.Class)
     */
    public <C extends AbstractCapability> C getCapability(Class<C> capability) {
        return wrappedResource.getCapability(capability);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getCapabilities()
     */
    public Capabilities getCapabilities() {
        return wrappedResource.getCapabilities();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getDescriptor()
     */
    public IDescriptor getDescriptor() {
        return wrappedResource.getDescriptor();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getDataTimes()
     */
    public DataTime[] getDataTimes() {
        return wrappedResource.getDataTimes();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getLoadProperties()
     */
    public LoadProperties getLoadProperties() {
        return wrappedResource.getLoadProperties();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getResourceData()
     */
    public AbstractResourceData getResourceData() {
        return wrappedResource.getResourceData();
    }

    public CollaborationWrapperResourceData getWrapperResourceData() {
        return (CollaborationWrapperResourceData) resourceData;
    }

    /**
     * @param capability
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#hasCapability(java.lang.Class)
     */
    public boolean hasCapability(Class<? extends AbstractCapability> capability) {
        return wrappedResource.hasCapability(capability);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    public String getName() {
        return wrappedResource.getName();
    }

    /**
     * @param coord
     * @return
     * @throws VizException
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon.uf.common.geospatial.ReferencedCoordinate)
     */
    public String inspect(ReferencedCoordinate coord) throws VizException {
        return wrappedResource.inspect(coord);
    }

    /**
     * @param coord
     * @return
     * @throws VizException
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#interrogate(com.raytheon.uf.common.geospatial.ReferencedCoordinate)
     */
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        return wrappedResource.interrogate(coord);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getStatus()
     */
    public ResourceStatus getStatus() {
        return wrappedResource.getStatus();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getResourceOrder()
     */
    public ResourceOrder getResourceOrder() {
        return wrappedResource.getResourceOrder();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#isTimeAgnostic()
     */
    public boolean isTimeAgnostic() {
        return wrappedResource.isTimeAgnostic();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getProperties()
     */
    public ResourceProperties getProperties() {
        return wrappedResource.getProperties();
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getResourceContainer()
     */
    public IDisplayPaneContainer getResourceContainer() {
        return wrappedResource.getResourceContainer();
    }

    /**
     * @return
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return wrappedResource.hashCode();
    }

    /**
     * @param obj
     * @return
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if (super.equals(obj)) {
            return true;
        }
        return wrappedResource.equals(obj);
    }

    /**
     * @param dataTime
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#remove(com.raytheon.uf.common.time.DataTime)
     */
    public void remove(DataTime dataTime) {
        wrappedResource.remove(dataTime);
    }

    /**
     * @return
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return wrappedResource.toString();
    }

    /**
     * @param crs
     * @throws VizException
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    public void project(CoordinateReferenceSystem crs) throws VizException {
        wrappedResource.project(crs);
    }

    /**
     * @return
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#okToUnload()
     */
    public boolean okToUnload() {
        return wrappedResource.okToUnload();
    }

    /**
     * @param updatedProps
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#propertiesChanged(com.raytheon.uf.viz.core.rsc.ResourceProperties)
     */
    public void propertiesChanged(ResourceProperties updatedProps) {
        wrappedResource.propertiesChanged(updatedProps);
    }

}
