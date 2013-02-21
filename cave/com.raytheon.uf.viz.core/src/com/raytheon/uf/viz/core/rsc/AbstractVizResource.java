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
package com.raytheon.uf.viz.core.rsc;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.Capabilities;

/**
 * Provides a base implementation for creating visualizations that participate
 * in a descriptor. Examples of concrete implementations include displaying map
 * vector data, interactive drawing, and XY visualization of numerical data.
 * 
 * Unlike the original IVizResource implementation, the AbstractVizResource is
 * not serialized in its entirety. Instead, just the AbstractResourceData that
 * it contains is serialized.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2009             chammack    Initial creation from original IVizResource
 * Mar 3, 2009      2032   jsanchez    Added getDescriptor and paintProps.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@SuppressWarnings("unchecked")
public abstract class AbstractVizResource<T extends AbstractResourceData, D extends IDescriptor> {

    public enum ResourceStatus {
        NEW, LOADING, INITIALIZED, DISPOSED
    }

    private ResourceStatus status = ResourceStatus.NEW;

    private PaintStatus paintStatus = PaintStatus.REPAINT;

    /**
     * Should be returned if a resource does not pertain to time (e.g. static
     * map backgrounds)
     */
    public static final List<DataTime> TIME_AGNOSTIC = Collections.EMPTY_LIST;

    /**
     * The descriptor that this resource is contained in. This is frequently
     * useful for finding information out about the background
     * environment/information about the temporal or spatial context
     */
    protected D descriptor;

    /**
     * The resource data that was used to construct this resource, and generic
     * properties that should be used in the rendering of the data
     */
    protected T resourceData;

    /**
     * The list of DataTimes that are currently loaded into this resource. NOTE:
     * This is not the same thing as all DataTimes that are currently
     * *available* (a list which could be quite large). The system may determine
     * to load and unload specific times from the list of times as they become
     * available or no longer needed.
     */
    protected List<DataTime> dataTimes = TIME_AGNOSTIC;

    /**
     * The LoadProperties used to load the resource
     */
    protected LoadProperties loadProperties;

    /**
     * List of Refresh listeners call registerListener to modify list
     */
    private Set<IRefreshListener> refreshListeners;

    /**
     * List of Init listeners call registerListener to modify list
     */
    private Set<IInitListener> initListeners;

    /**
     * List of Paint listeners call registerListener to modify list
     */
    private Set<IPaintListener> paintListeners;

    /**
     * List of Paint listeners call registerListener to modify list
     */
    private Set<IPaintStatusChangedListener> paintStatusListeners;

    /**
     * List of Dispose listeners call registerListener to modify list
     */
    private Set<IDisposeListener> disposeListeners;

    /**
     * The base constructor
     * 
     * This must be implemented and called by any concrete class class
     * 
     * @param resourceData
     *            the resource data
     * @param capabilities
     *            the capabilities
     */
    protected AbstractVizResource(T resourceData, LoadProperties loadProperties) {
        this.resourceData = resourceData;
        this.loadProperties = loadProperties;
        refreshListeners = new CopyOnWriteArraySet<IRefreshListener>();
        initListeners = new CopyOnWriteArraySet<IInitListener>();
        paintListeners = new CopyOnWriteArraySet<IPaintListener>();
        paintStatusListeners = new CopyOnWriteArraySet<IPaintStatusChangedListener>();
        disposeListeners = new CopyOnWriteArraySet<IDisposeListener>();

        if (resourceData != null) {
            resourceData.addChangeListener(new IResourceDataChanged() {
                @Override
                public void resourceChanged(ChangeType type, Object object) {
                    if (type == ChangeType.DATA_REMOVE
                            && object instanceof DataTime) {
                        remove((DataTime) object);
                    }
                }
            });
        }
    }

    /**
     * Set the descriptor for the resource
     * 
     * This method will be called prior to paint, or when the Descriptor
     * changes.
     * 
     * @param descriptor
     *            the descriptor
     */
    public void setDescriptor(D descriptor) {
        this.descriptor = descriptor;
    }

    /**
     * Return the descriptor
     * 
     * @return the descriptor
     */
    public D getDescriptor() {
        return this.descriptor;
    }

    /**
     * Return the resource data
     * 
     * @return the resource data
     */
    public T getResourceData() {
        return resourceData;
    }

    /**
     * Return back a capability for the resource
     */
    public <C extends AbstractCapability> C getCapability(Class<C> capability) {
        return getCapabilities().getCapability(getResourceData(), capability);
    }

    /**
     * Check for capability in resource.
     * 
     * @param capability
     * @return true if resource has the capability; false otherwise
     */
    public boolean hasCapability(Class<? extends AbstractCapability> capability) {
        return getLoadProperties().getCapabilities().hasCapability(capability);
    }

    /**
     * Get a list of displayable times
     * 
     * @return the currently loaded set of datatimes
     */
    public DataTime[] getDataTimes() {
        return this.dataTimes.toArray(new DataTime[this.dataTimes.size()]);
    }

    /**
     * Remove a data time from the list of available times.
     * 
     * It may be a good idea to override this method and dispose of any data
     * that is no longer relevant.
     * 
     * @param dataTime
     *            the data time to remove
     */
    public void remove(DataTime dataTime) {
        this.dataTimes.remove(dataTime);
    }

    /**
     * Return the name of the resource
     * 
     * @return the name of the resource
     */
    public String getName() {
        // Implementation:
        // The base implementation will call the name generator to decorate a
        // proper name based on the currently displayed data

        if (resourceData == null) {
            return "";
        }

        AbstractNameGenerator generator = resourceData.getNameGenerator();
        return generator != null ? generator.getName(this) : null;
    }

    /**
     * Perform an inspect operation at a particular point of interest.
     * 
     * An inspect operation returns a simple string result.
     * 
     * 
     * The point of interest is provided as a ReferencedCoordinate, which can be
     * expressed as a pixel, world or CRS coordinate (if applicable)
     * 
     * Inspect may frequently be implemented by calling interrogate, and
     * formatting the result as a string.
     * 
     * @param coord
     *            the coordinate
     * @return a string that represents the result of inspecting that point
     * @throws VizException
     */
    public String inspect(ReferencedCoordinate coord) throws VizException {
        // No inspection by default
        return null;
    }

    /**
     * Perform an interrogate operation at a particular point of interest.
     * 
     * An interrogate operation returns an unformatted hashmap of data. This
     * data can be formatted by the user.
     * 
     * The point of interest is provided as a ReferencedCoordinate, which can be
     * expressed as a pixel, world or CRS coordinate (if applicable)
     * 
     * @param coord
     * @return the data values at coord
     * @throws VizException
     */
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        // No Interrogation by default
        return null;
    }

    /**
     * Notify the resource that the CoordinateReferenceSystem has changed, and
     * that the resource should use this new CRS for rendering its data.
     * 
     * @param crs
     *            the new Coordinate Reference System
     * @throws VizException
     */
    public void project(CoordinateReferenceSystem crs) throws VizException {
        return; // NO OP
    }

    /**
     * Do not call this method unless calling from initInternal in a blended
     * resource
     * 
     * This method will be called before paint.
     * 
     * This method is frequently used for longer running pre-processing steps.
     * 
     * @param target
     *            the graphics target
     * @throws VizException
     *             if initialization fails
     */
    public final void init(IGraphicsTarget target) throws VizException {
        status = ResourceStatus.LOADING;
        initInternal(target);
        status = ResourceStatus.INITIALIZED;

        for (IInitListener listener : initListeners) {
            listener.inited(this);
        }
        issueRefresh();
    }

    /**
     * Dispose the resource
     * 
     * The resource should release any native resources such as SWT colors and
     * Renderable Resources such as IWireframeShapes.
     * 
     * For improving garbage collection, it is also often desirable to clear any
     * collections or maps so that the objects are more likely to be flagged by
     * the garbage collector.
     * 
     */
    public final void dispose() {
        if (status == ResourceStatus.INITIALIZED) {
            status = ResourceStatus.DISPOSED;
            disposeInternal();
            for (IDisposeListener listener : disposeListeners) {
                listener.disposed(this);
            }
        }
    }

    /**
     * The method used to actually dispose of the resource
     */
    protected abstract void disposeInternal();

    /**
     * Return the capabilities for the resource
     * 
     * @return the capabilities
     */
    public Capabilities getCapabilities() {
        return getLoadProperties().getCapabilities();
    }

    /**
     * @return the loadProperties
     */
    public LoadProperties getLoadProperties() {
        if (loadProperties == null) {
            loadProperties = new LoadProperties();
        }
        return loadProperties;
    }

    public boolean okToUnload() {
        return true;
    }

    /**
     * unload Resource from the descriptor's resourceList
     */
    public void unload() {
        unload(descriptor.getResourceList());
    }

    /**
     * unload resource from specified list
     * 
     * @param list
     */
    public void unload(ResourceList list) {
        list.removeRsc(this);
    }

    /** The thread to call init on the resource from */
    private class InitJob extends Job {
        private IGraphicsTarget target;

        private VizException exception;

        public InitJob(IGraphicsTarget target) {
            super("Initializing...");
            this.target = target;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                // Call init
                init(target);
            } catch (VizException e) {
                this.exception = e;
            }
            // issue paint to be called again
            issueRefresh();
            return Status.OK_STATUS;
        }
    }

    private InitJob initJob = null;

    private ResourceProperties properties;

    public final PaintStatus paint(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        switch (status) {
        case NEW: {
            if (initJob == null) {
                initJob = new InitJob(target);
                initJob.schedule();
            }
            updatePaintStatus(PaintStatus.INCOMPLETE);
            break;
        }
        case LOADING: {
            // still initializing, check for exceptions
            if (initJob != null && initJob.exception != null) {
                VizException e = initJob.exception;
                // Reset status and job
                status = ResourceStatus.NEW;
                initJob = null;
                throw e;
            }
            updatePaintStatus(PaintStatus.INCOMPLETE);
            break;
        }
        case INITIALIZED: {
            // We have initialized successfully, now time to paint
            try {
                updatePaintStatus(PaintStatus.PAINTING);
                paintInternal(target, paintProps);
            } catch (VizException e) {
                updatePaintStatus(PaintStatus.ERROR);
                throw e;
            }
            if (paintStatus == PaintStatus.PAINTING) {
                updatePaintStatus(PaintStatus.PAINTED);
            }
            for (IPaintListener listener : paintListeners) {
                listener.painted(this);
            }
            break;
        }
        case DISPOSED: {
            throw new VizException(
                    "Unable to paint, resource has been disposed");
        }
        }
        return paintStatus;
    }

    /**
     * Get the current status of the resource
     * 
     * @return The current status
     */
    public ResourceStatus getStatus() {
        return this.status;
    }

    /**
     * This method will do the actual painting of the resource
     * 
     * @param target
     *            the graphics target
     * @param paintProps
     *            the paint properties
     * @throws VizException
     */
    protected abstract void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException;

    /**
     * Method used to initialize anything that may need initializing before
     * painting. This method will be called outside of the UI thread the first
     * time paint is called. RESOURCES THAT PAINT INTERNAL RESOURCES SHOULD NOT
     * CALL THIS METHOD, CALL init(IGraphicsTarget) INSTEAD SO ResourceStatus
     * GETS SET PROPERLY
     * 
     * @param target
     * @throws VizException
     */
    protected abstract void initInternal(IGraphicsTarget target)
            throws VizException;

    /**
     * Issue a refresh on the resource, all listeners will be notified to
     * refresh
     */
    public final void issueRefresh() {
        updatePaintStatus(PaintStatus.REPAINT);
        for (IRefreshListener list : refreshListeners) {
            list.refresh();
        }
    }

    /**
     * Register a refresh listener on the resource, IRefreshListener.refresh
     * will be called from issueRefresh
     * 
     * @param listener
     *            the refresh listener
     */
    public final void registerListener(IRefreshListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                rp.getResource().registerListener(listener);
            }
        }
        refreshListeners.add(listener);
    }

    /**
     * Remove a refresh listener from the resource, IRefreshListener.refresh
     * will be called from issueRefresh
     * 
     * @param listener
     *            the refresh listener
     */
    public final void unregisterListener(IRefreshListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null) {
                    rsc.unregisterListener(listener);
                }
            }
        }
        refreshListeners.remove(listener);
    }

    /**
     * Register a dispose listener on the resource, IDisposeListener.dispose
     * will be called when the resource is disposed
     * 
     * @param listener
     *            the dispose listener
     */
    public final void registerListener(IDisposeListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                if (rp != null && rp.getResource() != null) {
                    rp.getResource().registerListener(listener);
                }
            }
        }
        disposeListeners.add(listener);
    }

    /**
     * Remove a dispose listener from the resource
     * 
     * @param listener
     *            the dispose listener
     */
    public final void unregisterListener(IDisposeListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null) {
                    rsc.unregisterListener(listener);
                }
            }
        }
        disposeListeners.remove(listener);
    }

    /**
     * Register a init listener on the resource, IInitListener.inited will be
     * called when the resource is initialized
     * 
     * @param listener
     *            the init listener
     */
    public final void registerListener(IInitListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                rp.getResource().registerListener(listener);
            }
        }
        initListeners.add(listener);
    }

    /**
     * Remove a init listener from the resource
     * 
     * @param listener
     *            the init listener
     */
    public final void unregisterListener(IInitListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null) {
                    rsc.unregisterListener(listener);
                }
            }
        }
        initListeners.remove(listener);
    }

    /**
     * Register a paint listener on the resource, IPaintListener.painted will be
     * called when the resource is painted
     * 
     * @param listener
     *            the paint listener
     */
    public final void registerListener(IPaintListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                rp.getResource().registerListener(listener);
            }
        }
        paintListeners.add(listener);
    }

    /**
     * Remove a paint listener from the resource
     * 
     * @param listener
     *            the paint listener
     */
    public final void unregisterListener(IPaintListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null) {
                    rsc.unregisterListener(listener);
                }
            }
        }
        paintListeners.remove(listener);
    }

    /**
     * Register a paint listener on the resource, IPaintListener.painted will be
     * called when the resource is painted
     * 
     * @param listener
     *            the paint listener
     */
    public final void registerListener(IPaintStatusChangedListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                rp.getResource().registerListener(listener);
            }
        }
        paintStatusListeners.add(listener);
    }

    /**
     * Remove a paint listener from the resource
     * 
     * @param listener
     *            the paint listener
     */
    public final void unregisterListener(IPaintStatusChangedListener listener) {
        if (this instanceof IResourceGroup) {
            for (ResourcePair rp : ((IResourceGroup) this).getResourceList()) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null) {
                    rsc.unregisterListener(listener);
                }
            }
        }
        paintStatusListeners.remove(listener);
    }

    /**
     * Update the resources paint status
     * 
     * @param newStatus
     */
    protected final void updatePaintStatus(PaintStatus newStatus) {
        PaintStatus oldStatus = paintStatus;
        paintStatus = newStatus;
        if (oldStatus != newStatus) {
            for (IPaintStatusChangedListener listener : paintStatusListeners) {
                listener.statusChanged(oldStatus, newStatus, this);
            }
        }
    }

    public PaintStatus getPaintStatus() {
        return paintStatus;
    }

    /**
     * Recycle a resource to be used again, will call dispose on the resource if
     * it is in the INITIALIZED state, otherwise sets status to NEW which will
     * trigger the initInternal method on the next paint
     */
    public final void recycle() {
        if (status == ResourceStatus.INITIALIZED) {
            disposeInternal();
        }
        status = ResourceStatus.NEW;
        initJob = null;
        dataTimes.clear();
    }

    public ResourceOrder getResourceOrder() {
        return RenderingOrderFactory.getRenderingOrder(this);
    }

    /**
     * Notified when the ResourceProperties change, gets called when setVisible,
     * setHoverOn, and setBlinking is called
     * 
     * @param updatedProps
     */
    public void propertiesChanged(ResourceProperties updatedProps) {

    }

    /**
     * Method that declares if the resource is time agnostic
     * 
     * @return true if resource is time agnostic
     */
    public boolean isTimeAgnostic() {
        return dataTimes == TIME_AGNOSTIC;
    }

    /**
     * Looks up the ResourceProperties in the resouce's descriptor's resource
     * list
     * 
     * @return the resource properties, may be null
     */
    public ResourceProperties getProperties() {
        return this.properties;
    }

    /**
     * Get the container the resource is loaded to
     * 
     * @return the container
     */
    public IDisplayPaneContainer getResourceContainer() {
        IDisplayPaneContainer container = null;
        if (descriptor != null) {
            IRenderableDisplay display = descriptor.getRenderableDisplay();
            if (display != null) {
                container = display.getContainer();
            }
        }
        return container;
    }

    /**
     * set the properties for this resource
     * 
     * @param properties
     *            the resource properties
     */
    protected void setProperties(ResourceProperties properties) {
        this.properties = properties;
    }
}
