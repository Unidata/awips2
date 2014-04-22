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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.NoDataAvailableException;
import com.raytheon.uf.viz.core.exception.NoMatchingTimesException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * ResourceList implements a list that contains resources
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 05, 2007           chammack    Initial Creation.
 * Apr 09, 2009  1288     rjpeter     Added iterator implementation to fix
 *                                    remove.
 * Apr 24, 2013  1950     bsteffen    Sort resources before instantiation.
 * Oct 22, 2013  2491     bsteffen    Remove ISerializableObject
 * Jan 17, 2013  2651     bsteffen    Synchronize removeRsc for slightly better
 *                                    thread safety.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ResourceList extends CopyOnWriteArrayList<ResourcePair> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ResourceList.class);

    private static final long serialVersionUID = 1L;

    private static final int HIGHEST = RenderingOrderFactory.ResourceOrder.HIGHEST.value;

    private static final int LOWEST = RenderingOrderFactory.ResourceOrder.LOWEST.value;

    private static final Comparator<ResourcePair> INSTANTIATION_ORDERER = new Comparator<ResourcePair>() {

        @Override
        public int compare(ResourcePair rp1, ResourcePair rp2) {
            if (rp1.getProperties().isSystemResource()) {
                if (rp2.getProperties().isSystemResource()) {
                    return 0;
                } else {
                    return -1;
                }
            } else if (rp2.getProperties().isSystemResource()) {
                return 1;
            }
            if (rp1.getProperties().isMapLayer()) {
                if (rp2.getProperties().isMapLayer()) {
                    return 0;
                } else {
                    return -1;
                }
            } else if (rp2.getProperties().isMapLayer()) {
                return 1;
            }
            return 0;
        }

    };
    
    private final transient Set<AddListener> preAddListeners;

    private final transient Set<AddListener> postAddListeners;

    private final transient Set<RemoveListener> preRemoveListeners;

    private final transient Set<RemoveListener> postRemoveListeners;

    private final List<ResourcePair> resourcesToInstantiate = new LinkedList<ResourcePair>();

    private final String uniqueID = UUID.randomUUID().toString();

    /**
     * Defines the move operations that can be performed by the moveResource
     * method
     * 
     */
    public enum MoveOperation {
        Up, Down, ToTop, ToBottom
    };

    public ResourceList() {
        preAddListeners = new LinkedHashSet<AddListener>();
        postAddListeners = new LinkedHashSet<AddListener>();
        preRemoveListeners = new LinkedHashSet<RemoveListener>();
        postRemoveListeners = new LinkedHashSet<RemoveListener>();

        addPostAddListener(new AddListener() {
            @Override
            public void notifyAdd(ResourcePair rp) throws VizException {
                // Register with catalog
                ResourceCatalog.getInstance().addResource(rp.getResource(),
                        uniqueID);
            }
        });

        addPostRemoveListener(new RemoveListener() {
            @Override
            public void notifyRemove(ResourcePair rp) throws VizException {
                ResourceCatalog.getInstance().removeResource(rp.getResource(),
                        uniqueID);
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.List#add(int, java.lang.Object)
     */
    @Override
    public void add(int index, ResourcePair element) {
        throw new UnsupportedOperationException();
    }

    /**
     * Add a resource with a default resource properties (convenience method)
     * 
     * @param vr
     *            the resource
     * @return true if the add succeeds
     */
    public boolean add(AbstractVizResource<?, ?> vr) {
        return add(vr, new ResourceProperties());
    }

    /**
     * Add a resource with a specified resource properties
     * 
     * @param vr
     *            the resource
     * @param props
     *            the resource properties
     * @return
     */
    public boolean add(AbstractVizResource<?, ?> vr, ResourceProperties props) {
        ResourcePair rp = new ResourcePair();
        rp.setResource(vr);
        rp.setProperties(props);
        return add(rp);
    }

    /**
     * Function for determining if the {@link ResourcePair} can be added to the
     * list. Default checks if the pair is already in the list and returns false
     * if so
     * 
     * @param e
     * @return
     */
    protected boolean canAdd(ResourcePair e) {
        return contains(e) == false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.List#add(java.lang.Object)
     */
    @Override
    public boolean add(ResourcePair e) {
        if (e == null || canAdd(e) == false) {
            return false;
        }

        // Only fire listeners when resourcepairs are fully instantiated
        boolean fireListeners = false;
        if (e.getResource() != null) {
            fireListeners = true;
        }

        if (fireListeners) {
            try {
                firePreAddListeners(e);
            } catch (VizException e1) {
                return false;
            }
        }
        boolean b = addInternal(e);
        if (b) {
            if (fireListeners) {
                try {
                    firePostAddListeners(e);
                } catch (VizException e1) {
                    return false;
                }
            }
        }
        return b;
    }

    public void firePostAddListeners(ResourcePair e) throws VizException {
        fireAddListeners(e, postAddListeners);
    }

    public void firePreAddListeners(ResourcePair e) throws VizException {
        fireAddListeners(e, preAddListeners);
    }

    private void fireAddListeners(ResourcePair e,
            Collection<AddListener> listeners) throws VizException {
        try {
            List<AddListener> copy = new ArrayList<AddListener>();
            synchronized (listeners) {
                copy.addAll(listeners);
            }
            for (AddListener al : copy) {
                al.notifyAdd(e);
            }
        } catch (VizException e1) {
            statusHandler
                    .handle(Priority.PROBLEM, "Error notifying add listener: "
                            + e1.getLocalizedMessage(), e1);
            throw e1;
        }
    }

    private void fireRemoveListeners(ResourcePair e,
            Collection<RemoveListener> listeners) throws VizException {
        try {
            List<RemoveListener> copy = new ArrayList<RemoveListener>();
            synchronized (listeners) {
                copy.addAll(listeners);
            }
            for (RemoveListener al : copy) {
                al.notifyRemove(e);
            }
        } catch (VizException e1) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Error notifying remove listener: "
                            + e1.getLocalizedMessage(), e1);
            throw e1;
        }
    }

    private boolean addInternal(ResourcePair e) {
        ResourceProperties props = e.getProperties();
        if (props == null) {
            props = new ResourceProperties();
            e.setProperties(props);
        }

        int order = props.getRenderingOrder();
        if (order == RenderingOrderFactory.ResourceOrder.NOT_SET.value
                && e.getResource() != null) {
            String id = props.getRenderingOrderId();
            if (id == null) {
                ResourceOrder ro = e.getResource().getResourceOrder();
                id = ro.id;
                order = ro.value;
            } else {
                order = RenderingOrderFactory.getRenderingOrder(id).value;
            }
            props.setRenderingOrder(order);
            props.setRenderingOrderId(id);
        }

        synchronized (this) {
            if (canAdd(e) == false) {
                return false;
            }

            int i = size() - 1;
            for (; i >= 0; i--) {
                if (get(i).getProperties().getRenderingOrder() <= order) {
                    break;
                }
            }

            super.add(i >= 0 ? (i + 1) : 0, e);
            synchronized (resourcesToInstantiate) {
                resourcesToInstantiate.add(e);
            }
        }

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.List#addAll(java.util.Collection)
     */
    @Override
    public boolean addAll(Collection<? extends ResourcePair> c) {
        Validate.notNull(c);
        List<ResourcePair> toAdd = new ArrayList<ResourcePair>(c.size());
        synchronized (this) {
            for (ResourcePair rp : c) {
                Validate.notNull(rp);
                if (rp.getResource() == null) {
                    Validate.notNull(rp.getResourceData());
                }
                if (canAdd(rp)) {
                    toAdd.add(rp);
                }
            }
        }

        boolean anyModified = false;
        for (ResourcePair rp : toAdd) {
            if (rp.getResource() != null) {
                try {
                    firePreAddListeners(rp);
                } catch (VizException e1) {
                    return false;
                }
            }

            boolean modified = this.addInternal(rp);

            if (modified) {
                if (rp.getProperties() == null) {
                    rp.setProperties(new ResourceProperties());
                }

                if (rp.getResource() != null) {
                    try {
                        firePostAddListeners(rp);
                    } catch (VizException e1) {
                        return false;
                    }
                }
                anyModified = true;
            }

        }

        return anyModified;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.List#addAll(int, java.util.Collection)
     */
    @Override
    public boolean addAll(int index, Collection<? extends ResourcePair> c) {
        throw new UnsupportedOperationException();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.List#clear()
     */
    @Override
    public void clear() {
        List<ResourcePair> copy = new ArrayList<ResourcePair>(this);
        // Notify listeners of all items being removed
        for (ResourcePair rp : copy) {
            try {
                fireRemoveListeners(rp, preRemoveListeners);
            } catch (VizException e1) {
                // eat exception
            }
        }

        super.removeAll(copy);

        for (ResourcePair rp : copy) {
            try {
                fireRemoveListeners(rp, postRemoveListeners);
            } catch (VizException e1) {
                // eat exception
            }
        }

        synchronized (resourcesToInstantiate) {
            resourcesToInstantiate.clear();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.List#remove(int)
     */
    @Override
    public ResourcePair remove(int index) {
        ResourcePair rp = get(index);
        if (rp == null) {
            return null;
        }

        try {
            fireRemoveListeners(rp, preRemoveListeners);
        } catch (VizException e1) {
            // eat exception
        }

        super.remove(index);
        synchronized (resourcesToInstantiate) {
            // Ensure rp is removed from list entirely
            Iterator<ResourcePair> iter = resourcesToInstantiate.iterator();
            while (iter.hasNext()) {
                if (iter.next() == rp) {
                    iter.remove();
                }
            }
        }

        try {
            fireRemoveListeners(rp, postRemoveListeners);
        } catch (VizException e1) {
            // eat exception
        }

        return rp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.List#remove(java.lang.Object)
     */
    @Override
    public boolean remove(Object o) {
        Validate.notNull(o);
        Validate.isTrue(o instanceof ResourcePair);

        int idx = indexOf(o);
        if (idx < 0) {
            return false;
        }

        remove(idx);

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.List#removeAll(java.util.Collection)
     */
    @Override
    public boolean removeAll(Collection<?> c) {
        Validate.notNull(c);

        for (Object o : c) {
            Validate.notNull(o);
            Validate.isTrue(o instanceof ResourcePair);

            this.remove(o);
        }

        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.List#retainAll(java.util.Collection)
     */
    @Override
    public boolean retainAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    /**
     * Return the ResourceProperties for a specified resource
     * 
     * @param rsc
     *            the resource
     * @return the resource properties
     */
    public ResourceProperties getProperties(AbstractVizResource<?, ?> rsc) {
        // Do not support searching for null resources as any number of
        // resources could be null in list
        if (rsc == null) {
            return null;
        }

        ResourcePair loc = null;
        for (ResourcePair rp : this) {
            if (rp.getResource() == rsc) {
                loc = rp;
                break;
            }
        }

        // Resource not found
        if (loc == null) {
            return null;
        }

        return loc.getProperties();

    }

    /**
     * Add a pre-add listener
     * 
     * @param al
     */
    public void addPreAddListener(AddListener al) {
        Validate.notNull(al, "Attempting to add null listener");
        synchronized (preAddListeners) {
            this.preAddListeners.add(al);
        }
    }

    /**
     * Add a post-add listener
     * 
     * @param al
     */
    public void addPostAddListener(AddListener al) {
        Validate.notNull(al, "Attempting to add null listener");
        synchronized (postAddListeners) {
            this.postAddListeners.add(al);
        }
    }

    /**
     * Add a pre-remove listener
     * 
     * @param rl
     */
    public void addPreRemoveListener(RemoveListener rl) {
        Validate.notNull(rl, "Attempting to add null listener");
        synchronized (preRemoveListeners) {
            this.preRemoveListeners.add(rl);
        }
    }

    /**
     * Add a post-remove listener
     * 
     * @param rl
     */
    public void addPostRemoveListener(RemoveListener rl) {
        Validate.notNull(rl, "Attempting to add null listener");
        synchronized (postRemoveListeners) {
            this.postRemoveListeners.add(rl);
        }
    }

    /**
     * Add a pre-add listener
     * 
     * @param al
     */
    public void removePreAddListener(AddListener al) {
        synchronized (preAddListeners) {
            this.preAddListeners.remove(al);
        }
    }

    /**
     * Add a post-add listener
     * 
     * @param al
     */
    public void removePostAddListener(AddListener al) {
        synchronized (postAddListeners) {
            this.postAddListeners.remove(al);
        }
    }

    /**
     * Add a pre-remove listener
     * 
     * @param rl
     */
    public void removePreRemoveListener(RemoveListener rl) {
        synchronized (preRemoveListeners) {
            this.preRemoveListeners.remove(rl);
        }
    }

    /**
     * Add a post-remove listener
     * 
     * @param rl
     */
    public void removePostRemoveListener(RemoveListener rl) {
        synchronized (postRemoveListeners) {
            this.postRemoveListeners.remove(rl);
        }
    }

    /**
     * Add Listener
     * 
     * Defines the interface that is used to notify on additions
     */
    public static interface AddListener {
        /**
         * Notify the implementer of an add operation
         * 
         * @param rp
         *            the resource pair that was added
         * @throws VizException
         */
        void notifyAdd(ResourcePair rp) throws VizException;
    }

    /**
     * Remove Listener
     * 
     * Defines the interface that is used to notify on removals
     */
    public static interface RemoveListener {
        /**
         * Notify the implementer of a removal
         * 
         * @param rp
         *            the resource pair that was removed
         * @throws VizException
         */
        void notifyRemove(ResourcePair rp) throws VizException;
    }

    private int indexOfRsc(AbstractVizResource<?, ?> rsc) {
        for (int i = 0; i < size(); i++) {
            AbstractVizResource<?, ?> r = get(i).getResource();
            if (r != null) {
                if (r.equals(rsc)) {
                    return i;
                }
            }
        }
        return -1;
    }

    /**
     * This method moves an element within the list. It assumes the requested
     * move is legal (doesn't break any business rules) and valid (both from and
     * to are within the range of the list).
     * 
     * It is the caller's responsibility to ensure these assumptions are true.
     * 
     * @param from
     *            original position
     * @param to
     *            desired position
     */
    private void moveInternal(int from, int to) {
        ResourcePair element = get(from);
        int increment = (to > from ? 1 : -1);
        for (int i = from; i != to; i += increment) {
            set(i, get(i + increment));
        }
        set(to, element);

        cleanup();
    }

    private void cleanup() {
        if (size() < 2) {
            return;
        }
        String prevId;
        int prevOrder;
        ResourcePair current = get(0);
        String currentId = current.getProperties().getRenderingOrderId();
        int currentOrder = current.getProperties().getRenderingOrder();
        ResourceProperties currentProps = current.getProperties();
        currentProps.setRenderingOrder(currentOrder);
        currentProps.setRenderingOrderId(currentId);
        for (int i = 1; i < size(); i++) {
            prevId = currentId;
            prevOrder = currentOrder;

            current = get(i);
            currentId = current.getProperties().getRenderingOrderId();
            currentOrder = current.getProperties().getRenderingOrder();

            currentProps = current.getProperties();
            if (currentOrder < prevOrder) {
                currentProps.setRenderingOrder(prevOrder);
                currentProps.setRenderingOrderId(prevId);
            } else {
                currentProps.setRenderingOrder(currentOrder);
                currentProps.setRenderingOrderId(currentId);
            }
        }
    }

    /**
     * Move a requested resource to a specified location
     * 
     * @param resource
     * @param operation
     */
    public void moveResource(AbstractVizResource<?, ?> resource,
            MoveOperation operation) {
        synchronized (this) {
            // get index of resource to be moved
            int idx = indexOfRsc(resource);

            // if resource is not in the list
            if (idx < 0) {
                throw new IllegalArgumentException("Resource not in list");
            }

            // determine the new index based on the requested operation
            int newIdx = idx;
            switch (operation) {
            case Down:
                if (idx > 0) {
                    do {
                        newIdx--;
                    } while (newIdx >= 0
                            && get(newIdx).getProperties().isSystemResource() == true);
                }
                break;

            case ToBottom:
                newIdx = 0;
                break;

            case Up:
                if (idx < size() - 1) {
                    do {
                        newIdx++;
                    } while (newIdx < size()
                            && get(newIdx).getProperties().isSystemResource() == true);
                }
                break;

            case ToTop:
                newIdx = size() - 1;
                break;

            default:
                throw new UnsupportedOperationException(operation.name()
                        + " is not a supported operation");
            }

            if (newIdx < 0) {
                newIdx = 0;
            }

            if (newIdx > size() - 1) {
                newIdx = size() - 1;
            }

            // ensure newIdx doesn't move above a HIGHEST
            while (newIdx > 0
                    && get(newIdx).getProperties().getRenderingOrder() == HIGHEST) {
                newIdx--;
            }

            // ensure newIdx doesn't move below a LOWEST
            while (newIdx < size()
                    && get(newIdx).getProperties().getRenderingOrder() == LOWEST) {
                newIdx++;
            }

            // if the resource needs to be moved, move it
            if (newIdx != idx) {
                moveInternal(idx, newIdx);
            }
        }
    }

    public boolean containsRsc(AbstractVizResource<?, ?> rsc) {
        if (indexOfRsc(rsc) >= 0) {
            return true;
        }

        return false;
    }

    public boolean removeRsc(AbstractVizResource<?, ?> rsc) {
        synchronized (this) {
            int idx = indexOfRsc(rsc);
            if (idx < 0) {
                return false;
            }

            return (this.remove(idx) != null);
        }
    }

    @Override
    public String toString() {
        StringBuffer s = new StringBuffer("ResourceList[");

        if (size() > 0) {
            s.append("\n\t");
            Iterator<ResourcePair> iter = iterator();
            s.append(iter.next());
            while (iter.hasNext()) {
                s.append(",\n\t");
                s.append(iter.next());
            }
            s.append("\n");
        }

        s.append("]");
        return s.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#finalize()
     */
    @Override
    protected void finalize() throws Throwable {
        super.clear();
    }

    /**
     * Sorts the resource list based on rendering orders
     */
    public void sort() {
        // Ensure the rendering orders are populated properly, and then sort the
        // resource list
        for (ResourcePair rp : this) {
            ResourceProperties props = rp.getProperties();
            int order = props.getRenderingOrder();
            if (order == RenderingOrderFactory.ResourceOrder.NOT_SET.value
                    && rp.getResource() != null) {
                String id = props.getRenderingOrderId();
                if (id == null) {
                    ResourceOrder ro = rp.getResource().getResourceOrder();
                    id = ro.id;
                    order = ro.value;
                } else {
                    order = RenderingOrderFactory.getRenderingOrder(id).value;
                }
                props.setRenderingOrderId(id);
                props.setRenderingOrder(order);
            }
        }

        synchronized (this) {
            List<ResourcePair> copy = new ArrayList<ResourcePair>(this);
            Collections.sort(copy, new RenderingOrderComparator());
            super.clear();
            super.addAll(copy);
        }
    }

    /**
     * Instantiate (realize) all resources from their resource data that have
     * not already been instantiated.
     * 
     * Performs a sort after the instantiation now that all sort orders are
     * available.
     * 
     * @param descriptor
     * @param fireListeners
     */
    public void instantiateResources(IDescriptor descriptor,
            boolean fireListeners) {
        List<ResourcePair> orderedList = null;
        synchronized (resourcesToInstantiate) {
            orderedList = new ArrayList<ResourcePair>(resourcesToInstantiate);
            resourcesToInstantiate.removeAll(orderedList);
        }

        Collections.sort(orderedList, INSTANTIATION_ORDERER);
        if (descriptor.getTimeMatcher() != null) {
            orderedList = new ArrayList<ResourcePair>(descriptor
                    .getTimeMatcher().getResourceLoadOrder(orderedList));
        }

        Iterator<ResourcePair> iterator = orderedList.iterator();
        List<ResourcePair> noTimes = new ArrayList<ResourcePair>();
        List<ResourcePair> successful = new ArrayList<ResourcePair>();
        while (iterator.hasNext()) {
            ResourcePair rp = iterator.next();
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc == null) {
                try {
                    if (rp.instantiateResource(descriptor, fireListeners)) {
                        successful.add(rp);
                    } else {
                        // null is returned when the user selects cancel on
                        // a time amtching dialog. Null should be considered
                        // a normal event. In case of exceptional
                        // circumstances requiring user notification an
                        // exception should be thrown

                        // UFStatus.handle(Priority.PROBLEM,
                        // Activator.PLUGIN_ID,
                        // StatusConstants.CATEGORY_WORKSTATION, null,
                        // "Error instantiating resource from resource data: "
                        // + rp.getResourceData());
                        super.remove(rp);
                        continue;
                    }
                } catch (NoMatchingTimesException e) {
                    noTimes.add(rp);
                    super.remove(rp);
                } catch (NoDataAvailableException e) {
                    super.remove(rp);
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "No data available for resource "
                                            + rp.getResourceData().getClass()
                                                    .getName(), e);

                } catch (IllegalArgumentException e) {
                    super.remove(rp);
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Unable to add invalid resource: "
                                    + e.getLocalizedMessage(), e);
                } catch (VizException e) {
                    statusHandler
                            .handle(Priority.SIGNIFICANT,
                                    "Error instantiating resource, this resource will likely be inoperable",
                                    e);
                }
            } else {
                // set the resource for the properties
                rp.getProperties().setResource(rsc);
            }
        }
        if (!noTimes.isEmpty()) {
            Priority priority = Priority.INFO;
            if (successful.isEmpty()) {
                priority = Priority.PROBLEM;
            }
            StringBuilder message = new StringBuilder("No matching times for ");
            if (noTimes.size() == 1) {
                message.append(noTimes.get(0).getResourceData().getClass()
                        .getSimpleName());
            } else {
                message.append(noTimes.size());
                message.append(" resources");
                for (ResourcePair pair : noTimes) {
                    message.append("\n");
                    message.append(pair.getResourceData().getClass()
                            .getSimpleName());
                }
            }
            statusHandler.handle(priority, message.toString());

        }
        sort();
    }

    private class RenderingOrderComparator implements Comparator<ResourcePair> {

        @Override
        public int compare(ResourcePair o1, ResourcePair o2) {
            Integer i1 = o1.getProperties().getRenderingOrder();
            Integer i2 = o2.getProperties().getRenderingOrder();
            return i1.compareTo(i2);
        }
    }

    /**
     * Get a list of resources that implement or extend the class passed in
     * already casted to the type
     * 
     * @param clazz
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T extends Object> List<T> getResourcesByTypeAsType(Class<T> clazz) {
        List<T> resources = new ArrayList<T>();
        if (clazz != null) {
            for (ResourcePair rp : this) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null && clazz.isInstance(rsc)) {
                    resources.add((T) rsc);
                }
            }
        }
        return resources;
    }

    /**
     * Get a list of resources that implement or extend the class passed in
     * 
     * 
     * @param clazz
     * @return
     */
    public <T extends Object> List<AbstractVizResource<?, ?>> getResourcesByType(
            Class<T> clazz) {
        List<AbstractVizResource<?, ?>> resources = new ArrayList<AbstractVizResource<?, ?>>();
        if (clazz != null) {
            for (ResourcePair rp : this) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                if (rsc != null && clazz.isInstance(rsc)) {
                    resources.add(rsc);
                }
            }
        }
        return resources;
    }

    /**
     * Get the list of resources with the capability class passed in
     * 
     * @param clazz
     * @return
     */
    public <T extends AbstractCapability> List<AbstractVizResource<?, ?>> getResourcesByCapability(
            Class<T> clazz) {
        List<AbstractVizResource<?, ?>> resources = new ArrayList<AbstractVizResource<?, ?>>();
        for (ResourcePair rp : this) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc != null && rsc.hasCapability(clazz)) {
                resources.add(rsc);
            }
        }
        return resources;
    }


}
