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
package com.raytheon.uf.viz.collaboration.ui.rsc.rendering;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.CollaborationObjectEventStorage;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.IObjectEventRetrieval;
import com.raytheon.uf.viz.collaboration.ui.role.dataprovider.event.IPersistedEvent;
import com.raytheon.uf.viz.collaboration.ui.rsc.CollaborationResource;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;

/**
 * Collaboration rendering data manager, manages render data and render handlers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CollaborationRenderingDataManager implements IObjectEventRetrieval {

    private static final String RENDERING_EXTENSION = "com.raytheon.uf.viz.collaboration.ui.renderingExtension";

    private static Collection<IConfigurationElement> handlerElements = new LinkedList<IConfigurationElement>();

    static {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        if (registry != null) {
            IExtensionPoint point = registry
                    .getExtensionPoint(RENDERING_EXTENSION);
            if (point != null) {
                IExtension[] extensions = point.getExtensions();

                for (IExtension ext : extensions) {
                    IConfigurationElement[] config = ext
                            .getConfigurationElements();
                    handlerElements.addAll(Arrays.asList(config));
                }
            }
        }
    }

    private Map<Integer, Object[]> renderableObjectMap = new HashMap<Integer, Object[]>();

    private IGraphicsTarget target;

    private PaintProperties paintProps;

    private EventBus disposerRouter;

    private IObjectEventRetrieval retrieval;

    private CollaborationResource resource;

    public CollaborationRenderingDataManager(ISharedDisplaySession session,
            CollaborationResource resource) {
        this.resource = resource;
        this.disposerRouter = new EventBus();
        this.retrieval = CollaborationObjectEventStorage
                .createRetrievalObject(session);
    }

    /**
     * @param target
     * @param paintProps
     */
    public void beginRender(IGraphicsTarget target, PaintProperties paintProps) {
        this.target = target;
        this.paintProps = paintProps;
    }

    /**
     * @return the target
     */
    public IGraphicsTarget getTarget() {
        return target;
    }

    /**
     * @return the paintProperties
     */
    public PaintProperties getPaintProperties() {
        return paintProps;
    }

    /**
     * Put a renderable object in the object map. If an object already exists
     * for that id, it will be sent to the disposer
     * 
     * @param objectId
     * @param obj
     */
    public void putRenderableObject(int objectId, Object obj) {
        if (obj != null) {
            Object[] objects = null;
            if (obj.getClass().isArray()) {
                objects = (Object[]) obj;
            } else {
                objects = new Object[] { obj };
            }
            Object[] oldValue = renderableObjectMap.put(objectId, objects);
            if (oldValue != null) {
                dispose(oldValue);
            }
        }
    }

    /**
     * Get a renderable object out of the object map as the objectType, if no
     * object exists or the object is not of type objectType, null is returned.
     * Use Object[].class as the objectType if you want all objects bound to the
     * id. The first object in the array that is of type objectType will be
     * returned otherwise
     * 
     * @param <T>
     * @param objectId
     * @param objectType
     * @return
     */
    public <T> T getRenderableObject(int objectId, Class<T> objectType) {
        return getRenderableObject(objectId, objectType, true);
    }

    public <T> T getRenderableObject(int objectId, Class<T> objectType,
            boolean retrieve) {
        T obj = null;
        Object[] toCheck = renderableObjectMap.get(objectId);
        if (toCheck != null) {
            if (objectType == Object[].class) {
                obj = objectType.cast(toCheck);
            } else {
                for (Object check : toCheck) {
                    if (objectType.isInstance(check)) {
                        obj = objectType.cast(check);
                        break;
                    }
                }
            }
        } else if (retrieve) {
            try {
                resource.lockObject(objectId);
                AbstractDispatchingObjectEvent[] events = retrieveObjectEvents(objectId);
                for (AbstractDispatchingObjectEvent event : events) {
                    resource.postObjectEvent(event);
                }
                return getRenderableObject(objectId, objectType, false);
            } catch (CollaborationException e) {
                Activator.statusHandler.handle(
                        Priority.PROBLEM,
                        "Error retrieving object events: "
                                + e.getLocalizedMessage(), e);
            } finally {
                resource.unlockObject(objectId);
            }
        }
        return obj;
    }

    /**
     * Disposes all renderable data for the object
     * 
     * @param objectId
     */
    public void dispose(int objectId) {
        Object[] objects = renderableObjectMap.remove(objectId);
        if (objects != null) {
            dispose(objects);
        }
    }

    /**
     * Dispose all renderable object data
     */
    public void dispose() {
        for (Object[] obj : renderableObjectMap.values()) {
            dispose(obj);
        }
        renderableObjectMap.clear();
    }

    /**
     * Disposes a single renderable object by sending it to the disposer
     * EventBus
     * 
     * @param obj
     */
    public void dispose(Object[] objects) {
        for (Object toDispose : objects) {
            disposerRouter.post(toDispose);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.event.IObjectEventRetrieval
     * #retrieveEvent
     * (com.raytheon.uf.viz.collaboration.ui.role.event.IPersistedEvent)
     */
    @Override
    public AbstractDispatchingObjectEvent retrieveEvent(IPersistedEvent event)
            throws CollaborationException {
        return retrieval.retrieveEvent(event);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.event.IObjectEventRetrieval
     * #retrieveObjectEvents(int)
     */
    @Override
    public AbstractDispatchingObjectEvent[] retrieveObjectEvents(int objectId)
            throws CollaborationException {
        return retrieval.retrieveObjectEvents(objectId);
    }

    /**
     * @param dataManager
     * @return
     */
    public static synchronized Collection<CollaborationRenderingHandler> createRenderingHandlers(
            CollaborationRenderingDataManager dataManager) {
        List<CollaborationRenderingHandler> handlers = new ArrayList<CollaborationRenderingHandler>(
                handlerElements.size());
        for (IConfigurationElement element : handlerElements) {
            try {
                CollaborationRenderingHandler handler = (CollaborationRenderingHandler) element
                        .createExecutableExtension("class");
                handler.setDataManager(dataManager);
                handlers.add(handler);
                dataManager.disposerRouter.register(handler);
            } catch (CoreException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
        return handlers;
    }
}
