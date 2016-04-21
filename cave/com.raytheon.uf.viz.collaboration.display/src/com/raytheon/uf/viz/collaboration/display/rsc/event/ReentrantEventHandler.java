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
package com.raytheon.uf.viz.collaboration.display.rsc.event;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.display.Activator;

/**
 * An implementation of EventBus that posts all events to listeners immediately.
 * The major difference between this implementation and the default
 * implementation is that it allows reentrant posts, that is if you call post
 * from within a handler it will immediately post the event while the default
 * implementation will add the new event to a queue. Because EventBus is not
 * designed for extension this is basically a reimplementation from scratch
 * using the EventBus as an interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ReentrantEventHandler extends EventBus {

    private static class EventHandler {

        public final Method method;

        public final Object obj;

        public EventHandler(Method method, Object obj) {
            this.method = method;
            this.obj = obj;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((method == null) ? 0 : method.hashCode());
            result = prime * result + ((obj == null) ? 0 : obj.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            EventHandler other = (EventHandler) obj;
            if (method == null) {
                if (other.method != null)
                    return false;
            } else if (!method.equals(other.method))
                return false;
            // If obj.equals(other.obj) we still want the two ahndlers to be
            // treated as different so we are only equal if obj == other.obj.
            if (this.obj != other.obj)
                return false;
            return true;
        }

    }

    private Map<Class<?>, Set<EventHandler>> handlers = new HashMap<Class<?>, Set<EventHandler>>();

    @Override
    public void post(Object obj) {
        for (Class<?> clazz : getClassHierarchy(obj.getClass())) {
            Set<EventHandler> handlers = this.handlers.get(clazz);
            if (handlers == null) {
                return;
            }
            for (EventHandler handler : handlers) {
                try {
                    handler.method.invoke(handler.obj, new Object[] { obj });
                } catch (IllegalAccessException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } catch (InvocationTargetException e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
    }

    private List<Class<?>> getClassHierarchy(Class<?> clazz) {
        List<Class<?>> result = new ArrayList<Class<?>>();
        while (clazz != null) {
            result.add(clazz);
            result.addAll(Arrays.asList(clazz.getInterfaces()));
            clazz = clazz.getSuperclass();
        }
        return result;
    }

    @Override
    public void register(Object object) {
        Class<?> objClazz = object.getClass();
        for (Class<?> clazz : getClassHierarchy(objClazz)) {
            for (Method method : clazz.getMethods()) {
                if (method.isAnnotationPresent(Subscribe.class)) {
                    Class<?>[] types = method.getParameterTypes();
                    if (types.length != 1) {
                        throw new IllegalStateException(method.getName()
                                + " has the wrong number of arguments");
                    }
                    Class<?> type = types[0];
                    Set<EventHandler> handlers = this.handlers.get(type);
                    if (handlers == null) {
                        handlers = new HashSet<EventHandler>();
                        this.handlers.put(type, handlers);
                    }
                    try {
                        // get the actual method from the obj so that if there
                        // are any overrides they are handled correctly.
                        Method objMethod = objClazz.getMethod(method.getName(),
                                method.getParameterTypes());
                        handlers.add(new EventHandler(objMethod, object));
                    } catch (NoSuchMethodException e) {
                        // This method will exist, this exception will never
                        // happen.
                        Activator.statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        }
    }

    @Override
    public void unregister(Object object) {
        // This is a brute force algorithm, more efficient algorithms could be
        // written but are currently not needed
        boolean removed = false;
        Iterator<Entry<Class<?>, Set<EventHandler>>> entryItr = handlers
                .entrySet().iterator();
        while (entryItr.hasNext()) {
            Set<EventHandler> handlers = entryItr.next().getValue();
            Iterator<EventHandler> handlerItr = handlers.iterator();
            while (handlerItr.hasNext()) {
                if (handlerItr.next().obj == object) {
                    handlerItr.remove();
                    removed = true;
                }
            }
            if (handlers.isEmpty()) {
                entryItr.remove();
            }
        }
        if (!removed) {
            throw new IllegalArgumentException("Object was never registered: "
                    + object);
        }
    }

}
