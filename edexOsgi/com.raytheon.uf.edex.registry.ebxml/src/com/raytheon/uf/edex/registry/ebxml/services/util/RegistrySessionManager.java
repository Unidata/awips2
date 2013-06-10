package com.raytheon.uf.edex.registry.ebxml.services.util;

import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.util.session.SessionContextFactory;
import com.raytheon.uf.common.util.session.SessionManager;

/**
 * Manage individual interactions with the Registry to manage hiberate
 * transactions and event framework notifications.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            jspinks     Initial creation
 * Sep 27, 2012 1187       djohnson    Split implementation between {@link SessionManager} and {@link RegistrySessionContext}.
 * Feb 07, 2013 1543       djohnson    Use SessionContextFactory instead of context class directly.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public class RegistrySessionManager {

    /**
     * {@link SessionContextFactory} for {@link RegistrySessionContext}
     * instances.
     */
    private static final SessionContextFactory<RegistrySessionContext> REGISTRY_SESSION_CONTEXT_FACTORY = new SessionContextFactory<RegistrySessionContext>() {
        /**
         * {@inheritDoc}
         */
        @Override
        public RegistrySessionContext getSessionContext() {
            return new RegistrySessionContext();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Class<RegistrySessionContext> getSessionContextClass() {
            return RegistrySessionContext.class;
        }
    };

    /**
     * Opens a Hibernate transaction and binds it to a thread
     */
    public static void openSession() {
        SessionManager.openSession(REGISTRY_SESSION_CONTEXT_FACTORY);
    }

    /**
     * Closes the transaction currently bound to this thread and sends any
     * events captured during this RegistrySession to the event framework for
     * processing.
     */
    public static void closeSession() {
        SessionManager.closeSession(REGISTRY_SESSION_CONTEXT_FACTORY);
    }

    /**
     * Add the posting of a event to the current, active Session. Any attempt to
     * add a event to an inactive Session will result in an
     * IllegalStateException.
     * 
     * @param event
     *            The event Object to post when the transaction successfully
     *            completes.
     * 
     * @throws IllegalStateException
     *             If an attempt is made to add an event to an inactive Session.
     */
    public static void postEvent(Event event) throws IllegalStateException {
        RegistrySessionContext ctx = SessionManager
                .getSessionContext(REGISTRY_SESSION_CONTEXT_FACTORY);
        ctx.postEvent(event);
    }
}
