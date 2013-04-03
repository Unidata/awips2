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
package com.raytheon.uf.common.util.session;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.ReflectionUtil;

/**
 * Provides the functionality to manage a 'session'. Each thread has its own
 * instance of the context. 1..N open requests can be made, and once the Nth
 * close request comes in the session is closed. All data related to the session
 * is stored in the sub-class of {@link SessionContext}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 26, 2012 1195       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class SessionManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SessionManager.class);

    /**
     * Inner-class used to hold the SessionContext tracking data.
     */
    private static class SessionContextTracker {
        private int openRequests;
        private final SessionContext sessionContext;

        /**
         * Constructor.
         * 
         * @param sessionContext
         *            the session context to track
         */
        private SessionContextTracker(SessionContext sessionContext) {
            this.sessionContext = sessionContext;
        }
    }

    /**
     * Kept in a ThreadLocal to avoid synchronization expenditures.
     */
    private static final ThreadLocal<Map<String, SessionContextTracker>> context = new ThreadLocal<Map<String, SessionContextTracker>>() {

        /**
         * {@inheritDoc}
         */
        @Override
        protected Map<String, SessionContextTracker> initialValue() {
            return new HashMap<String, SessionContextTracker>();
        }
    };

    /**
     * Opens a session and binds the context to a thread, if not already open.
     * 
     * @param contextClass
     *            the context class for the session type
     */
    public static <T extends SessionContext> T openSession(
            Class<T> contextClass) {
        final Map<String, SessionContextTracker> map = context.get();
        final String key = contextClass.getName();

        SessionContextTracker ctxTracker = map.get(key);
        if (ctxTracker == null) {
            SessionContext ctx = ReflectionUtil.newInstanceOfAssignableType(
                    SessionContext.class, contextClass);
            ctx.open();

            ctxTracker = new SessionContextTracker(ctx);
            map.put(key, ctxTracker);
        }
        
        ctxTracker.openRequests++;
        
        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug(String.format(
                    " context [%s] openRequests [%s]", contextClass.getName(),
                    ctxTracker.openRequests));
        }

        return contextClass.cast(ctxTracker.sessionContext);
    }

    /**
     * Closes the session currently bound to this thread, if the last open
     * requester has signaled it should be closed.
     * 
     * @param contextClass
     *            the context class for the session type
     * @throws IllegalStateException
     *             if the session is not open
     */
    public static <T extends SessionContext> void closeSession(
            Class<T> contextClass) {
        final Map<String, SessionContextTracker> map = context.get();
        final String key = contextClass.getName();

        SessionContextTracker ctxTracker = map.get(key);

        if (ctxTracker == null) {
            throw new IllegalStateException(
                    "Unable to close a session that is not opened!  "
                            + "Please be sure to pair the closeSession() request with a prior openSession() request.");
        }

        ctxTracker.openRequests--;

        // If the last open request finally checked in, close it down
        if (ctxTracker.openRequests < 1) {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug(String.format(
                        "context [%s] closing, openRequests [%s]",
                        contextClass.getName(), ctxTracker.openRequests));
            }

            SessionContext ctx = ctxTracker.sessionContext;
            ctx.close();
            map.remove(key);
        } else if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug(String.format(
                    "context [%s] not closing, openRequests [%s]",
                    contextClass.getName(), ctxTracker.openRequests));
        }
    }

    /**
     * Retrieve the session context class for an open session.
     * 
     * @param contextClass
     *            the context class for the session type
     * @return
     * @throws IllegalStateException
     *             if the session is not open
     */
    public static <T extends SessionContext> T getSessionContext(
            Class<T> contextClass) {
        final Map<String, SessionContextTracker> map = context.get();
        final String key = contextClass.getName();

        SessionContextTracker ctxTracker = map.get(key);
        if (ctxTracker == null) {
            throw new IllegalStateException("No session is currently open!");
        }

        return contextClass.cast(ctxTracker.sessionContext);
    }
}
