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
package com.raytheon.uf.viz.collaboration.comm.provider.session;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Base class for chat and collaboration sessions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class BaseSession implements ISession {

    protected final String sessionId;

    private EventBus managerEventBus;

    private EventBus eventBus;

    private Map<Object, Object> eventSubscribers;

    private CollaborationConnection connection;

    /**
     * 
     * @param container
     * @param externalBus
     * @param manager
     */
    protected BaseSession(EventBus externalBus,
            CollaborationConnection manager) throws CollaborationException {
        this(externalBus, manager, UUID.randomUUID().toString());
    }

    /**
     * 
     * @param container
     * @param externalBus
     * @param manager
     * @param sessionId
     */
    protected BaseSession(EventBus externalBus,
            CollaborationConnection manager, String sessionId)
            throws CollaborationException {
        // Set the session identifier.
        this.sessionId = sessionId;
        managerEventBus = externalBus;
        eventBus = new EventBus();
        connection = manager;
        eventSubscribers = new HashMap<Object, Object>();
    }

    /**
     * Get access to the peer to peer session instance.
     * 
     * @return The peer to peer chat session instance.
     * @throws CollaborationException
     */
    protected PeerToPeerChat getP2PSession() throws CollaborationException {
        return (PeerToPeerChat) connection.getPeerToPeerSession();
    }

    /**
     * 
     * @return
     */
    protected EventBus getManagerEventPublisher() {
        return managerEventBus;
    }

    /**
     * 
     * @return
     */
    protected CollaborationConnection getSessionManager() {
        return connection;
    }

    // *****************
    // Implement IEventPublisher methods
    // *****************

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#getUserID()
     */
    @Override
    public UserId getUserID() {
        return connection.getUser();
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#isConnected()
     */
    @Override
    public boolean isConnected() {
        return connection.isConnected();
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#close()
     */
    @Override
    public void close() {

        // Unregister any handlers added using this session
        // for(Object o : eventSubscribers.values()) {
        // managerEventBus.unregister(o);
        // }
        connection.removeSession(this);
    }

    /**
     * Get the session identifier.
     * 
     * @return The session id for this session.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#getSessionId()
     */
    @Override
    public String getSessionId() {
        return sessionId;
    }

    // *****************
    // Implement IEventPublisher methods
    // *****************

    /**
     * 
     * @param handler
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher#registerEventHandler(java.lang.Object)
     */
    @Override
    public void registerEventHandler(Object handler) {
        if (!eventSubscribers.containsKey(handler)) {
            eventBus.register(handler);
            eventSubscribers.put(handler, handler);
        }
    }

    /**
     * 
     * @param handler
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher#unregisterEventHandler(java.lang.Object)
     */
    @Override
    public void unregisterEventHandler(Object handler) {
        eventSubscribers.remove(handler);
        eventBus.unregister(handler);
    }

    /**
     * 
     */
    @Override
    public void postEvent(Object event) {
        if (event != null) {
            eventBus.post(event);
        }
    }

    @Override
    public CollaborationConnection getConnection() {
        return connection;
    }

}
