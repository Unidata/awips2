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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.Namespace;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ecf.presence.im.IChatMessage;
import org.eclipse.ecf.presence.im.IChatMessageSender;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IPeerToPeer;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IPropertied.Property;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IEventPublisher;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageFilter;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IPresenceListener;
import com.raytheon.uf.viz.collaboration.comm.provider.Errors;

/**
 * 
 * 
 * Only one instance of this class should be created.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class PeerToPeerChat extends BaseSession implements IPeerToPeer,
        IEventPublisher {

    /**
     * 
     * TODO Add Description
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Feb 27, 2012            jkorman     Initial creation
     * 
     * </pre>
     * 
     * @author jkorman
     * @version 1.0
     */
    private static class InternalListener {

        private IMessageListener messageListener;

        private IPresenceListener presenceListener;

        private IMessageFilter filter;

        /**
         * 
         * @param listener
         * @param filter
         */
        public InternalListener(IMessageListener listener, IMessageFilter filter) {
            messageListener = listener;
            this.filter = filter;

        }

        /**
         * 
         * @param listener
         * @param filter
         */
        public InternalListener(IPresenceListener listener,
                IMessageFilter filter) {
            presenceListener = listener;
            this.filter = filter;
        }

        /**
         * 
         * @param message
         */
        public void processMessage(IMessage message) {
            messageListener.processMessage(message);
        }

        /**
         * 
         * @param presence
         */
        public void processPresence(IPresence presence) {
            presenceListener.notifyPresence(presence);
        }

        /**
         * 
         * @param message
         * @return
         */
        public boolean filter(IMessage message) {
            return filter.filter(message);
        }
    }

    private List<InternalListener> messageListeners = null;

    private Namespace namespace = null;

    private IChatMessageSender chatSender = null;

    /**
     * 
     * @param container
     * @param externalBus
     * @param manager
     */
    PeerToPeerChat(IContainer container, EventBus externalBus,
            SessionManager manager) {
        super(container, externalBus, manager);
        try {
            setup();
        } catch (ECFException e) {
            // TODO
            e.printStackTrace();
        }
        chatSender = getConnectionPresenceAdapter().getChatManager()
                .getChatMessageSender();

    }

    /**
     * 
     */
    @Override
    public int sendPeerToPeer(IMessage message) {
        // Assume success
        int status = Errors.NO_ERROR;
        if (chatSender != null) {
            ID toID = createID(message.getTo().getName());
            String subject = message.getSubject();
            String body = message.getBody();
            Collection<Property> properties = message.getProperties();
            Map<String, String> props = null;
            if ((properties != null) && (properties.size() > 0)) {
                props = new HashMap<String, String>();
                for (Property p : properties) {
                    props.put(p.getKey(), p.getValue());
                }
            }
            try {
                chatSender.sendChatMessage(toID, null, IChatMessage.Type.CHAT,
                        subject, body, props);
            } catch (ECFException e) {
                System.out.println("Error sending message");
                e.printStackTrace();
            }
        }
        return status;
    }

    /**
     * Send a message to the named recipient.
     * 
     * @param to
     *            The recipient of the message.
     * @param message
     *            The body of the message to send.
     */
    @Override
    public int sendPeerToPeer(String to, String message) {
        // Assume success
        int status = Errors.NO_ERROR;
        ID toID = createID(to);
        try {
            chatSender.sendChatMessage(toID, message);
        } catch (ECFException e) {
            System.out.println("Error sending message");
            e.printStackTrace();
        }

        return status;
    }

    /**
     * 
     */
    @Override
    public IMessageListener addMessageListener(IMessageListener listener,
            IMessageFilter filter) {
        InternalListener messageListener = new InternalListener(listener,
                filter);
        messageListeners.add(messageListener);
        return listener;
    }

    /**
     * 
     */
    @Override
    public Collection<IMessageListener> getMessageListeners() {
        Collection<IMessageListener> listeners = new ArrayList<IMessageListener>();
        synchronized (messageListeners) {
            for (InternalListener intListener : messageListeners) {
                listeners.add(intListener.messageListener);
            }
        }
        return listeners;
    }

    /**
     * 
     */
    @Override
    public IMessageListener removeMessageListener(IMessageListener listener) {
        IMessageListener removed = null;
        if (messageListeners.remove(listener)) {
            removed = listener;
        }
        return removed;
    }

}
