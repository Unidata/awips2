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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.ecf.core.IContainer;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ecf.presence.im.IChatMessage;
import org.eclipse.ecf.presence.im.IChatMessageSender;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IPeerToPeer;
import com.raytheon.uf.viz.collaboration.comm.identity.IPropertied.Property;

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
 * Apr 18, 2012            njensen      Cleanup
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class PeerToPeerChat extends BaseSession implements IPeerToPeer {

    private IChatMessageSender chatSender = null;

    /**
     * 
     * @param container
     * @param externalBus
     * @param manager
     */
    PeerToPeerChat(IContainer container, EventBus externalBus,
            CollaborationConnection manager) throws CollaborationException {
        super(container, externalBus, manager);
        chatSender = getConnectionPresenceAdapter().getChatManager()
                .getChatMessageSender();
    }

    /**
     * @throws CollaborationException
     * 
     */
    @Override
    public void sendPeerToPeer(IMessage message) throws CollaborationException {
        if (chatSender != null) {
            ID toID = createID(message.getTo().getFQName());
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
                throw new CollaborationException(
                        "Error sending message to peer "
                                + message.getTo().getName(), e);
            }
        }
    }

    /**
     * Send a message to the named recipient.
     * 
     * @param to
     *            The recipient of the message.
     * @param message
     *            The body of the message to send.
     * @throws CollaborationException
     */
    @Override
    public void sendPeerToPeer(String to, String message)
            throws CollaborationException {
        ID toID = createID(to);
        try {
            chatSender.sendChatMessage(toID, message);
        } catch (ECFException e) {
            throw new CollaborationException("Error sending message to peer "
                    + to, e);
        }
    }

    /**
     * 
     * @return
     */
    @Override
    public EventBus getEventPublisher() {
        return getManagerEventPublisher();
    }

}
