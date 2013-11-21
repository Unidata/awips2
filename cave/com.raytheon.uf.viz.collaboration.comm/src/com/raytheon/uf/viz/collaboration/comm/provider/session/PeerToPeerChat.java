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

import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.MessageListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Message.Type;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.viz.collaboration.comm.Activator;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IPeerToPeer;
import com.raytheon.uf.viz.collaboration.comm.identity.IPropertied.Property;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;

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
 * Dec  6, 2013 2561       bclement    removed ECF
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class PeerToPeerChat extends BaseSession implements IPeerToPeer {

    /**
     * 
     * @param container
     * @param externalBus
     * @param manager
     */
    PeerToPeerChat(EventBus externalBus,
            CollaborationConnection manager) throws CollaborationException {
        super(externalBus, manager);
    }

    /**
     * @throws CollaborationException
     * 
     */
    @Override
    public void sendPeerToPeer(IMessage message) throws CollaborationException {
        CollaborationConnection manager = getConnection();
        XMPPConnection conn = manager.getXmppConnection();
        IQualifiedID to = message.getTo();
        String toId = to.getFQName();
        Message xmppMessage = new Message(toId, Type.chat);
        xmppMessage.setBody(message.getBody());
        for (Property p : message.getProperties()) {
            xmppMessage.setProperty(p.getKey(), p.getValue());
        }
        xmppMessage.setSubject(message.getSubject());
        synchronized (this) {
            Activator.getDefault().getNetworkStats()
                    .log(Activator.PEER_TO_PEER, message.getBody().length(), 0);
            // TODO this is how ECF sent messages, we should look into the
            // side-effects of creating an empty message listener every time we
            // send a message. Alternative would be to redesign around keeping
            // track of the created chat
            Chat chat = conn.getChatManager().createChat(toId,
                    new MessageListener() {
                        public void processMessage(Chat chat, Message message) {
                        }
                    });
            try {
                chat.sendMessage(xmppMessage);
            } catch (XMPPException e) {
                throw new CollaborationException("Unable to send message to: "
                        + toId, e);
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
    public void sendPeerToPeer(IQualifiedID to, String message)
            throws CollaborationException {
        TextMessage msg = new TextMessage(to, message);
        this.sendPeerToPeer(msg);
    }

}
