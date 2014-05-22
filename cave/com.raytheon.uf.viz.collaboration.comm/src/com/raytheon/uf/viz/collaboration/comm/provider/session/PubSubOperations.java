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

import java.util.List;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.IQ;
import org.jivesoftware.smack.packet.IQ.Type;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smackx.pubsub.Node;
import org.jivesoftware.smackx.pubsub.NodeExtension;
import org.jivesoftware.smackx.pubsub.PubSubElementType;
import org.jivesoftware.smackx.pubsub.Subscription;
import org.jivesoftware.smackx.pubsub.SubscriptionsExtension;
import org.jivesoftware.smackx.pubsub.packet.PubSub;
import org.jivesoftware.smackx.pubsub.packet.PubSubNamespace;
import org.jivesoftware.smackx.pubsub.packet.SyncPacketSend;

import com.raytheon.uf.common.xmpp.ext.ChangeAffiliationExtension;

/**
 * PubSub operations not implemented by the Smack PubSub manager. This includes
 * operations to modify ownership of a topic node.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2014 2751       bclement     Initial creation
 * Apr 15, 2014 2822       bclement     added getAllSubscriptions()
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PubSubOperations {

    public static final String PUBSUB_SUBDOMAIN_PREFIX = "pubsub.";

    private PubSubOperations() {
    }

    /**
     * Send packet to change affiliation of user on a pubsub topic node. Calling
     * client must be owner of node.
     * 
     * @param conn
     * @param affiliation
     * @throws XMPPException
     */
    public static void sendAffiliationPacket(XMPPConnection conn,
            ChangeAffiliationExtension affiliation) throws XMPPException {
        PubSub packet = createOwnerPacket(conn, affiliation, IQ.Type.SET);
        SyncPacketSend.getReply(conn, packet);
    }

    /**
     * List all subscriptions on node. Calling client must be owner of node.
     * 
     * @param conn
     * @param n
     * @return
     * @throws XMPPException
     */
    public static List<Subscription> getAllSubscriptions(XMPPConnection conn,
            Node n) throws XMPPException {
        PubSubElementType type = PubSubElementType.SUBSCRIPTIONS;
        /*
         * we need to use the OWNER namespace when we make the request, but we
         * reuse the provider (parser) for the default namespace for the return.
         * Use the default namespace to get the extension object from the packet
         */
        String namespace = type.getNamespace().getXmlns();
        NodeExtension ext = new NodeExtension(type, n.getId());
        PubSub packet = createOwnerPacket(conn, ext, Type.GET);
        Packet reply = SyncPacketSend.getReply(conn, packet);
        SubscriptionsExtension resp = (SubscriptionsExtension) reply
                .getExtension(type.getElementName(), namespace);
        if (resp == null){
            throw new XMPPException(
                    "Subscriptions response missing content for topic: "
                            + n.getId());
        }
        return resp.getSubscriptions();
    }

    /**
     * Create pubsub packet object with owner namespace
     * 
     * @param conn
     * @param ext
     * @param type
     * @return
     */
    private static PubSub createOwnerPacket(XMPPConnection conn,
            NodeExtension ext, Type type) {
        PubSub packet = new PubSub();
        packet.setType(type);
        packet.setTo(PUBSUB_SUBDOMAIN_PREFIX + conn.getServiceName());
        packet.setPubSubNamespace(PubSubNamespace.OWNER);
        packet.addExtension(ext);
        return packet;
    }

}
