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

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.IQ;
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
 * Feb 18, 2014 2751      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PubSubOperations {

    private PubSubOperations() {
    }

    /**
     * Send packet to change affiliation of user on a pubsub topic node
     * 
     * @param conn
     * @param affiliation
     * @throws XMPPException
     */
    public static void sendAffiliationPacket(XMPPConnection conn,
            ChangeAffiliationExtension affiliation) throws XMPPException {
        PubSub packet = new PubSub();
        packet.setType(IQ.Type.SET);
        packet.setTo("pubsub." + conn.getServiceName());
        packet.setPubSubNamespace(PubSubNamespace.OWNER);
        packet.addExtension(affiliation);
        SyncPacketSend.getReply(conn, packet);
    }

}
