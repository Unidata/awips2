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
package com.raytheon.openfire.plugin.configuration.collaboration.iq;

import java.util.Arrays;

import org.apache.commons.lang.StringUtils;
import org.dom4j.Element;
import org.jivesoftware.openfire.IQHandlerInfo;
import org.jivesoftware.openfire.XMPPServer;
import org.jivesoftware.openfire.auth.UnauthorizedException;
import org.jivesoftware.openfire.pubsub.Node;
import org.jivesoftware.openfire.pubsub.PubSubModule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.PacketError;
import org.xmpp.packet.PacketError.Condition;

/**
 * IQ handle for dataserver authorization and authentication. Stores client
 * public keys and authorizes key use for pubsub nodes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2014 2756       bclement     Initial creation
 * Feb 28, 2014 2756       bclement     if sessionid is not provided, skip authorization step
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DataAuthHandler extends AbstractConfigHandler {

    public static final String AUTH_QUERY_XMLNS = "urn:uf:viz:collaboration:iq:auth";

    public static final String INFO_ELEMENT_NAME = "authinfo";

    public static final String PUBKEY_ELEMENT_NAME = "publickey";

    public static final String ALG_ATTRIBUTE = "algorithm";

    public static final String JID_ATTRIBUTE = "jid";

    public static final String SESSIONID_ATTRIBUTE = "sessionid";

    private static final Logger log = LoggerFactory
            .getLogger(DataAuthHandler.class);

    private final PubSubModule pubsub;

    /**
     * @param moduleName
     */
    public DataAuthHandler() {
        super("Collaboration Auth Query Handler", Arrays
                .asList(AUTH_QUERY_XMLNS), new IQHandlerInfo(
                QUERY_ELEMENT_NAME, AUTH_QUERY_XMLNS));
        this.pubsub = XMPPServer.getInstance().getPubSubModule();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.openfire.plugin.configuration.collaboration.iq.
     * AbstractConfigHandler#handleGet(org.xmpp.packet.IQ)
     */
    @Override
    protected IQ handleGet(IQ packet) throws UnauthorizedException {
        JID from = packet.getFrom();
        String fromBare = from.toBareJID();
        Element queryElem = packet.getChildElement();
        String jid = queryElem.attributeValue(JID_ATTRIBUTE);
        if (jid != null) {
            if (!jid.equals(fromBare) && !isDataServerUser(from)) {
                log.debug("User not authorized: " + from.toFullJID());
                throw new UnauthorizedException(
                        "User not authorized for service: "
                                + from.toFullJID());
            }
        } else {
            jid = fromBare;
        }
        JID target = new JID(jid);
        String sessionId = queryElem.attributeValue(SESSIONID_ATTRIBUTE);
        if (!StringUtils.isBlank(sessionId)) {
            /*
             * only check authorization if session id is specified, otherwise we
             * are just doing authentication
             */
            Node node = pubsub.getNode(sessionId);
            if (node == null) {
                String msg = "No topic found for session: " + sessionId;
                log.debug(msg);
                return createError(packet, new PacketError(
                        Condition.not_allowed, PacketError.Type.cancel, msg));
            }
            if (!isOwner(jid, node)) {
                String msg = "User '" + jid + "' is not an owner of session '"
                        + sessionId + "'";
                log.debug(msg);
                return createError(packet, new PacketError(
                        Condition.not_allowed, PacketError.Type.cancel, msg));
            }
        }
        return retrieve(packet, target.getNode(),
                createElement(INFO_ELEMENT_NAME, COLLAB_XMLNS));
    }


    /**
     * @param jid
     * @param node
     * @return true if user with jid is owner of node
     */
    private boolean isOwner(String jid, Node node) {
        for (JID owner : node.getOwners()) {
            if (jid.equals(owner.toBareJID())) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.openfire.plugin.configuration.collaboration.iq.
     * AbstractConfigHandler#handleSet(org.xmpp.packet.IQ)
     */
    @Override
    protected IQ handleSet(IQ packet) {
        Element queryElem = packet.getChildElement();
        Element infoElem;
        Element pubkeyElem;
        try {
            infoElem = getChildElement(queryElem, INFO_ELEMENT_NAME,
                    COLLAB_XMLNS);
            pubkeyElem = getChildElement(infoElem, PUBKEY_ELEMENT_NAME,
                    COLLAB_XMLNS);
        } catch (Exception e) {
            log.debug("Missing Required Element in packet: " + packet.toXML(),
                    e);
            return createError(packet, new PacketError(Condition.bad_request,
                    PacketError.Type.modify, e.getLocalizedMessage()));
        }
        String pubKey = pubkeyElem.getText();
        if (StringUtils.isBlank(pubKey)) {
            log.debug("Missing Public Key Text");
            return createError(packet, new PacketError(Condition.bad_request,
                    PacketError.Type.modify, "Missing Required Text Body"));
        }
        if (StringUtils.isBlank(pubkeyElem.attributeValue(ALG_ATTRIBUTE))) {
            String msg = "Missing attribute: " + ALG_ATTRIBUTE;
            return createError(packet, new PacketError(Condition.bad_request,
                    PacketError.Type.modify, msg));
        }
        store(packet.getFrom().getNode(), infoElem);
        return IQ.createResultIQ(packet);
    }

}
