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
package com.raytheon.uf.viz.collaboration.comm.provider.connection;

import java.net.URI;
import java.net.URL;

import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.XMPPError;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.xmpp.PacketConstants;
import com.raytheon.uf.viz.collaboration.comm.Activator;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IHttpXmppMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IHttpdCollaborationConfigurationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IUser;
import com.raytheon.uf.viz.collaboration.comm.packet.SessionPayload;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.ChatMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.HttpdCollaborationConfigurationEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Listens for peer to peer messages and routes them appropriately.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jkorman     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * Dec 18, 2013 2562       bclement    added timeout for HTTP config,
 *                                     data now in packet extension
 * Dec 19, 2013 2563       bclement    removed wait for HTTP config, added reset
 * Feb 13, 2014 2751       bclement    changed IQualifiedID objects to IUser
 * Feb 17, 2014 2756       bclement    null check for message from field
 *                                      moved url validation from regex to java utility
 * Feb 24, 2014 2756       bclement    moved xmpp objects to new packages
 * Apr 14, 2014 2903       bclement    moved from session subpackage to connection
 * Jun 17, 2014 3078       bclement    routing for private chat messages
 * Jun 17, 2014 3078       bclement    only accept config from server, don't route data without sessionId
 * Jun 20, 2014 3281       bclement    refactored processPacket(), added chat error handling
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class PeerToPeerCommHelper implements PacketListener {

    private static final transient IUFStatusHandler statusHandler = CollaborationConnection
            .getStatusHandler();

    private static volatile String httpServer;

    /**
     * Get HTTP server address. This value will be updated if the server sends
     * new HTTP configuration. If this address is null, the server most likely
     * doesn't support collaborative displays.
     * 
     * @return
     */
    public static String getCollaborationHttpServer() {
        return httpServer;
    }

    private final CollaborationConnection manager;

    /**
     * 
     * @param manager
     * @param presenceAdapter
     */
    protected PeerToPeerCommHelper(CollaborationConnection manager) {
        this.manager = manager;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.smack.PacketListener#processPacket(org.jivesoftware.
     * smack.packet.Packet)
     */
    @Override
    public void processPacket(Packet packet) {
        if (packet instanceof Message) {
            Message msg = (Message) packet;
            String fromStr = msg.getFrom();

            if (fromStr == null) {
                /*
                 * Messages coming from the server will either have a null
                 * 'from' address or it will be the host name of the server.
                 * Normalize so that it is always just the name of the server.
                 */
                UserId account = CollaborationConnection.getConnection()
                        .getUser();
                fromStr = account.getHost();
            }
            if (IDConverter.isFromRoom(fromStr)) {
                if (msg.getType().equals(Message.Type.groupchat)) {
                    /* group chat is picked up by listeners on the venue */
                    return;
                }
            }
            XMPPError error = msg.getError();
            SessionPayload payload = (SessionPayload) msg
                    .getExtension(PacketConstants.COLLAB_XMLNS);
            String body = msg.getBody();
            if (error != null) {
                processError(fromStr, msg, error);
            } else if (payload != null) {
                processPayload(fromStr, msg, payload);
            } else if (body != null) {
                processBody(fromStr, msg, body);
            }
        }
    }

    /**
     * Process chat message error
     * 
     * @param fromStr
     * @param msg
     * @param error
     */
    private void processError(String fromStr, Message msg, XMPPError error) {
        ChatMessageEvent event = createMessageEvent(msg);
        /*
         * errors in text messages happen when the message is bounced back from
         * the server, inform chat view that the message can't be delivered
         */
        String errorText = "Unable to deliver message to " + fromStr;
        event.setError(errorText);
        routeMessage(msg, event);
    }

    /**
     * Process collaboration payload packet from server
     * 
     * @param fromStr
     * @param msg
     * @param payload
     */
    private void processPayload(String fromStr, Message msg,
            SessionPayload payload) {
        if (payload != null) {
            switch (payload.getPayloadType()) {
            case Command:
                routeData(payload, msg);
                break;
            case Config:
                handleConfiguration(fromStr, payload.getData().toString());
                break;
            default:
                // do nothing
            }
        }
    }

    /**
     * Process text message from XMPP server
     * 
     * @param fromStr
     * @param msg
     * @param body
     */
    private void processBody(String fromStr, Message msg, String body) {
        Activator.getDefault().getNetworkStats()
                .log(Activator.PEER_TO_PEER, 0, body.length());
        if (body.startsWith(Tools.CONFIG_PREAMBLE)) {
            // TODO Legacy config support
            body = body.substring(Tools.CONFIG_PREAMBLE.length(), body.length()
                    - Tools.DIRECTIVE_SUFFIX.length());
            this.handleConfiguration(fromStr, body);
        } else {
            // anything else pass to the normal text
            routeMessage(msg, createMessageEvent(msg));
        }
    }

    /**
     * Post data as event to associated session
     * 
     * @param payload
     * @param msg
     */
    private void routeData(SessionPayload payload, Message msg) {
        String sessionId = (String) msg.getProperty(Tools.PROP_SESSION_ID);
        Object object = payload.getData();
        if (object != null && sessionId != null) {
            // Ok, we have a session id.
            ISession session = manager.getSession(sessionId);
            if (session != null) {
                /*
                 * TODO 14.4 sends objects using venue handles, pre-14.4 uses
                 * actual user IDs. Once all clients are using venue handles, we
                 * should validate that the message is coming from a participant
                 * in the venue session. See Omaha #3294
                 */
                session.postEvent(object);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "ERROR: Unknown sessionid [" + sessionId + "]");
            }
        } else {
            String warning = "Received null for the following: ";
            if (object == null) {
                warning += "payload data, ";
            }
            if (sessionId == null) {
                warning += "session ID, ";
            }
            warning += "from " + msg.getFrom();
            statusHandler.debug(warning);
        }
    }

    /**
     * Construct TextMessage and ChatMessageEvent from XMPP message
     * 
     * @param message
     * @return
     */
    private ChatMessageEvent createMessageEvent(Message message) {
        String fromStr = message.getFrom();
        IUser fromId;
        if (IDConverter.isFromRoom(fromStr)) {
            fromId = IDConverter.convertFromRoom(null, fromStr);
        } else {
            fromId = IDConverter.convertFrom(message.getFrom());
        }
        TextMessage textMsg = new TextMessage(fromId, message.getBody());
        textMsg.setFrom(fromId);
        textMsg.setBody(message.getBody());
        textMsg.setSubject(message.getSubject());
        for (String key : message.getPropertyNames()) {
            Object v = message.getProperty(key);
            if (v instanceof String) {
                textMsg.setProperty(key, (String) v);
            }
        }
        return new ChatMessageEvent(textMsg);
    }

    /**
     * Post text message event to chat
     * 
     * @param message
     */
    private void routeMessage(Message message, ChatMessageEvent chatEvent) {
        String sessionId = (String) message.getProperty(
                Tools.PROP_SESSION_ID);
        // Now find out who gets the message. If the message doesn't contain
        // a session id then assume its a straight text chat message.
        if (sessionId == null) {
            manager.postEvent(chatEvent);
        } else {
            // Ok, we have a session id.
            ISession session = manager.getSession(sessionId);
            if (session != null) {
                session.postEvent(chatEvent);
            }
        }
    }

    /**
     * Parse server configuration and notify general event bus of config event
     * 
     * @param from
     *            used the validate that the configuration comes from the server
     * @param body
     */
    private void handleConfiguration(String from, String body) {
        /* ignore config that doesn't come from server */
        UserId account = CollaborationConnection.getConnection().getUser();
        if (!from.equals(account.getHost())) {
            statusHandler.debug("Ignoring config message from " + from + ": "
                    + body);
            return;
        }
        // Determine if an error has occurred.
        if (IHttpXmppMessage.configErrorPattern.matcher(body).matches()) {
            statusHandler.handle(
                    UFStatus.Priority.ERROR,
                    this.getCollaborationConfigurationParameterValue(body,
                            IHttpXmppMessage.ERROR_PARAMETER_NAME)
                            + ". Shared Display Sessions have been disabled.");
            this.disableSharedDisplaySession();
            // terminate execution
            return;
        }

        // Validate the configuration.
        if (IHttpXmppMessage.configURLPattern.matcher(body).matches() == false) {
            statusHandler
                    .handle(UFStatus.Priority.PROBLEM,
                            "Received invalid configuration from openfire. Shared Display Sessions have been disabled.");
            this.disableSharedDisplaySession();
            return;
        }

        // Remove the parameter name.
        String httpCollaborationURL = this
                .getCollaborationConfigurationParameterValue(body,
                        IHttpXmppMessage.URL_PARAMETER_NAME);
        // validate the url.
        try {
            URL u = new URL(httpCollaborationURL);
            URI uri = u.toURI();
            if (!uri.getScheme().equalsIgnoreCase("http")) {
                throw new CollaborationException(
                        "Provided URL doesn't use the HTTP scheme");
            }
        } catch (Exception e) {
            statusHandler.handle(UFStatus.Priority.PROBLEM,
                    "Received an invalid http url from openfire - "
                            + httpCollaborationURL
                            + ". Shared Display Sessions have been disabled.",
                    e);
            this.disableSharedDisplaySession();
            return;
        }


        httpServer = httpCollaborationURL;
        // configuration is valid; publish it.
        IHttpdCollaborationConfigurationEvent configurationEvent = new HttpdCollaborationConfigurationEvent(
                httpCollaborationURL);
        manager.postEvent(configurationEvent);
    }

    /**
     * Parse config parameter value from key:value string
     * 
     * @param body
     * @param parameterName
     * @return
     */
    private String getCollaborationConfigurationParameterValue(String body,
            String parameterName) {

        // Remove the parameter name.
        return body.replace(parameterName + " :", "").trim();
    }

    /**
     * Notify general event bus that shared display is disabled
     */
    private void disableSharedDisplaySession() {
        // ensure that the shared session displays will be disabled
        IHttpdCollaborationConfigurationEvent configurationEvent = new HttpdCollaborationConfigurationEvent(
                null);
        manager.postEvent(configurationEvent);
    }

    /**
     * reset internal state when client disconnects from server
     */
    public static void reset() {
        httpServer = null;
    }

}
