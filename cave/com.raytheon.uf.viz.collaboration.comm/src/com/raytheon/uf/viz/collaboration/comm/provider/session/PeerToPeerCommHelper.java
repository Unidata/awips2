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

import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.Activator;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IHttpdCollaborationConfigurationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IHttpdXmppMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ITextMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.SessionPayload;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.ChatMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.HttpdCollaborationConfigurationEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;

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
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class PeerToPeerCommHelper implements PacketListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PeerToPeerCommHelper.class);

    private static Object httpServerLockObj = new Object();

    private static String httpServer;
    
    private static long HTTP_SERVER_TIMEOUT = 10 * 1000; // 10 seconds

    /**
     * Get HTTP server address. This method blocks until the address has been
     * received from the chat server or a timeout has expired.
     * 
     * @return
     */
    public static String getCollaborationHttpServer() {
        /**
         * Wait for initialization of field httpServer.
         */
        synchronized (httpServerLockObj) {
            long start = System.currentTimeMillis();
            try {
                while (httpServer == null) {
                    if (System.currentTimeMillis() - start > HTTP_SERVER_TIMEOUT) {
                        // TODO should we get fallback server address from
                        // localization?
                        statusHandler
                                .error("HTTP URL configuration not received from server");
                        break;
                    }
                    httpServerLockObj.wait(500);
                }
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "PeerToPeerCommHelper unable to resolve server URL. "
                                + e.getLocalizedMessage(), e);
            }
        }
        return httpServer;
    }

    private CollaborationConnection manager;

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
            if (IDConverter.isFromRoom(msg.getFrom())) {
                // venues will have their own listeners
                return;
            }
            String body = msg.getBody();
            if (body != null) {
                Activator.getDefault().getNetworkStats()
                        .log(Activator.PEER_TO_PEER, 0, body.length());
                if (body.startsWith(Tools.CONFIG_PREAMBLE)) {
                    // TODO Legacy config support
                    body = body.substring(Tools.CONFIG_PREAMBLE.length(),
                            body.length() - Tools.DIRECTIVE_SUFFIX.length());
                    this.handleConfiguration(body);
                } else {
                    // anything else pass to the normal text
                    routeMessage(msg);
                }
            } else {
                SessionPayload payload = (SessionPayload) msg
                        .getExtension(SessionPayload.XMLNS);
                if (payload != null) {
                    switch (payload.getPayloadType()) {
                    case Command:
                        routeData(payload,
                                (String) msg.getProperty(Tools.PROP_SESSION_ID));
                        break;
                    case Config:
                        handleConfiguration(payload.getData().toString());
                        break;
                    default:
                        // do nothing
                    }
                }
            }
        }
    }


    /**
     * Post data as event either to associated session, or general event bus
     * 
     * @param payload
     * @param sessionId
     */
    private void routeData(SessionPayload payload, String sessionId) {
        Object object = payload.getData();
        if (object != null) {
            if (sessionId == null) {
                manager.postEvent(object);
            } else {
                // Ok, we have a session id.
                ISession session = manager.getSession(sessionId);
                if (session != null) {
                    session.postEvent(object);
                } else {
                    statusHandler.handle(Priority.PROBLEM,
                            "ERROR: Unknown sessionid [" + sessionId + "]");
                }
            }
        }
    }

    /**
     * Post text message to chat
     * 
     * @param message
     */
    private void routeMessage(Message message) {
        IQualifiedID fromId = IDConverter.convertFrom(message.getFrom());
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
        ITextMessageEvent chatEvent = new ChatMessageEvent(textMsg);

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
     * @param body
     */
    private void handleConfiguration(String body) {
        // Determine if an error has occurred.
        if (IHttpdXmppMessage.configErrorPattern.matcher(body).matches()) {
            statusHandler.handle(
                    UFStatus.Priority.ERROR,
                    this.getCollaborationConfigurationParameterValue(body,
                            IHttpdXmppMessage.ERROR_PARAMETER_NAME)
                            + ". Shared Display Sessions have been disabled.");
            this.disableSharedDisplaySession();
            // terminate execution
            return;
        }

        // Validate the configuration.
        if (IHttpdXmppMessage.configURLPattern.matcher(body).matches() == false) {
            statusHandler
                    .handle(UFStatus.Priority.PROBLEM,
                            "Received invalid configuration from openfire. Shared Display Sessions have been disabled.");
            this.disableSharedDisplaySession();
            return;
        }

        // Remove the parameter name.
        String httpdCollaborationURL = this
                .getCollaborationConfigurationParameterValue(body,
                        IHttpdXmppMessage.URL_PARAMETER_NAME);
        // validate the url.
        if (IHttpdXmppMessage.urlPattern.matcher(httpdCollaborationURL)
                .matches() == false) {
            statusHandler.handle(UFStatus.Priority.PROBLEM,
                    "Received an invalid http url from openfire - "
                            + httpdCollaborationURL
                            + ". Shared Display Sessions have been disabled.");
            this.disableSharedDisplaySession();
            return;
        }

        synchronized (httpServerLockObj) {
            httpServer = httpdCollaborationURL;
            httpServerLockObj.notifyAll();
        }
        // configuration is valid; publish it.
        IHttpdCollaborationConfigurationEvent configurationEvent = new HttpdCollaborationConfigurationEvent(
                httpdCollaborationURL);
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

}
