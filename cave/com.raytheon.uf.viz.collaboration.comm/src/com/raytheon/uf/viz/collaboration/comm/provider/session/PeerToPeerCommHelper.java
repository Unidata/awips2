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

import java.util.Map;
import java.util.regex.Pattern;

import org.eclipse.ecf.presence.IIMMessageEvent;
import org.eclipse.ecf.presence.IIMMessageListener;
import org.eclipse.ecf.presence.im.IChatMessage;
import org.eclipse.ecf.presence.im.IChatMessageEvent;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.Activator;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IHttpdCollaborationConfigurationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ITextMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.ChatMessageEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.HttpdCollaborationConfigurationEvent;
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
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class PeerToPeerCommHelper implements IIMMessageListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PeerToPeerCommHelper.class);

    private static String httpServer;

    public static String getCollaborationHttpServer() {
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

    /**
     * 
     */
    @Override
    public void handleMessageEvent(IIMMessageEvent messageEvent) {
        if (messageEvent instanceof IChatMessageEvent) {
            IChatMessageEvent event = (IChatMessageEvent) messageEvent;

            IChatMessage msg = event.getChatMessage();
            String body = msg.getBody();
            Activator.getDefault().getNetworkStats()
                    .log(Activator.PEER_TO_PEER, 0, body.length());
            if (body != null) {
                if (body.startsWith(Tools.CMD_PREAMBLE)) {
                    routeData(msg);
                } else if (body.startsWith(Tools.CONFIG_PREAMBLE)) {
                    this.handleConfiguration(body);
                } else {
                    // anything else pass to the normal text
                    routeMessage(msg);
                }
            }
        }
    }

    /**
     * 
     * @param message
     */
    private void routeData(IChatMessage message) {
        Object object = null;
        try {
            object = Tools.unMarshallData(message.getBody());
        } catch (CollaborationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error unmarshalling PeerToPeer data", e);
        }
        if (object != null) {
            String sessionId = (String) message.getProperties().get(
                    Tools.PROP_SESSION_ID);
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
            manager.postEvent(object);
        }
    }

    /**
     * 
     * @param message
     */
    private void routeMessage(IChatMessage message) {
        String from = message.getFromID().getName();
        IQualifiedID fromId = new UserId(Tools.parseName(from),
                Tools.parseHost(from));
        fromId.setResource(Tools.parseResource(message.getFromID().getName()));
        TextMessage textMsg = new TextMessage(fromId, message.getBody());
        textMsg.setFrom(fromId);
        textMsg.setBody(message.getBody());
        textMsg.setSubject(message.getSubject());
        @SuppressWarnings("unchecked")
        Map<Object, Object> props = message.getProperties();
        for (Object o : props.keySet()) {
            if (o instanceof String) {
                String key = (String) o;
                Object v = props.get(key);
                if (v instanceof String) {
                    textMsg.setProperty(key, (String) v);
                }
            }
        }
        ITextMessageEvent chatEvent = new ChatMessageEvent(textMsg);

        String sessionId = (String) message.getProperties().get(
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

    private void handleConfiguration(String body) {
        final String parameterName = "sessionDataHttpURL";
        final String suffix = "]]";
        // Validate the configuration.
        final String configPatternRegex = Tools.CONFIG_PREAMBLE.replace("[",
                "\\[") + parameterName + " : .+" + suffix;
        Pattern configPattern = Pattern.compile(configPatternRegex);
        if (configPattern.matcher(body).matches() == false) {
            statusHandler
                    .handle(UFStatus.Priority.PROBLEM,
                            "Received invalid configuration from openfire. Shared Display Sessions have been disabled.");
        }

        // Eliminate the preamble.
        String encodedConfiguration = body.replace(Tools.CONFIG_PREAMBLE, "");
        // Eliminate the suffix: ]]
        encodedConfiguration = encodedConfiguration.substring(0,
                encodedConfiguration.length() - 2);

        // Currently, we are only receiving one configurable element.

        // JSON would be preferable.
        // sessionDataHttpURL : [URL]

        // Remove the parameter name.
        String httpdCollaborationURL = encodedConfiguration.replace(
                parameterName + " :", "").trim();
        final String collaborationURLRegex = "http://.+:[1-9][0-9]*/session_data/";
        // validate the url.
        Pattern urlPattern = Pattern.compile(collaborationURLRegex);
        if (urlPattern.matcher(httpdCollaborationURL).matches() == false) {
            statusHandler.handle(UFStatus.Priority.PROBLEM,
                    "Received an invalid http url from openfire - "
                            + httpdCollaborationURL
                            + ". Shared Display Sessions have been disabled.");
            return;
        }

        httpServer = httpdCollaborationURL;
        // configuration is valid; publish it.
        IHttpdCollaborationConfigurationEvent configurationEvent = new HttpdCollaborationConfigurationEvent(
                httpdCollaborationURL);
        manager.postEvent(configurationEvent);
    }
}
