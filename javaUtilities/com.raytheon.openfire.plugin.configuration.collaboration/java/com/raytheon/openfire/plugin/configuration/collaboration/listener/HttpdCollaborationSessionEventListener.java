/**
 * 
 */
package com.raytheon.openfire.plugin.configuration.collaboration.listener;

import java.util.TimerTask;

import org.jivesoftware.openfire.event.SessionEventListener;
import org.jivesoftware.openfire.session.Session;
import org.jivesoftware.util.TaskEngine;
import org.jivesoftware.openfire.MessageRouter;
import org.xmpp.packet.JID;
import org.xmpp.packet.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.openfire.plugin.configuration.collaboration.util.HttpdCollaborationUtil;
import com.raytheon.openfire.plugin.configuration.collaboration.httpd.HttpdCollaborationStatusMonitor;
import com.raytheon.openfire.plugin.configuration.collaboration.exception.HttpdCollaborationNotRunningException;
import com.raytheon.openfire.plugin.configuration.collaboration.exception.HttpdCollaborationStatusException;

/**
 * Impelements @{link SessionEventListener} to wait for new users to connect to
 * openfire so that it can send the configuration information back to them.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2012            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public class HttpdCollaborationSessionEventListener implements
		SessionEventListener {
	private static final Logger logger = LoggerFactory
			.getLogger(HttpdCollaborationSessionEventListener.class);

	// provided by the configuration
	private String HTTPD_COLLABORATION_CONFIGURATION;

	private static final int MSG_SEND_DELAY = 5000;

	private HttpdCollaborationStatusMonitor httpdCollaborationStatusMonitor;

	private JID serverAddress;

	private MessageRouter router;

	/**
	 * 
	 */
	public HttpdCollaborationSessionEventListener(
			String _httpdCollaborationConfiguration) {
		HTTPD_COLLABORATION_CONFIGURATION = _httpdCollaborationConfiguration;
		this.serverAddress = null;
		this.router = null;
	}

	public void dispose() {
		this.serverAddress = null;
		this.router = null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jivesoftware.openfire.event.SessionEventListener#anonymousSessionCreated
	 * (org.jivesoftware.openfire.session.Session)
	 */
	@Override
	public void anonymousSessionCreated(Session session) {
		/* Ignore. */
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.jivesoftware.openfire.event.SessionEventListener#
	 * anonymousSessionDestroyed(org.jivesoftware.openfire.session.Session)
	 */
	@Override
	public void anonymousSessionDestroyed(Session session) {
		/* Ignore. */
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jivesoftware.openfire.event.SessionEventListener#resourceBound(org
	 * .jivesoftware.openfire.session.Session)
	 */
	@Override
	public void resourceBound(Session session) {
		/* Ignore. */
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jivesoftware.openfire.event.SessionEventListener#sessionCreated(org
	 * .jivesoftware.openfire.session.Session)
	 */
	@Override
	public void sessionCreated(Session session) {
		final Message message = new Message();
		message.setTo(session.getAddress());
		message.setFrom(this.serverAddress);
		message.setBody(this.composeMessageBody());

		TimerTask messageTask = new TimerTask() {
			@Override
			public void run() {
				router.route(message);
			}
		};
		TaskEngine.getInstance().schedule(messageTask, MSG_SEND_DELAY);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jivesoftware.openfire.event.SessionEventListener#sessionDestroyed
	 * (org.jivesoftware.openfire.session.Session)
	 */
	@Override
	public void sessionDestroyed(Session session) {
		/* Ignore. */
	}

	private String composeMessageBody() {
		// Verify that httpd-collaboration is / is still running.
		try {
			this.httpdCollaborationStatusMonitor.statusHttpdCollaboration();
		} catch (HttpdCollaborationNotRunningException e1) {
			logger.error("httpd-collaboration is not running.", e1);
			return HttpdCollaborationUtil.encodeErrorMessage(e1);
		} catch (HttpdCollaborationStatusException e2) {
			logger.error(
					"failed to determine the status of httpd-collaboration", e2);
			return HttpdCollaborationUtil.encodeErrorMessage(e2);
		} catch (Exception e3) {
			logger.error("Unexpected exception occurred!", e3);
			return HttpdCollaborationUtil.encodeErrorMessage(e3);
		}

		return HTTPD_COLLABORATION_CONFIGURATION;
	}

	public void setHttpdCollaborationStatusChecker(
			HttpdCollaborationStatusMonitor httpdCollaborationStatusMonitor) {
		this.httpdCollaborationStatusMonitor = httpdCollaborationStatusMonitor;
	}

	/**
	 * @param serverAddress
	 *            the serverAddress to set
	 */
	public void setServerAddress(JID serverAddress) {
		this.serverAddress = serverAddress;
	}

	/**
	 * @param router
	 *            the router to set
	 */
	public void setRouter(MessageRouter router) {
		this.router = router;
	}
}