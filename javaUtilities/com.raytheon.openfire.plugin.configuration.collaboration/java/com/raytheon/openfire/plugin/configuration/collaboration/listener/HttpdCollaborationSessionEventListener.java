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

import com.raytheon.openfire.plugin.configuration.collaboration.configuration.HttpdCollaborationConfiguration;

/**
 * @author bkowal
 * 
 */
public class HttpdCollaborationSessionEventListener implements SessionEventListener {
	private static final int MSG_SEND_DELAY = 5000;
	private static final String CONFIG_PREAMBLE = "[[CONFIG#";
	private static final String CONFIG_SUFFIX = "]]";
	private HttpdCollaborationConfiguration httpdCollaborationConfiguration;
	private JID serverAddress;
	private MessageRouter router;

	/**
	 * 
	 */
	public HttpdCollaborationSessionEventListener() {
		this.httpdCollaborationConfiguration = null;
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
		final String body = CONFIG_PREAMBLE
				+ this.httpdCollaborationConfiguration.toString() + CONFIG_SUFFIX;
		message.setBody(body);
		
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

	/**
	 * @param httpdCollaborationConfiguration
	 *            the openfireConfiguration to set
	 */
	public void setOpenfireConfiguration(
			HttpdCollaborationConfiguration httpdCollaborationConfiguration) {
		this.httpdCollaborationConfiguration = httpdCollaborationConfiguration;
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