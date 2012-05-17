/**
 * @author bkowal
 */
package com.raytheon.openfire.plugin.configuration.collaboration;

import java.io.File;
import java.net.InetAddress;
import java.net.UnknownHostException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.jivesoftware.openfire.MessageRouter;
import org.jivesoftware.openfire.XMPPServer;
import org.jivesoftware.openfire.container.Plugin;
import org.jivesoftware.openfire.container.PluginManager;
import org.jivesoftware.openfire.event.SessionEventDispatcher;
import org.xmpp.packet.JID;
import org.apache.commons.configuration.ConfigurationException;

import com.raytheon.openfire.plugin.configuration.collaboration.configuration.HttpdCollaborationConfiguration;
import com.raytheon.openfire.plugin.configuration.collaboration.httpd.HttpdCollaborationConfReader;
import com.raytheon.openfire.plugin.configuration.collaboration.listener.HttpdCollaborationSessionEventListener;

/**
 * @author bkowal
 * 
 */
public class HttpdCollaborationConfigurationPlugin implements Plugin {
	private static final Logger Log = LoggerFactory
			.getLogger(HttpdCollaborationConfigurationPlugin.class);
	private static final String ABORT_ERROR_MESSAGE = 
		"Aborting initialization of the HttpdCollaborationConfigurationPlugin plugin.";
	private HttpdCollaborationSessionEventListener listener;

	/**
	 * 
	 */
	public HttpdCollaborationConfigurationPlugin() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.jivesoftware.openfire.container.Plugin#destroyPlugin()
	 */
	@Override
	public void destroyPlugin() {
		if (this.listener == null) {
			return;
		}

		SessionEventDispatcher.removeListener(this.listener);

		this.listener.dispose();
		this.listener = null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.jivesoftware.openfire.container.Plugin#initializePlugin(org.jivesoftware
	 * .openfire.container.PluginManager, java.io.File)
	 */
	@Override
	public void initializePlugin(PluginManager arg0, File arg1) {
		/*
		 * Attempt to read the httpd collaboration configuration.
		 */
		HttpdCollaborationConfReader confReader = new HttpdCollaborationConfReader();
		try {
			confReader.execute();
		} catch (ConfigurationException e1) {
			Log.error("Unable to read the httpd collaboration configuration.",
					e1);
			Log.error(ABORT_ERROR_MESSAGE);
			return;
		} catch (Exception e2) {
			Log.error("An unexpected error has occurred.", e2);
			Log.error(ABORT_ERROR_MESSAGE);
			return;
		}

		/*
		 * Determine the hostname - there is a restriction in place that
		 * requires openfire and httpd-collaboration to be installed on the same
		 * machine.
		 */
		InetAddress address = null;
		try {
			address = InetAddress.getLocalHost();
		} catch (UnknownHostException e1) {
			Log.error("Unable to retrieve the hostname.", e1);
			Log.error(ABORT_ERROR_MESSAGE);
		}

		/* Persist the configuration information. */
		HttpdCollaborationConfiguration httpdCollaborationConfiguration = new HttpdCollaborationConfiguration();
		httpdCollaborationConfiguration.setSessionDataHost(address.getHostName());
		httpdCollaborationConfiguration.setSessionDataPort(confReader.getListenPort());

		/* Retrieve openfire components. */
		JID serverAddress = new JID(XMPPServer.getInstance().getServerInfo()
				.getXMPPDomain());
		MessageRouter router = XMPPServer.getInstance().getMessageRouter();

		/* Create a new listener. */
		this.listener = new HttpdCollaborationSessionEventListener();

		/* Initialize the listener. */
		this.listener.setOpenfireConfiguration(httpdCollaborationConfiguration);
		this.listener.setServerAddress(serverAddress);
		this.listener.setRouter(router);

		/* Make it possible for the listener to receive events. */
		SessionEventDispatcher.addListener(this.listener);
	}
}