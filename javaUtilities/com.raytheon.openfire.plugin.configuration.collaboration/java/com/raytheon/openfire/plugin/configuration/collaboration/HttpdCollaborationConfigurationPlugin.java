/**
 * @author bkowal
 */
package com.raytheon.openfire.plugin.configuration.collaboration;

import java.io.File;
import java.net.InetAddress;
import java.net.UnknownHostException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.jivesoftware.util.JiveGlobals;
import org.jivesoftware.util.TaskEngine;
import org.jivesoftware.openfire.MessageRouter;
import org.jivesoftware.openfire.XMPPServer;
import org.jivesoftware.openfire.container.Plugin;
import org.jivesoftware.openfire.container.PluginManager;
import org.jivesoftware.openfire.event.SessionEventDispatcher;
import org.xmpp.packet.JID;
import org.apache.commons.configuration.ConfigurationException;

import com.raytheon.openfire.plugin.configuration.collaboration.configuration.HttpdCollaborationConfiguration;
import com.raytheon.openfire.plugin.configuration.collaboration.httpd.HttpdCollaborationStatusMonitor;
import com.raytheon.openfire.plugin.configuration.collaboration.httpd.HttpdCollaborationConfReader;
import com.raytheon.openfire.plugin.configuration.collaboration.listener.HttpdCollaborationSessionEventListener;

/**
 * The main plugin class for the AWIPS II Httpd Collaboration Configuration
 * plugin; creates and configures an openfire listener and the httpd monitor.
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
public class HttpdCollaborationConfigurationPlugin implements Plugin {
	private static final Logger logger = LoggerFactory
			.getLogger(HttpdCollaborationConfigurationPlugin.class);

	private static final String LOCATION = "plugin.collaboration.httpd.location";

	private static final String INTERVAL = "plugin.collaboration.httpd.interval";

	private static final String DEFAULT_LOCATION = "/awips2/httpd_collaboration";

	private static final long DEFAULT_INTERVAL_MS = 60000;

	private static final long MONITOR_DELAY_MS = 60000; /* 1 Minute */

	private static final String ABORT_ERROR_MESSAGE = "Aborting initialization of the HttpdCollaborationConfigurationPlugin plugin.";

	private HttpdCollaborationSessionEventListener listener;

	private HttpdCollaborationStatusMonitor httpdCollaborationStatusMonitor;

	private TaskEngine monitorTaskEngine;

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
		if ((this.listener == null) == false) {
			SessionEventDispatcher.removeListener(this.listener);

			this.listener.dispose();
			this.listener = null;
		}

		if ((this.monitorTaskEngine == null) == false) {
			this.monitorTaskEngine.shutdown();
			this.monitorTaskEngine = null;
		}
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
		HttpdCollaborationConfReader confReader = new HttpdCollaborationConfReader(
				this.getHttpdCollaborationLocation());
		try {
			confReader.execute();
		} catch (ConfigurationException e1) {
			logger.error("Unable to read the httpd collaboration configuration.",
					e1);
			logger.error(ABORT_ERROR_MESSAGE);
			return;
		} catch (Exception e2) {
			logger.error("An unexpected error has occurred.", e2);
			logger.error(ABORT_ERROR_MESSAGE);
			return;
		}

		/*
		 * Determine the hostname - there is a requirement in place that
		 * requires openfire and httpd-collaboration to be installed on the same
		 * machine.
		 */
		InetAddress address = null;
		try {
			address = InetAddress.getLocalHost();
		} catch (UnknownHostException e1) {
			logger.error("Unable to retrieve the hostname.", e1);
			logger.error(ABORT_ERROR_MESSAGE);
			return;
		}

		/* Persist the configuration information. */
		HttpdCollaborationConfiguration httpdCollaborationConfiguration = new HttpdCollaborationConfiguration();
		httpdCollaborationConfiguration.setSessionDataHost(address
				.getHostName());
		httpdCollaborationConfiguration.setSessionDataPort(confReader
				.getListenPort());

		/* The httpd-collaboration status monitor */
		this.httpdCollaborationStatusMonitor = new HttpdCollaborationStatusMonitor(
				this.getHttpdCollaborationLocation(),
				httpdCollaborationConfiguration.getHttpdCollaborationURL(),
				httpdCollaborationConfiguration.toString());

		/* Retrieve openfire components. */
		JID serverAddress = new JID(XMPPServer.getInstance().getServerInfo()
				.getXMPPDomain());
		MessageRouter router = XMPPServer.getInstance().getMessageRouter();

		/* Create a new listener. */
		this.listener = new HttpdCollaborationSessionEventListener(
				httpdCollaborationConfiguration.toString());

		/* Initialize the listener. */
		this.listener
				.setHttpdCollaborationStatusChecker(this.httpdCollaborationStatusMonitor);
		this.listener.setServerAddress(serverAddress);
		this.listener.setRouter(router);

		/* Make it possible for the listener to receive events. */
		SessionEventDispatcher.addListener(this.listener);
		this.scheduleStatusMonitor();
	}

	/**
	 * Schedules the httpd collaboration status monitor at the specified,
	 * configurable interval using the openfire TaskEngine; see
	 * {@link TaskEngine}
	 */
	private synchronized void scheduleStatusMonitor() {
		logger.info("Scheduling the httpd collaboration status monitor ...");
		if (this.monitorTaskEngine == null) {
			this.monitorTaskEngine = TaskEngine.getInstance();
		}
		this.monitorTaskEngine.schedule(this.httpdCollaborationStatusMonitor,
				MONITOR_DELAY_MS, this.getHttpdMonitorInterval());
	}

	/**
	 * Stops the scheduled httpd collaboration status monitor if it has been
	 * scheduled.
	 */
	private synchronized void stopStatusMonitor() {
		if (this.monitorTaskEngine == null) {
			return;
		}
		this.monitorTaskEngine
				.cancelScheduledTask(this.httpdCollaborationStatusMonitor);
	}

	/**
	 * Sets the configurable installation location of awips2-httpd-collaboration
	 * 
	 * @param _location
	 *            the location of awips2-httpd-collaboration
	 */
	public void setHttpdCollaborationLocation(String _location) {
		JiveGlobals.setProperty(LOCATION, _location);
	}

	/**
	 * Returns the installation location of awips2-httpd-collaboration from the
	 * openfire configuration
	 * 
	 * @return the installation root of awips2-httpd-collaboration
	 */
	public String getHttpdCollaborationLocation() {
		return JiveGlobals.getProperty(LOCATION, DEFAULT_LOCATION);
	}

	/**
	 * Sets the configurable interval for the amount of time between executions
	 * of the httpd collaboration status monitor
	 * 
	 * @param _interval
	 *            the interval in milliseconds
	 */
	public void setHttpdMonitorInterval(long _interval) {
		long originalInterval = this.getHttpdMonitorInterval();
		if (_interval == originalInterval) {
			return;
		}
		JiveGlobals.setProperty(INTERVAL, Long.toString(_interval));
		this.stopStatusMonitor();
		this.scheduleStatusMonitor();
	}

	/**
	 * Returns the interval from the openfire configuration
	 * 
	 * @return the interval in milliseconds
	 */
	public long getHttpdMonitorInterval() {
		return JiveGlobals.getLongProperty(INTERVAL, DEFAULT_INTERVAL_MS);
	}
}