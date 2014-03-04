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
package com.raytheon.openfire.plugin.configuration.collaboration;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.jivesoftware.openfire.IQRouter;
import org.jivesoftware.openfire.RoutingTable;
import org.jivesoftware.openfire.XMPPServer;
import org.jivesoftware.openfire.container.Plugin;
import org.jivesoftware.openfire.container.PluginManager;
import org.jivesoftware.openfire.disco.IQDiscoInfoHandler;
import org.jivesoftware.openfire.event.SessionEventDispatcher;
import org.jivesoftware.openfire.session.ClientSession;
import org.jivesoftware.util.JiveGlobals;
import org.jivesoftware.util.TaskEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmpp.packet.IQ;
import org.xmpp.packet.JID;
import org.xmpp.packet.Packet;

import com.raytheon.openfire.plugin.configuration.collaboration.configuration.ConfigurationPacket;
import com.raytheon.openfire.plugin.configuration.collaboration.http.HttpStatusMonitor;
import com.raytheon.openfire.plugin.configuration.collaboration.iq.AbstractConfigHandler;
import com.raytheon.openfire.plugin.configuration.collaboration.iq.DataAuthHandler;
import com.raytheon.openfire.plugin.configuration.collaboration.iq.HttpAddressHandler;
import com.raytheon.openfire.plugin.configuration.collaboration.iq.SecurityToggleHandler;
import com.raytheon.openfire.plugin.configuration.collaboration.listener.CollaborationSessionEventListener;

/**
 * The main plugin class for the AWIPS II Collaboration HTTP Configuration
 * plugin; creates and configures an openfire listener and the http monitor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 07, 2012            bkowal      Initial creation
 * Jan 06, 2013  2563      bclement    replaced TaskEngine shutdown with cancel task
 *                                     added legacy format setter/accessor
 * Feb 14, 2013 2756       bclement    rename and refactor for operation with generic http
 *                                     server configured over XMPP
 * Mar 04, 2014 2756       bclement    added dataserver security toggle update to setLegacySupport
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public class HttpConfigurationPlugin implements Plugin {
	private static final Logger logger = LoggerFactory
			.getLogger(HttpConfigurationPlugin.class);

    private static final String INTERVAL = "plugin.collaboration.http.interval";

    private static final long DEFAULT_INTERVAL_MS = 60000;

    private static final long MONITOR_DELAY_MS = 10000; /* 10 seconds */

    private TaskEngine monitorTaskEngine;

	private CollaborationSessionEventListener listener;

	private HttpStatusMonitor httpStatusMonitor;

    private JID serverId;

	/**
	 * 
	 */
	public HttpConfigurationPlugin() {
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
            // we don't want to shutdown the engine since it is a singleton
            // if we do, we can't reload the plugin
            this.monitorTaskEngine
                    .cancelScheduledTask(this.httpStatusMonitor);
            this.monitorTaskEngine = null;
        }
	}

    /**
     * Registers IQ configuration handlers with IQ router and adds features to
     * info discovery
     * 
     * @param server
     * @param handlers
     */
    private void registerConfigHandlers(XMPPServer server,
            List<AbstractConfigHandler> handlers) {
        IQRouter router = server.getIQRouter();
        IQDiscoInfoHandler infoHandler = server.getIQDiscoInfoHandler();
        for (AbstractConfigHandler handler : handlers) {
            Iterator<String> iter = handler.getFeatures();
            while (iter.hasNext()) {
                infoHandler.addServerFeature(iter.next());
            }
            router.addHandler(handler);
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
    public void initializePlugin(PluginManager manager, File arg1) {
        XMPPServer server = XMPPServer.getInstance();

        DataAuthHandler authHandler = new DataAuthHandler();
        HttpAddressHandler addressHandler = new HttpAddressHandler();
        SecurityToggleHandler secTogHandler = new SecurityToggleHandler();
        registerConfigHandlers(server,
                Arrays.asList(authHandler, addressHandler, secTogHandler));

        /* Retrieve openfire components. */
        serverId = new JID(server.getServerInfo().getXMPPDomain());

        /* The http status monitor */
        this.httpStatusMonitor = new HttpStatusMonitor(addressHandler, serverId);

        /* Create a new listener. */
        this.listener = new CollaborationSessionEventListener();

        /* Initialize the listener. */
        this.listener
                .setHttpStatusChecker(this.httpStatusMonitor);
        this.listener.setServerAddress(serverId);
        this.listener.setRouter(server.getMessageRouter());

        /* Make it possible for the listener to receive events. */
        SessionEventDispatcher.addListener(this.listener);
        this.scheduleStatusMonitor();
	}

    /**
     * Schedules the http collaboration status monitor at the specified,
     * configurable interval using the openfire TaskEngine; see
     * {@link TaskEngine}
     */
    private synchronized void scheduleStatusMonitor() {
        logger.info("Scheduling the httpd collaboration status monitor ...");
        if (this.monitorTaskEngine == null) {
            this.monitorTaskEngine = TaskEngine.getInstance();
        }
        this.monitorTaskEngine.schedule(this.httpStatusMonitor,
                MONITOR_DELAY_MS, this.getHttpMonitorInterval());
    }

    /**
     * Send packet to all available client sessions for the primary dataserver
     * user
     * 
     * @param p
     */
    private void sendPacketToDataserver(final Packet p) {
        String primary = AbstractConfigHandler.getPrimaryDataServer();
        if ( primary != null){
            JID to = new JID(primary);
            p.setTo(to);
            p.setFrom(serverId);
            Collection<ClientSession> sessions = getSessions(to);
            if (sessions.isEmpty()) {
                logger.warn("No sessions found for dataserver user");
            }
            for (ClientSession session : sessions) {
                session.process(p);
            }
        } else {
            logger.warn("No dataserver user configured in settings");
        }
    }

    /**
     * Get a collection of client sessions with server for a user
     * 
     * @param id
     * @return
     */
    private Collection<ClientSession> getSessions(JID id) {
        XMPPServer server = XMPPServer.getInstance();
        RoutingTable routingTable = server.getRoutingTable();
        List<JID> routes = routingTable.getRoutes(id, serverId);
        List<ClientSession> rval = new ArrayList<ClientSession>(routes.size());
        for (JID route : routes) {
            ClientSession session = routingTable.getClientRoute(route);
            if (session != null) {
                rval.add(session);
            }
        }
        return rval;
    }

    /**
     * Stops the scheduled http collaboration status monitor if it has been
     * scheduled.
     */
    private synchronized void stopStatusMonitor() {
        if (this.monitorTaskEngine == null) {
            return;
        }
        this.monitorTaskEngine
                .cancelScheduledTask(this.httpStatusMonitor);
    }

    /**
     * Sets the configurable interval for the amount of time between executions
     * of the http collaboration status monitor
     * 
     * @param _interval
     *            the interval in milliseconds
     */
    public void setHttpMonitorInterval(long _interval) {
        long originalInterval = this.getHttpMonitorInterval();
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
    public long getHttpMonitorInterval() {
        return JiveGlobals.getLongProperty(INTERVAL, DEFAULT_INTERVAL_MS);
    }


    /**
     * Sets the global value for toggling pre 14.3 message format support
     * 
     * @param legacy
     */
    public void setLegacySupport(boolean legacy) {
        JiveGlobals.setProperty(ConfigurationPacket.LEGACY_KEY,
                Boolean.toString(legacy));
        IQ update = SecurityToggleHandler.createUpdatePacket();
        sendPacketToDataserver(update);
    }

    /**
     * @return true if configured to support pre 14.3 message format
     */
    public boolean hasLegacySupport() {
        return JiveGlobals.getBooleanProperty(ConfigurationPacket.LEGACY_KEY,
                true);
    }

    /**
     * Set white list of accounts that belong to dataservers. The first server
     * in the list is treated as the primary dataserver.
     * 
     * @param whiteList
     */
    public void setDataserverUsers(String whiteList) {
        JiveGlobals
                .setProperty(AbstractConfigHandler.DATASERVER_USERS_KEY, whiteList);
    }

    /**
     * @return white list of accounts that belong to dataservers. The first
     *         server in the list is treated as the primary dataserver.
     */
    public String getDataserverUsers() {
        return JiveGlobals
                .getProperty(AbstractConfigHandler.DATASERVER_USERS_KEY, "");
    }

}