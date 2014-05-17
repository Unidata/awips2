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
package com.raytheon.rcm.server;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.rcm.coll.Collector;
import com.raytheon.rcm.coll.RequestScheduler;
import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.ConfigurationProvider;
import com.raytheon.rcm.config.MutableConfiguration;
import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.NotificationEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.otrmgr.OTRManager;
import com.raytheon.rcm.rmrmgr.RMRManager;
import com.raytheon.rcm.rpsmgr.RPSListManager;

/**
 * Main class for the Radar Server.
 * <p>
 * This is a container class for the various components of the Radar Server.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ...
 * 2014-02-03   DR 14762   D. Friedman Connect configuration's event target to
 *                                     the RadarServer instance.
 * </pre>
 */
public class RadarServer implements RadarEventListener {
    // protected ConfigurationProvider configurationProvider;
    protected Configuration configuration;

    protected LinkManager linkManager;

    protected ConnectionManagerImpl connectionManager;

    protected ArrayList<RadarEventListener> listeners = new ArrayList<RadarEventListener>();

    protected Object runSem = new Object();

    public static void main(String[] args) throws IllegalAccessException,
            InstantiationException, ClassNotFoundException {
        RadarServer server = createServer();

        server.addDefaultListeners(); // listeners should not do anything until
                                      // radars are connected...
        // but what to rmr and otr do?

        // server.connectDedicatedRadars();
        server.run();
    }

    // TODO: throw these exceptions?
    public static RadarServer createServer() throws IllegalAccessException,
            InstantiationException, ClassNotFoundException {

        Log.event("RadarServer started");

        String configurationProviderClassName = System
                .getProperty(RadarServer.class.getCanonicalName()
                        + ".configurationProviderClass");

        if (configurationProviderClassName == null)
            configurationProviderClassName = "com.raytheon.rcm.config.std.StandardConfigProvider";

        ConfigurationProvider provider = (ConfigurationProvider) Class.forName(
                configurationProviderClassName).newInstance();

        /* (ConfigurationProvider) Class.forName(args[0]) */
        /* lookup a *factory* class and pass relevate args */

        return new RadarServer(provider.getConfiguration());
    }

    public static RadarServer createServer(Configuration configuration) {
        return new RadarServer(configuration);
    }

    public RadarServer(Configuration configuration) {
        // this.configurationProvider = provider;
        this.configuration = configuration; // provider.getConfiguration();
        linkManager = new com.raytheon.rcm.oldio.LinkManager(configuration);
        linkManager.setEventHandler(this);
        connectionManager = new ConnectionManagerImpl(this, linkManager);
        addListener(connectionManager);
        // connectionManager =
        if (configuration instanceof MutableConfiguration) {
            ((MutableConfiguration) configuration).setConfigurationEventTarget(this);
        }
    }

    public void addDefaultListeners() {
        addListener(new EventLogger());
        addListener(new RPSListManager(this));
        addListener(new Collector(this));
        // addListener(new Awips2Endpoint(this));
        addListener(new OTRManager(this));
        addListener(new RMRManager(this));
        addListener(new AlertRequestManager(this));
        addListener(new DedicatedRadarActivator(this));
        addListener(new RequestScheduler(this));
        addListener(new RadarServerAvailable(this));
    }

    public void addListener(RadarEventListener l) {
        synchronized (listeners) {
            listeners.add(l);
        }
    }

    // Needed so that the RemoteRadarServer can find components.
    public List<RadarEventListener> getListeners() {
        synchronized (listeners) {
            return new ArrayList<RadarEventListener>(listeners);
        }
    }

    public void run() {
        try {
            try {
                synchronized (runSem) {
                    runSem.wait();
                }
            } catch (InterruptedException e) {
                // nothing
            }
        } finally {
            // TODO: connectionManager.stop()..
        }
    }

    public void stop() {
        synchronized (runSem) {
            runSem.notify();
        }
    }

    public ConnectionManager getConnectionManager() {
        return connectionManager;
    }

    public StatusManager getStatusManager() {
        return connectionManager;
    }

    public Configuration getConfiguration() {
        return configuration;
    }

    @Override
    public void handleRadarEvent(RadarEvent event) {
        RadarEventListener[] ls; // TODO: reuse
        synchronized (listeners) {
            ls = listeners.toArray(new RadarEventListener[listeners.size()]);
        }
        for (RadarEventListener l : ls) {
            try {
                l.handleRadarEvent(event);
            } catch (Exception e) {
                Log.errorf("Error while listener %s processed event %s:", l,
                        event, e);
            }
        }
    }

    @Override
    public void handleConfigEvent(ConfigEvent event) {
        RadarEventListener[] ls; // TODO: reuse
        synchronized (listeners) {
            ls = listeners.toArray(new RadarEventListener[listeners.size()]);
        }
        for (RadarEventListener l : ls) {
            try {
                l.handleConfigEvent(event);
            } catch (Exception e) {
                Log.errorf("Error while listener %s processed event %s: %s", l,
                        event, e);
            }
        }
    }

    @Override
    public void handleNotificationEvent(NotificationEvent event) {
        RadarEventListener[] ls; // TODO: reuse
        synchronized (listeners) {
            ls = listeners.toArray(new RadarEventListener[listeners.size()]);
        }
        for (RadarEventListener l : ls) {
            try {
                l.handleNotificationEvent(event);
            } catch (Exception e) {
                Log.errorf("Error while listener %s processed event %s: %s", l,
                        event, e);
            }
        }
    }

}
