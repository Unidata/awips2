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
package com.raytheon.uf.viz.monitor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.monitor.events.IMonitorEvent;
import com.raytheon.uf.viz.monitor.listeners.IMonitorConfigurationListener;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;
import com.raytheon.uf.viz.monitor.listeners.IMonitorThresholdListener;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * 
 * Monitor, abstract core for FFMP/SCAN/FOG/SNOW/SAFESEAS
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 1981       dhladky     Initial creation.
 * 2/27/2009    2047       grichard    Use 'foreach' in 'arrived' methods.
 * 3/2/2009     2047       grichard    Added stationId resolution method.
 * 3/5/2009     2047       grichard    Made plugin and station names arrays.
 * Dec 18, 2009 3424       zhao        Made addMinitorListener public
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public abstract class Monitor implements IMonitor, IMonitorThresholdListener,
        IMonitorConfigurationListener {

    /** Set to whatever plugins the monitor needs -- empty by default */
    public static String[] pluginName = { "", "", "" };

    /** Set to whatever stations the filter needs -- empty by default */
    protected static String[] stationName = { "" };

    /** Array of monitor listeners **/
    private ArrayList<IMonitorListener> monitorListeners = new ArrayList<IMonitorListener>();

    @Override
	public void alertArrived(Collection<AlertMessage> alertMessages) {
		// knock down messages we don't want
		for (AlertMessage msg : alertMessages) {
			DataTime dataTime = (DataTime) msg.decodedAlert.get("dataTime");
			if (dataTime.getRefTime().before(
					SimulatedTime.getSystemTime().getTime())) {
				if (filterProductMessage(msg)) {
					processProductMessage(msg);
				}
			}
		}
	}

    @Override
    public void notificationArrived(NotificationMessage[] notifyMessages) {
        // knock down messages we don't want
        for (NotificationMessage msg : notifyMessages) {
            if (filterNotifyMessage(msg)) {
                processNotifyMessage(msg);
            }
        }
    }

    /**
     * Adds this monitor as an Observer to CAVE events
     * 
     * @param pluginName
     * @param Monitor
     */
    public void initObserver(String pluginName, Monitor monitor) {
        // adds this monitor as a listener for ProductAlerts
        // you can filter by data pluginName if you wish.
        ProductAlertObserver.addObserver(pluginName, monitor);
    }

    /**
     * Removes this monitor as an Observer to CAVE events
     * 
     * @param pluginName
     * @param Monitor
     */
    public void stopObserver(String pluginName, Monitor monitor) {
        // removes this monitor as a listener for ProductAlerts.
        ProductAlertObserver.removeObserver(pluginName, monitor);
    }

    /**
     * Kill this monitor
     * 
     * @param pluginName
     * @param Monitor
     */
    public void exitMonitor(String pluginName, Monitor monitor) {
        stopObserver(pluginName, monitor);
    }

    /**
     * Helper method to resolve syntactical mismatch for field names depending
     * on the plugin name.
     * 
     * @param pluginName
     *            -- the name of the plugin
     * @return String -- representation of the field name for station identifier
     */
    public String getStationIdFieldName(String pluginName) {
        String fieldName = "stationId";
        if ("obs".equals(pluginName)) {
            fieldName = "location.stationId";
        }
        return fieldName;
    }

    /**
     * Filter for product messages we care about.
     * 
     * @param alertMessages
     * @return
     */
    protected abstract boolean filterProductMessage(AlertMessage alertMessage);

    /**
     * Process the filtered product messages.
     * 
     * @param filtered
     */
    protected abstract void processProductMessage(AlertMessage filtered);

    /**
     * Process the localization filtered messages.
     * 
     * @param filtered
     */
    protected abstract void processNotifyMessage(NotificationMessage filtered);

    /**
     * Filter for localization messages we care about.
     * 
     * @param alertMessages
     * @return
     */
    protected abstract boolean filterNotifyMessage(
            NotificationMessage alertMessage);

    /**
     * Fire data change event notifications to the front end displays.
     * 
     * @param monitor
     **/
    public void fireMonitorEvent() {

        final Monitor fmonitor = this;

        VizApp.runAsync(new Runnable() {
            public void run() {
                IMonitorEvent me = new IMonitorEvent(fmonitor);
                Iterator<IMonitorListener> iter = monitorListeners.iterator();

                while (iter.hasNext()) {

                    IMonitorListener iml = iter.next();
                    iml.notify(me);
                }
            }
        });
    }

    /**
     * Fire data change event notifications to the front end displays, Updates
     * by listener type.
     * 
     * @param monitor
     **/
    public void fireMonitorEvent(String type) {

        final Monitor fmonitor = this;
        final String ftype = type;
        // System.out.println("Listener class: "+type);

        VizApp.runAsync(new Runnable() {
            public void run() {
                IMonitorEvent me = new IMonitorEvent(fmonitor);
                Iterator<IMonitorListener> iter = monitorListeners.iterator();

                while (iter.hasNext()) {
                    IMonitorListener listener = iter.next();
                    // selectively fire events based on a targeted listener
                    if (listener.getClass().getName().equals(ftype)) {
                        listener.notify(me);
                    }
                }
            }
        });
    }

    /**
     * Fire data change event notifications to the front end displays.
     * 
     * @param monitor
     **/
    public void fireMonitorEvent(Monitor monitor) {

        final Monitor fmonitor = monitor;
        VizApp.runAsync(new Runnable() {
            public void run() {
                IMonitorEvent me = new IMonitorEvent(fmonitor);
                Iterator<IMonitorListener> iter = monitorListeners.iterator();

                while (iter.hasNext()) {

                    IMonitorListener iml = iter.next();
                    // debug(" in fireMonitorEvent(): " + iml.toString());
                    iml.notify(me);
                }
            }
        });
    }

    /**
     * Kill this monitor by nullifying the monitor's private instance variable.
     */
    protected abstract void nullifyMonitor();

    /**
     * Add monitor listener
     * 
     * Revison history Dec 15, 2009 zhao Changed this method from "protected" to
     * "public"
     * 
     * @param isu
     */
    public synchronized void addMonitorListener(IMonitorListener iml) {
        monitorListeners.add(iml);
    }

    /**
     * remove the monitor listener
     * 
     * @param isu
     */
    protected synchronized void removeMonitorListener(IMonitorListener iml) {
        monitorListeners.remove(iml);
    }

    /**
     * Get the list if you need it
     * 
     * @return
     */
    public ArrayList<IMonitorListener> getMonitorListeners() {
        return monitorListeners;
    }

    /**
     * Make a DB request
     * 
     * @param sql
     * @return
     */
    public List<Object[]> dataRequest(String pluginName, String sql) {

        List<Object[]> results = null;
        try {
            results = DirectDbQuery.executeQuery(sql, "metadata",
                    DirectDbQuery.QueryLanguage.SQL);

        } catch (Exception ed2) {
            ed2.printStackTrace();
        }
        return results;
    }
}
