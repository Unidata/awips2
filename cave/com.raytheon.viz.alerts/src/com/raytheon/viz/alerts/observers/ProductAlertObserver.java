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
package com.raytheon.viz.alerts.observers;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.edex.msg.DataURINotificationMessage;
import com.raytheon.edex.msg.PracticeDataURINotificationMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.NoPluginException;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.core.mode.CAVEMode;

/**
 * Job to monitor the JMS topic containing alert messages, delegating the
 * messages to the appropriate listeners. Listener execution is performed in
 * isolated threads, so users may perform appropriate operations inside the
 * listener method.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/26/07		140			bphillip	Initial Creation
 * 11/26/07     443         bphillip    Modified to receive data URI lists
 * 02/28/08     966         chammack    Refactored to support generalized listeners
 * 05/08/08     1127        randerso    Changed to implement INotificationObserver
 * 10/06/08     1433        chammack    Updated to use VizStatus
 * 01/14/2013   1442        rferrel     Filter out simulated time "future" alerts.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

public class ProductAlertObserver implements INotificationObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProductAlertObserver.class);

    /** The source used for VizStatus */
    public static final String SOURCE = "ALERTS";

    /** The subscription topic */
    private static final String ALERT_TOPIC = "edex.alerts";

    /**
     * The constant used to designate all messages (no plugin filter) internally
     */
    private static final String NO_FILTER_CONSTANT = "ALL";

    private static final long ALERT_LOG_INTERVAL = 300000;

    /** The singleton instance */
    private static ProductAlertObserver instance;

    /** The observer map of pluginName to interested parties */
    protected Map<String, List<IAlertObserver>> observers;

    /** The map of job threads from observers */
    protected Map<IAlertObserver, JobWrapper> jobWrappers;

    protected int alertsProcessed = 0;

    protected long lastLogTime = System.currentTimeMillis();

    private static ProductAlertObserver getInstance(String specificTopic) {
        if (instance == null) {
            instance = new ProductAlertObserver();
            NotificationManagerJob.addObserver(ALERT_TOPIC, instance);
        }
        if (specificTopic != null) {
            NotificationManagerJob.addObserver(ALERT_TOPIC + "."
                    + specificTopic, instance);
        }
        return instance;
    }

    private static ProductAlertObserver getInstance() {
        return getInstance(null);
    }

    /**
     * @param name
     */
    private ProductAlertObserver() {
        this.observers = new HashMap<String, List<IAlertObserver>>();
        this.jobWrappers = new HashMap<IAlertObserver, JobWrapper>();
    }

    protected void sendToObserver(IAlertObserver observer, AlertMessage msg) {
        // Get the corresponding job, creating the
        // wrapper if necessary
        JobWrapper wrapper = null;
        synchronized (this) {
            wrapper = jobWrappers.get(observer);
            if (wrapper == null) {
                wrapper = new JobWrapper(observer);
                jobWrappers.put(observer, wrapper);
            }
        }
        wrapper.put(msg);
    }

    /**
     * Register for alerts messages using a callback. Messages can be
     * pre-filtered for a specific plugin, or be applicable to all plugins by
     * passing null for pluginName.
     * 
     * @param pluginName
     *            optional pluginName filter
     * @param obs
     *            the alert observer callback
     */
    public static synchronized void addObserver(String pluginName,
            IAlertObserver obs) {
        addCustomObserver(pluginName, null, obs);
    }

    /**
     * Register for alerts messages using a callback. Messages can be
     * pre-filtered for a specific plugin, or be applicable to all plugins by
     * passing null for pluginName.
     * 
     * @param pluginName
     *            optional pluginName filter
     * @param specificTopic
     * @param obs
     *            the alert observer callback
     */
    public static synchronized void addCustomObserver(String pluginName,
            String specificTopic, IAlertObserver obs) {
        ProductAlertObserver instance = getInstance(specificTopic);
        if (pluginName == null)
            pluginName = NO_FILTER_CONSTANT;

        List<IAlertObserver> observerList = instance.observers.get(pluginName);
        if (observerList == null) {
            observerList = new CopyOnWriteArrayList<IAlertObserver>();
            instance.observers.put(pluginName, observerList);
        }
        observerList.add(obs);

    }

    /**
     * Removes an alert message observer that was registered using the
     * addObserver method. This must be called in exactly the same
     * 
     * @param pluginName
     *            optional pluginName filter
     * @param obs
     *            the observer to remove
     */
    public static synchronized void removeObserver(String pluginName,
            IAlertObserver obs) {
        ProductAlertObserver instance = getInstance();
        if (pluginName == null)
            pluginName = NO_FILTER_CONSTANT;

        List<IAlertObserver> observerList = instance.observers.get(pluginName);
        if (observerList == null) {
            return;
        }
        observerList.remove(obs);
    }

    /**
     * Provides internal insulation from slow-running processes by forming a
     * queue
     * 
     */
    private class JobWrapper extends Job {

        protected ConcurrentLinkedQueue<AlertMessage> messages;

        protected IAlertObserver observer;

        public JobWrapper(IAlertObserver observer) {
            super("Alert thread for " + observer);
            this.setSystem(true);
            this.observer = observer;
            this.messages = new ConcurrentLinkedQueue<AlertMessage>();
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if (!this.messages.isEmpty()) {
                // grab the current messages for processing and create a new
                // collection for future messages
                Collection<AlertMessage> messagesToProc = messages;
                messages = new ConcurrentLinkedQueue<AlertMessage>();
                SimulatedTime time = SimulatedTime.getSystemTime();
                if (!time.isRealTime()) {
                    // Filter out any "future" alerts.
                    long simTime = time.getTime().getTime();
                    Iterator<AlertMessage> iter = messagesToProc.iterator();
                    while (iter.hasNext()) {
                        AlertMessage message = iter.next();
                        Map<String, Object> attribs = new HashMap<String, Object>(
                                message.decodedAlert);
                        DataTime messageTime = (DataTime) attribs
                                .get("dataTime");
                        if (messageTime != null
                                && (simTime < messageTime.getRefTime()
                                        .getTime())) {
                            iter.remove();
                        }
                    }
                    if (messagesToProc.isEmpty()) {
                        return Status.OK_STATUS;
                    }
                }

                try {
                    observer.alertArrived(messagesToProc);
                } catch (Throwable e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error occurred while processing alerts", e);
                }
            }

            return Status.OK_STATUS;
        }

        /**
         * Add the message to the queue
         * 
         * @param msg
         *            the msg
         */
        public void put(AlertMessage msg) {
            this.messages.offer(msg);
        }

        /**
         * Checks if is empty.
         * 
         * @return true, if is empty
         */
        public boolean isEmpty() {
            return this.messages.isEmpty();
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.alerts.INotificationObserver#notificationArrived(javax
     * .jms.Message[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        boolean loggedException = false;
        for (NotificationMessage msg : messages) {

            Object payLoad = null;
            try {
                payLoad = msg.getMessagePayload();
            } catch (NotificationException e1) {
                // Only log the first time this happens, otherwise we flood the
                // logs
                if (!loggedException) {
                    loggedException = true;
                    statusHandler
                            .handle(Priority.SIGNIFICANT,
                                    "Error reading alert messages.  Alerts are not being processed",
                                    e1);

                }
                continue;
            }

            // The message is a list of data URIs
            if ((payLoad instanceof DataURINotificationMessage)
                    || (CAVEMode.getMode().equals(CAVEMode.PRACTICE) && payLoad instanceof PracticeDataURINotificationMessage)) {
                String[] dataURIs = null;
                if (payLoad instanceof DataURINotificationMessage) {
                    DataURINotificationMessage uriMsg = (DataURINotificationMessage) payLoad;
                    dataURIs = uriMsg.getDataURIs();
                } else {
                    PracticeDataURINotificationMessage uriMsg = (PracticeDataURINotificationMessage) payLoad;
                    dataURIs = uriMsg.getDataURIs();
                }
                for (int i = 0; i < dataURIs.length; ++i) {
                    String str = dataURIs[i];
                    processDataURI(str);
                }

                startWrappers();

                if (dataURIs != null && dataURIs.length > 0) {
                    alertsProcessed += dataURIs.length;
                }

                long curTime = System.currentTimeMillis();
                if (curTime - ALERT_LOG_INTERVAL > lastLogTime) {
                    if (alertsProcessed > 0) {
                        statusHandler.handle(Priority.VERBOSE, "Processed "
                                + alertsProcessed + " alerts in the last "
                                + ((curTime - lastLogTime) / 60000)
                                + " minutes");
                        alertsProcessed = 0;
                    }
                    lastLogTime = curTime;
                }
            }
        }
    }

    public static void processDerivedAlerts(Collection<String> datauris) {
        for (String datauri : datauris) {
            getInstance().processDataURI(datauri);
        }
        getInstance().startWrappers();
    }

    private void processDataURI(String datauri) {
        if (datauri == null)
            return;
        try {
            Map<String, Object> attribs;
            try {
                attribs = RecordFactory.getInstance().loadMapFromUri(datauri);

            } catch (NoPluginException e) {
                // ignore, if we hit this it means we received an alert from
                // edex about ingested data, but viz doesn't have the necessary
                // plugins to do anything with it
                return;
            } catch (Exception e1) {
                statusHandler.handle(Priority.WARN, e1.getLocalizedMessage(),
                        e1);
                return;
            }

            AlertMessage am = new AlertMessage();
            am.dataURI = datauri;
            am.decodedAlert = Collections.unmodifiableMap(attribs);
            List<IAlertObserver> obsList = observers.get(NO_FILTER_CONSTANT);

            if (obsList != null) {
                for (IAlertObserver obs : obsList)
                    sendToObserver(obs, am);
            }

            String pluginName = (String) am.decodedAlert.get("pluginName");

            // Then to the listeners listening for this
            // specific type
            if (pluginName != null) {
                obsList = observers.get(pluginName);
                if (obsList != null) {
                    for (IAlertObserver obs : obsList)
                        sendToObserver(obs, am);
                }
            }

        } catch (RuntimeException e) {
            statusHandler
                    .handle(Priority.PROBLEM, "Error preparing updates", e);

        }
    }

    private void startWrappers() {
        Iterator<JobWrapper> iterator = jobWrappers.values().iterator();
        while (iterator.hasNext()) {
            JobWrapper wrapper = iterator.next();
            if (!wrapper.isEmpty())
                wrapper.schedule();
        }
    }

}
