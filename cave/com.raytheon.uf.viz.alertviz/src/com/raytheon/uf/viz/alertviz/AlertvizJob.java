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
package com.raytheon.uf.viz.alertviz;

import java.io.StringReader;
import java.net.ServerSocket;
import java.util.HashSet;
import java.util.Set;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.TextMessage;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.apache.activemq.broker.BrokerService;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.alertviz.internal.LogMessageDAO;

/**
 * Primary AlertViz capability
 * 
 * This job can run in two modes--as a server, or as a client. The code will
 * automatically detect whether a server is already running on the specified
 * point and start running.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2008  1433       chammack     Initial creation
 * Jun 3, 2013  2026       randerso     Improve error handling
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class AlertvizJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertvizJob.class, "GDN_ADMIN", "GDN_ADMIN");

    public static final String TCP_CONNECTION = "tcp://0.0.0.0:";

    public static final String STOMP_CONNECTION = "stomp://0.0.0.0:";

    public static final String LOCAL_SERVICE_NAME = "notification";

    private static AlertvizJob instance;

    private int basePort;

    private BrokerService broker;

    private Set<ILogUpdatedCallback> logUpdatedCallbacks;

    private Set<IAlertArrivedCallback> alertArrivedCallbacks;

    private Container container;

    boolean alreadyStarted = true;

    /**
     * Start job in client mode
     * 
     * @param server
     */
    public AlertvizJob(String server, boolean hideErrors) throws Exception {
        super("AlertService");
        this.setSystem(true);
        this.logUpdatedCallbacks = new HashSet<ILogUpdatedCallback>();
        this.alertArrivedCallbacks = new HashSet<IAlertArrivedCallback>();
        instance = this;

        try {
            AlertVizClient avc = new AlertVizClient(server, hideErrors);
            avc.start();
        } catch (Exception e) {
            if (hideErrors) {
                // Log to internal Log4j log
                Container.logInternal(Priority.ERROR,
                        "AlertVizJob: exception when starting AlertVizClient.",
                        e);
                final String str = "Error setting up alert server";
                final Status s = new Status(Status.ERROR, Activator.PLUGIN_ID,
                        str, e);
                org.eclipse.jface.dialogs.ErrorDialog.openError(Display
                        .getCurrent().getActiveShell(), str, str, s);
            }
        }
    }

    /**
     * Start job in server mode, connecting to port
     * 
     * @param basePort
     */
    public AlertvizJob(int basePort) {
        super("AlertService");
        this.basePort = basePort;
        this.setSystem(true);
        this.logUpdatedCallbacks = new HashSet<ILogUpdatedCallback>();
        this.alertArrivedCallbacks = new HashSet<IAlertArrivedCallback>();

        instance = this;
        try {
            // Attempt to open socket on this port to see if in use
            java.net.ServerSocket socket = new ServerSocket(basePort);
            socket.close();
            // Not in use
            alreadyStarted = false;
        } catch (Exception e1) {
            // ignore, socket in use, do not start up?
        }
        try {
            AlertVizNotificationObserver.registerAlertVizNotificationObserver();
            if (!alreadyStarted) {
                this.startLocalAlertServer();
                this.startMessageConsumer();

                statusHandler.handle(Priority.VERBOSE,
                        "Alert subsystem has started.");

                this.schedule();
            } else {
                AlertVizClient avc = new AlertVizClient(false, this.basePort);
                avc.start();
            }
        } catch (Exception e) {
            // Log to internal Log4j log
            Container.logInternal(Priority.ERROR,
                    "AlertVizJob: exception when starting the local "
                            + "alert server or message consumer.", e);
            final String str = "Error setting up alert server";
            final Status s = new Status(Status.ERROR, Activator.PLUGIN_ID, str,
                    e);
            org.eclipse.jface.dialogs.ErrorDialog.openError(Display
                    .getCurrent().getActiveShell(), str, str, s);
        }
    }

    public boolean isAlreadyStarted() {
        return alreadyStarted;
    }

    private void startLocalAlertServer() throws AlertvizException {

        broker = new BrokerService();

        // configure the broker
        try {
            broker.setBrokerName(LOCAL_SERVICE_NAME);
            broker.setPersistent(false);
            broker.setUseJmx(false);
            broker.addConnector(STOMP_CONNECTION + (basePort + 1));
            broker.addConnector(TCP_CONNECTION + basePort);
            broker.start();
        } catch (Exception e) {
            throw new AlertvizException("Error setting up notification server",
                    e);
        }
    }

    private void startMessageConsumer() throws AlertvizException {
        AlertVizClient avc = new AlertVizClient(true, 0);
        avc.start();

        try {
            MessageConsumer consumer = avc.getInboundConsumer();
            JAXBContext context = JAXBContext.newInstance(StatusMessage.class);
            final javax.xml.bind.Unmarshaller umsh = context
                    .createUnmarshaller();
            container = new Container(alertArrivedCallbacks);
            consumer.setMessageListener(new MessageListener() {

                @Override
                public void onMessage(Message arg0) {
                    final TextMessage tm = (TextMessage) arg0;
                    Display.getDefault().asyncExec(new Runnable() {

                        @Override
                        public void run() {
                            String xmlString = null;
                            StatusMessage statusMessage = null;
                            try {
                                xmlString = tm.getText();
                                StringReader sr = new StringReader(xmlString);
                                statusMessage = (StatusMessage) umsh
                                        .unmarshal(sr);
                                if (statusMessage.getEventTime() == null) {
                                    statusMessage.setEventTime(SimulatedTime
                                            .getSystemTime().getTime());
                                }

                                displayAlert(statusMessage);

                            } catch (JMSException e) {
                                String message = "Unable to retrieve JMS message text";
                                handleInteralError(message, e);
                            } catch (JAXBException e) {
                                String message = "Unable to unmarshal XML:\n"
                                        + xmlString;
                                handleInteralError(message, e);
                            } catch (Exception e) {
                                String message = "Unexpected exception";
                                if (xmlString == null) {
                                    message += ": ";
                                } else if (statusMessage == null) {
                                    message += " while processing:\n"
                                            + xmlString;
                                } else {
                                    message += " while processing:\n"
                                            + statusMessage;
                                }
                                handleInteralError(message, e);
                            }
                        }

                    });

                }

            });

            avc.getConnectionManager().startProcessing();
        } catch (JMSException e) {
            throw new AlertvizException("Error setting up message consumer", e);
        } catch (JAXBException e) {
            throw new AlertvizException(
                    "Error preparing serialization context", e);
        }
    }

    private void handleInteralError(String message, Throwable e) {

        // Log to internal Log4j log
        Container.logInternal(Priority.CRITICAL, message, e);
        StatusMessage sm = new StatusMessage("GDN_ADMIN", "GDN_ADMIN",
                Priority.CRITICAL, this.getClass().getPackage().getName(),
                message, e);
        sm.setMachineToCurrent();
        sm.setEventTime(SimulatedTime.getSystemTime().getTime());
        try {
            LogMessageDAO.getInstance().save(sm);
        } catch (AlertvizException e1) {
            // Nothing but we can do but print stacktrace
            // Log to internal Log4j log
            Container.logInternal(Priority.ERROR,
                    "AlertVizJob unalbe to save to internal database.", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        return Status.OK_STATUS;
    }

    /**
     * Add a log updated callback
     * 
     * @param callback
     */
    public static void addLogUpdatedCallback(ILogUpdatedCallback callback) {
        synchronized (instance) {
            instance.logUpdatedCallbacks.add(callback);
        }
    }

    /**
     * Remove a log updated callback
     * 
     * @param callback
     */
    public synchronized static void removeLogUpdatedCallback(
            ILogUpdatedCallback callback) {
        synchronized (instance) {
            instance.logUpdatedCallbacks.remove(callback);
        }
    }

    /**
     * Add an alert arrived callback
     * 
     * @param callback
     */
    public static void addAlertArrivedCallback(IAlertArrivedCallback callback) {
        synchronized (instance) {
            instance.alertArrivedCallbacks.add(callback);
        }
    }

    /**
     * Remove an alert arrived callback
     * 
     * @param callback
     */
    public synchronized static void removeAlertArrivedCallback(
            IAlertArrivedCallback callback) {
        synchronized (instance) {
            instance.alertArrivedCallbacks.remove(callback);
        }
    }

    public static void displayAlert(StatusMessage msg) {
        try {
            instance.container.messageReceived(msg);
            // Notify the listeners
            synchronized (instance) {
                if (instance.logUpdatedCallbacks.size() > 0) {
                    int[] range = LogMessageDAO.getInstance().getLogRange();
                    for (ILogUpdatedCallback callback : instance.logUpdatedCallbacks) {
                        callback.logUpdated(range[0], range[1]);
                    }
                }
            }
        } catch (Exception e) {
            // Don't send to log because of potential for
            // infinite loop of exceptions
            // Log to internal Log4j log
            Container
                    .logInternal(
                            Priority.ERROR,
                            "AlertVizJob unable to get the LogRange from the internal database.",
                            e);
        }
    }

}
