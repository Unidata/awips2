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

package com.raytheon.uf.viz.product.alertviz;

import org.eclipse.core.internal.runtime.InternalPlatform;
import org.eclipse.core.runtime.ILogListener;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.statushandlers.StatusAdapter;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.alertviz.AlertVizPreferences;
import com.raytheon.uf.viz.alertviz.AlertVizStatusHandlerFactory;
import com.raytheon.uf.viz.alertviz.AlertvizJob;
import com.raytheon.uf.viz.alertviz.AlertvizJob.AlertVizJobListener;
import com.raytheon.uf.viz.alertviz.Container;
import com.raytheon.uf.viz.alertviz.ReceiverConnChecker;
import com.raytheon.uf.viz.alertviz.SystemStatusHandler;
import com.raytheon.uf.viz.application.component.IStandaloneComponent;
import com.raytheon.uf.viz.core.ProgramArguments;
import com.raytheon.uf.viz.core.localization.CAVELocalizationAdapter;
import com.raytheon.uf.viz.core.localization.CAVELocalizationNotificationObserver;
import com.raytheon.uf.viz.core.localization.LocalizationConstants;
import com.raytheon.uf.viz.core.localization.LocalizationInitializer;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;

/**
 *
 * Implements the AlertViz standalone application
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 29, 2008  1433     chammack  Initial creation
 * Jan 12, 2012  27       rferrel   Added createAlertVisualization
 * May 08, 2013  1939     rjpeter   Updated to start NotificationManagerJob.
 * Aug 26, 2014  3356     njensen   Explicitly set localization adapter
 * Jun 04, 2015  4473     njensen   Defer launching of UI to Activator and OSGi
 *                                  services
 * Sep 17, 2015  4822     njensen   Block application exit until receiver
 *                                  disconnects
 * Sep 21, 2015  4654     njensen   Setup extra localization levels
 * Apr 19, 2018  7013     tgurney   Remove unused createAlertVisualization
 *                                  method
 * Oct 04, 2018  7484     randerso  Changed to use AV_ADMIN for internal errors
 * Oct 08, 2018  7515     randerso  Adjusted priorities of AlertViz internal
 *                                  errors.
 * Oct 12, 2018  7515     randerso  Implemented AlertVizStatusHandlerFactory to
 *                                  handle internal AlertViz errors properly.
 *
 * </pre>
 *
 * @author chammack
 */
@SuppressWarnings("restriction")
public class AlertVizApplication implements IStandaloneComponent {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertVizApplication.class, "AV_ADMIN", "AV_ADMIN");

    /**
     * Private class used to verify we do not exit the application before the
     * job has been properly shut down.
     */
    private static class ConnectedListener implements AlertVizJobListener {
        private boolean connected;

        @Override
        public void receiverConnected() {
            connected = true;
        }

        @Override
        public void receiverDisconnected() {
            connected = false;
        }

    }

    @Override
    public Object startComponent(String componentName) throws Exception {
        UFStatus.setHandlerFactory(new AlertVizStatusHandlerFactory());

        Display display = PlatformUI.createDisplay();

        int port = -1;
        Integer checkPort = ProgramArguments.getInstance().getInteger("-p");
        if (checkPort != null) {
            port = checkPort.intValue();
        }

        // No need to check alertviz, we are alertviz
        try {
            initializeLocalization();
            if (port < 1) {
                // access prefs after localization is initialized
                port = AlertVizPreferences.getAlertVizPort();
            }
            // Need to register localization updates
            initializeObservers();
            IPreferenceStore store = LocalizationManager.getInstance()
                    .getLocalizationStore();
            // save the port so cave knows what port we are running on
            store.setValue(LocalizationConstants.P_ALERT_SERVER,
                    "tcp://localhost:" + port);
            if (store instanceof IPersistentPreferenceStore) {
                ((IPersistentPreferenceStore) store).save();
            }
        } catch (Exception e) {
            statusHandler.fatal(e.getLocalizedMessage(), e);
            throw e;
        }

        /*
         * This is a workaround to receive status messages because without the
         * PlatformUI initialized Eclipse throws out the status messages. The
         * standalone AlertViz currently never starts the PlatformUI.
         */
        InternalPlatform.getDefault()
                .getLog(WorkbenchPlugin.getDefault().getBundle())
                .addLogListener(new ILogListener() {

                    @Override
                    public void logging(IStatus status, String plugin) {
                        if (!PlatformUI.isWorkbenchRunning()) {
                            new SystemStatusHandler()
                                    .handle(new StatusAdapter(status), 0);
                        }
                    }

                });

        if (ReceiverConnChecker.isReceiverRunning(port)) {
            String exitMsg = "Alertviz already started on port: " + port
                    + ", exiting";
            System.out.println(exitMsg);
            Container.logInternal(Priority.ERROR, exitMsg);
            return IApplication.EXIT_OK;
        }
        AlertvizJob.getInstance().setEmbedded(false);
        ConnectedListener listener = new ConnectedListener();
        AlertvizJob.getInstance().addAlertVizJobListener(listener);
        AlertvizJob.getInstance().start(port);

        // open JMS connection to allow alerts to be received
        NotificationManagerJob.connect();

        Throwable t = null;
        try {
            while (!display.isDisposed()) {
                try {
                    if (!display.readAndDispatch()) {
                        display.sleep();
                    }
                } catch (Exception e) {
                    Container.logInternal(Priority.ERROR,
                            "Error occured while running alertviz", e);
                    statusHandler.handle(Priority.PROBLEM,
                            "Error occured while running alertviz", e);
                }
            }
        } catch (Throwable t1) {
            Container.logInternal(Priority.ERROR,
                    "Error occured while running alertviz", t1);
            statusHandler.handle(Priority.PROBLEM,
                    "Error occured while running alertviz", t1);
            t = t1;
        } finally {
            display.dispose();
            if (t != null) {
                // Killed because of error, set exit status to non zero value
                return IApplication.EXIT_RELAUNCH;
            }

            /*
             * don't let the application exit until it is properly disconnected
             */
            int stopTries = 0;
            while (listener.connected && stopTries < 5) {
                /*
                 * if the job is successfully canceled, the receiverDisconnect()
                 * will be triggered
                 */
                AlertvizJob.getInstance().cancel();
                Thread.sleep(5);
                stopTries++;
            }
            AlertvizJob.getInstance().removeAlertVizJobListener(listener);
        }

        return AlertvizJob.getInstance().getExitStatus();
    }

    protected void initializeObservers() {
        CAVELocalizationNotificationObserver.register();
    }

    protected void initializeLocalization() throws Exception {
        PathManagerFactory.setAdapter(new CAVELocalizationAdapter());
        new LocalizationInitializer(true, false).run();
        AlertVizLocalizationConfigurer.registerExtraLevels();
    }

}
