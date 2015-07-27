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

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.osgi.framework.ServiceRegistration;
import org.slf4j.LoggerFactory;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.Appender;

import com.raytheon.uf.viz.alertviz.AlertvizJob.AlertVizJobListener;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;

/**
 * 
 * The activator class controls the plug-in life cycle
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 3, 2014  2861       mschenke    Create preference store immediately
 * May 7, 2015  4473       mschenke    Hooked AlertvizJob into OSGi services
 * Jun 3, 2015  4473       njensen     Improved OSGi hooks     
 * 
 * </pre>
 * 
 * @author unknown
 * @version 1.0
 */
public class Activator extends AbstractUIPlugin {

    // The plug-in ID
    public static final String PLUGIN_ID = "com.raytheon.uf.viz.alertviz";

    // The shared instance
    private static Activator plugin;

    private final HierarchicalPreferenceStore prefs = new HierarchicalPreferenceStore(
            this);

    private final AlertvizJob job = new AlertvizJob();

    private ServiceRegistration<AlertService> service;

    /**
     * The constructor
     */
    public Activator() {
    }

    /**
     * @return The managed job for alertviz
     */
    public AlertvizJob getAlertvizJob() {
        return this.job;
    }

    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);
        plugin = this;

        // prepare the job but do not start it up or schedule it
        job.addAlertVizJobListener(new AlertVizJobListener() {

            @Override
            public void receiverConnected() {
                if (service == null) {
                    service = getBundle().getBundleContext().registerService(
                            AlertService.class, job, null);
                }
            }

            @Override
            public void receiverDisconnected() {
                if (service != null) {
                    service.unregister();
                    service = null;
                }
            }

        });

        // necessary to prevent multiple unregisters
        context.addServiceListener(new ServiceListener() {
            @Override
            public void serviceChanged(ServiceEvent event) {
                if (service != null
                        && event.getServiceReference().equals(
                                service.getReference())
                        && event.getType() == ServiceEvent.UNREGISTERING) {
                    service = null;
                }
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
     * )
     */
    @Override
    public void stop(BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);
        stopStatusMessageAppender();
        job.cancel();
    }

    /**
     * This isn't pretty but if the AlertvizJob (aka alertviz receiver) is
     * running internally, to prevent errors we have to make sure to stop the
     * AlertvizAppender if it exists before we stop the receiving job.
     */
    private void stopStatusMessageAppender() {
        if (job.getState() == Job.RUNNING) {
            Logger logger = (Logger) LoggerFactory.getLogger("CaveLogger");
            Appender<ILoggingEvent> app = logger
                    .getAppender("AlertvizAppender");
            if (app != null) {
                app.stop();
                logger.detachAppender(app);
            }
        }
    }

    /**
     * Returns the shared instance
     * 
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    @Override
    public HierarchicalPreferenceStore getPreferenceStore() {
        return prefs;
    }
}
