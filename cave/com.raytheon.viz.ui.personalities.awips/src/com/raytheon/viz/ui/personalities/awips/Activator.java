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
package com.raytheon.viz.ui.personalities.awips;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.viz.alertviz.AlertVizPreferences;
import com.raytheon.uf.viz.alertviz.AlertvizJob;
import com.raytheon.uf.viz.alertviz.ReceiverConnChecker;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

    // The plug-in ID
    public static final String PLUGIN_ID = "com.raytheon.viz.ui.personalities.awips";

    // The shared instance
    private static Activator plugin;

    private BundleContext context;

    private AlertVizChecker avChecker;

    /**
     * Checks if alertviz is running on the configured port, and if not, starts
     * it internally. Reschedules itself automatically to periodically check
     * again.
     */
    protected static class AlertVizChecker extends Job {
        public AlertVizChecker() {
            super("AlertViz Connection Check");
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            int port = AlertVizPreferences.getAlertVizPort();
            if (!ReceiverConnChecker.isReceiverRunning(port)) {
                AlertvizJob av = AlertvizJob.getInstance();
                if (av != null) {
                    av.start(port);
                }
            }

            // TODO every 10 seconds good enough?
            this.schedule(10000);
            return Status.OK_STATUS;
        }
    }

    /**
     * The constructor
     */
    public Activator() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
     * )
     */
    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);
        this.context = context;
        plugin = this;

    }

    public BundleContext getContext() {
        return plugin.context;
    }

    public List<Bundle> getUnresolvedBundles() {
        Bundle[] allBundles = getContext().getBundles();
        List<Bundle> unresolved = new ArrayList<Bundle>();
        for (int i = 0; i < allBundles.length; i++)
            if (allBundles[i].getState() == Bundle.INSTALLED)
                unresolved.add(allBundles[i]);
        return unresolved;
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
        if (avChecker != null) {
            avChecker.cancel();
        }
        super.stop(context);
    }

    /**
     * Returns the shared instance
     * 
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    public AlertVizChecker getAlertVizCheckJob() {
        if (avChecker == null && LocalizationManager.internalAlertServer) {
            avChecker = new AlertVizChecker();
            avChecker.setSystem(true);
        }

        return avChecker;
    }

}
