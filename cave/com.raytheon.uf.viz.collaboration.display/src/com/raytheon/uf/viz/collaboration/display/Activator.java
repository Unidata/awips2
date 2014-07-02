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
package com.raytheon.uf.viz.collaboration.display;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.jface.preference.IPreferenceStore;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.jobs.StatsJob;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;

/**
 * Activator for Collaboration Display bundle
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2012         ?           ?          Initial creation            
 * Jul 02, 2014 1255       bclement    added prefs store
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class Activator implements BundleActivator {

    public static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Activator.class);

    private static BundleContext context;

    // The shared instance
    private static Activator plugin;

    private IPersistentPreferenceStore prefs;

    private StatsJob statsJob = new StatsJob(
            "Collaboration Network Statistics",
            com.raytheon.uf.viz.collaboration.comm.Activator.getDefault()
                    .getNetworkStats());

    static BundleContext getContext() {
        return context;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext
     * )
     */
    public void start(BundleContext bundleContext) throws Exception {
        Activator.context = bundleContext;
        statsJob.schedule();
        plugin = this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
     */
    public void stop(BundleContext bundleContext) throws Exception {
        Activator.context = null;
        statsJob.shutdown();
    }

    /**
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
    }

    /**
     * @return the preference store
     */
    public IPreferenceStore getPreferenceStore() {
        if (prefs == null) {
            prefs = new HierarchicalPreferenceStore(context
                    .getBundle().getSymbolicName());
        }
        return prefs;
    }

}
