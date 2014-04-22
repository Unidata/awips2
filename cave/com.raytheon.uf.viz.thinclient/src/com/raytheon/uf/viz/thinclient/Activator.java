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
package com.raytheon.uf.viz.thinclient;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            mschenke    Initial creation
 * Mar  3, 2014 2861       mschenke    Create preference store immediately
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class Activator extends AbstractUIPlugin {

    // The plug-in ID
    public static final String PLUGIN_ID = "com.raytheon.uf.viz.thinclient";

    // The shared instance
    private static Activator plugin;

    private BundleContext ctx;

    // General preference store
    private IPersistentPreferenceStore prefs = new HierarchicalPreferenceStore(
            this);

    // Preferences for UI
    private HierarchicalPreferenceStore uiPrefs;

    private IThinClientComponent component;

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
    public void start(BundleContext context) throws Exception {
        super.start(context);
        this.ctx = context;
        plugin = this;
    }

    public BundleContext getContext() {
        return ctx;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
     * )
     */
    public void stop(BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);

        // Persist the cache manager
        if (component != null) {
            component.stopComponent();
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#getPreferenceStore()
     */
    @Override
    public IPersistentPreferenceStore getPreferenceStore() {
        if (prefs == null) {
            prefs = new ScopedPreferenceStore(new InstanceScope(), PLUGIN_ID);
        }

        return prefs;
    }

    /**
     * Get the Ui preference store
     * 
     * @return
     */
    public HierarchicalPreferenceStore getUiPreferenceStore() {
        return uiPrefs;
    }

    /**
     * Set the cache manager to be used for cache storage on stop(...)
     * 
     * @param cacheManager
     */
    public void setComponent(IThinClientComponent component) {
        this.component = component;
    }

    /**
     * Returns if application is running in thin client mode
     * 
     * @return true if thin client mode, false otherwise
     */
    public boolean isThinClientMode() {
        return component != null;
    }
}
