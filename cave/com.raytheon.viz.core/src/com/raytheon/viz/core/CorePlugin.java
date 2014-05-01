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

package com.raytheon.viz.core;

import javax.media.jai.ParameterBlockJAI;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;

/**
 * Core Plug-in activator
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/1/06                    chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * 
 */
public class CorePlugin extends AbstractUIPlugin {

    public static String PLUGIN_NAME = "com.raytheon.viz.core";

    // The shared instance.
    private static CorePlugin plugin;

    private IPersistentPreferenceStore prefs;

    private BundleContext ctx;

    /**
     * 
     */
    public CorePlugin() {
        super();
        plugin = this;

    }

    /**
     * Returns the shared instance.
     */
    public static CorePlugin getDefault() {
        return plugin;
    }

    /**
     * This method is called upon plug-in activation
     */
    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);

        this.ctx = context;

        // Hack to avoid jai classloader contention
        ParameterBlockJAI pbj = new ParameterBlockJAI("scale");
        pbj.getNumParameters();
        // DataStoreFactory.getDataStore(new File("/tmp/foo"));
    }

    public BundleContext getContext() {
        return this.ctx;
    }

    /**
     * This method is called when the plug-in is stopped
     */
    @Override
    public void stop(BundleContext context) throws Exception {
        super.stop(context);
        plugin = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#getPreferenceStore()
     */
    @Override
    public HierarchicalPreferenceStore getPreferenceStore() {
        try {
            if (prefs == null) {
                prefs = new HierarchicalPreferenceStore(this);
            }
        } catch (LocalizationException e) {
            UFStatus.getHandler().handle(
                    Priority.PROBLEM,
                    "Error reading preference store: "
                            + e.getLocalizedMessage(), e);
        }

        return (HierarchicalPreferenceStore) prefs;
    }

}
