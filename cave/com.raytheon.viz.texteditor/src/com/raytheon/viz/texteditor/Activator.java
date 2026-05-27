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

package com.raytheon.viz.texteditor;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;

/**
 * The activator class controls the plug-in life cycle
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 13, 2007  368      lvenable  Initial creation.
 * Oct 11, 2007  482      grichard  Reformatted file.
 * Feb 26, 2019  7746     randerso  Added preference store and methods to save
 *                                  most recently used TextWS Script directory.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class Activator extends AbstractUIPlugin {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(Activator.class);

    /** The plug-in ID */
    public static final String PLUGIN_ID = "com.raytheon.viz.texteditor";

    /**
     * Preference key for most recently used script directory
     */
    private static final String MRU_SCRIPT_DIR = "MostRecentScriptDirectory";

    // The shared instance
    private static Activator plugin;

    // pref store
    private HierarchicalPreferenceStore prefs = new HierarchicalPreferenceStore(
            this);

    /**
     * The constructor
     */
    public Activator() {
    }

    @Override
    public void start(BundleContext context) throws Exception {
        super.start(context);
        plugin = this;
    }

    @Override
    public void stop(BundleContext context) throws Exception {
        plugin = null;
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

    @Override
    public IPersistentPreferenceStore getPreferenceStore() {
        return prefs;
    }

    /**
     * @return most recently used TextWS script directory
     */
    public Path getMostRecentDir() {
        IPersistentPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        String scriptDir = prefs.getString(MRU_SCRIPT_DIR);
        if (scriptDir.isEmpty()) {
            scriptDir = System.getProperty("user.home");
        }

        return Paths.get(scriptDir);
    }

    /**
     * Save directory containing the most recently used TextWS script.
     *
     * @param scriptPath
     */
    public void saveMostRecentDir(Path scriptPath) {
        IPersistentPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();

        prefs.setValue(MRU_SCRIPT_DIR, scriptPath.getParent().toString());

        try {
            prefs.save();
        } catch (IOException e) {
            statusHandler.error("Error saving text workstation preferences", e);
        }
    }

}
