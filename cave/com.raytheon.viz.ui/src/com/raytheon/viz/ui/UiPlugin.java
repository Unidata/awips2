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

package com.raytheon.viz.ui;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.ui.jobs.MemoryMonitorJob;
import com.raytheon.viz.ui.panes.DrawCoordinatorJob;

/**
 * UI Plugin
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	 Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 01, 2006             chammack  Initial Creation.
 * Oct 27, 2009  2354       bsteffen  Added preferences to ui plugin
 * Oct 24, 2012  2491       bsteffen  Do not start DrawCoordinatorJob during
 *                                    activation to allow activation before
 *                                    localization is set.
 * 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class UiPlugin extends AbstractUIPlugin {

    public static final String PLUGIN_ID = "com.raytheon.viz.ui";

    // The shared instance.
    private static UiPlugin plugin;

    private HierarchicalPreferenceStore prefs;

    private final Job memoryWatchJob = new MemoryMonitorJob();

    /**
     * The constructor.
     */
    public UiPlugin() {
        plugin = this;
    }

    /**
     * This method is called upon plug-in activation
     */
    public void start(BundleContext context) throws Exception {
        super.start(context);
        memoryWatchJob.setPriority(Job.LONG);
        memoryWatchJob.setSystem(true);
        memoryWatchJob.schedule();
    }

    /**
     * This method is called when the plug-in is stopped
     */
    public void stop(BundleContext context) throws Exception {
        super.stop(context);
        plugin = null;
        DrawCoordinatorJob.getInstance().shutdown();
        memoryWatchJob.cancel();
    }

    /**
     * Returns the shared instance.
     */
    public static UiPlugin getDefault() {
        return plugin;
    }

    /**
     * Returns an image descriptor for the image file at the given plug-in
     * relative path.
     * 
     * @param path
     *            the path
     * @return the image descriptor
     */
    public static ImageDescriptor getImageDescriptor(String path) {
        return AbstractUIPlugin.imageDescriptorFromPlugin(
                "com.raytheon.viz.ui", path);
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

        return prefs;
    }
}
