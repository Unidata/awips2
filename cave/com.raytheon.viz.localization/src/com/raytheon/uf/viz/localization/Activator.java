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
package com.raytheon.uf.viz.localization;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.team.core.RepositoryProvider;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin implements BundleActivator {

    // The plug-in ID
    public static final String PLUGIN_ID = "com.raytheon.uf.viz.localization";

    // The shared instance
    private static Activator plugin;

    private IProject PROJECT;

    public static final String LOCALIZATION_PROJECT = ".localization";

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
        plugin = this;

        try {
            IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
            PROJECT = root.getProject(LOCALIZATION_PROJECT);
            if (PROJECT.exists()) {
                if (PROJECT.isOpen() == false) {
                    PROJECT.open(null);
                }
            } else {
                PROJECT.create(ResourcesPlugin.getWorkspace()
                        .newProjectDescription(LOCALIZATION_PROJECT), null);
                PROJECT.open(null);
                RepositoryProvider.map(PROJECT,
                        LocalizationRepositoryProvider.ID);
                ResourcesPlugin.getWorkspace().save(true, null);
            }
            root.refreshLocal(IResource.DEPTH_INFINITE, null);
        } catch (CoreException e) {
            e.printStackTrace();
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
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
        ResourcesPlugin.getWorkspace().save(true, null);
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

    public IProject getLocalizationProject() {
        return PROJECT;
    }
}
