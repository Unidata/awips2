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
package com.raytheon.uf.viz.app.launcher.handlers;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.osgi.framework.Bundle;

import com.raytheon.uf.viz.app.launcher.bundle.Environment;
import com.raytheon.uf.viz.app.launcher.bundle.Launcher;
import com.raytheon.uf.viz.app.launcher.runner.AppRunner;
import com.raytheon.uf.viz.app.launcher.utilities.AppLauncherUtilities;
import com.raytheon.uf.viz.core.VizApp;

/**
 * Menu/tool bar command handler that launches an external application.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009 2081       mfegan     Initial creation.
 * Apr 24, 2009 2088       mduff      Added varargs to execute method.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class AppLauncherHandler extends AbstractHandler {
    private static final String LOCALIZATION_ROOT = "LOCALIZATION_ROOT";

    private static final String HYDRO_APPS_DIR = "HYDRO_APPS_DIR";

    private static final String HYDRO_APPS_DIR_LOC = "/awips/hydroapps";

    private static final String PGSQL_DRIVER = "PGSQL_DRIVER_DIR";

    private static final String PGSQL_DRIVER_LOC = "/lib/dependencies/org.postgres";

    private static final String APPS_DIR = "apps_dir";

    private static final String EDEX_HOME = "EDEX_HOME";

    /**
     * optional argument which can be provided when calling execute(String)
     * directly
     */
    private String argument = null;

    /**
     * Constructor.
     */
    public AppLauncherHandler() {
        // intentionally empty
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        String bundleLocation = event.getParameter("bundleLocation");
        return execute(bundleLocation);
    }

    /**
     * Executes the App Launcher for the specified bundle location. This
     * override is intended to allow a launching an application from another
     * menu handler. An additional argument may be provided by first calling
     * {@link #setArgument(String)}.
     * 
     * @param bundleLocation
     *            relative path to the application bundle to launch.
     * 
     * @return always returns null
     * 
     * @throws ExecutionException
     *             if an error has occurred
     */
    public Object execute(String bundleLocation) throws ExecutionException {
        return execute(bundleLocation, argument);
    }

    /**
     * Executes the App Launcher for the specified bundle location. This
     * override is intended to allow a launching an application from another
     * menu handler.
     * 
     * @param bundleLocation
     *            relative path to the application bundle to launch.
     * @param additional
     *            additional argument to add to the launch
     * 
     * @return always returns null
     * 
     * @throws ExecutionException
     *             if an error has occurred
     */
    public Object execute(String bundleLocation, String... additional)
            throws ExecutionException {
        try {
            Launcher launcher = AppLauncherUtilities
                    .importLauncherBundle(bundleLocation);
            if (additional != null) {
                for (int i = 0; i < additional.length; i++) {
                    if (additional[i] != null) {
                        launcher.getApplication().addArgument(additional[i]);
                    }
                }
            }
            Environment environment = launcher.getSettings().getEnvironment();

            /*
             * need to add specific values to the environment -- these are
             * internal to CAVE
             */
            Map<String, String> sysEnv = System.getenv();

            String hydroAppsDir = VizApp.getDataDir() + File.separator
                    + "hydroapps";
            environment.addValue(HYDRO_APPS_DIR, hydroAppsDir);
            environment.addValue(APPS_DIR, hydroAppsDir);
            String edexHome = sysEnv.get(EDEX_HOME) == null ? hydroAppsDir
                    : sysEnv.get(EDEX_HOME);
            environment.addValue(EDEX_HOME, edexHome);

            String pgDriverLoc = "";
            for (Bundle bundle : com.raytheon.uf.viz.core.Activator
                    .getDefault().getContext().getBundles()) {
                if ("org.postgres".equals(bundle.getSymbolicName())) {
                    try {
                        URL url = FileLocator.find(bundle, new Path("."), null);
                        url = FileLocator.resolve(url);
                        File tmp = new File(url.getPath());
                        pgDriverLoc = tmp.getCanonicalPath();
                    } catch (MalformedURLException e) {
                        System.out
                                .println("Unable to find Postgres plugin, using default of "
                                        + hydroAppsDir + "/" + PGSQL_DRIVER_LOC);
                        pgDriverLoc = hydroAppsDir + "/" + PGSQL_DRIVER_LOC;
                    }
                    break;
                }
            }

            System.out.println(pgDriverLoc);

            String driverPath = sysEnv.get(PGSQL_DRIVER) == null ? pgDriverLoc
                    : sysEnv.get(PGSQL_DRIVER);
            environment.addValue(PGSQL_DRIVER, driverPath);

            AppRunner runner = new AppRunner(launcher);
            runner.execute();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Returns the value of the (optional) argument attribute.
     */
    public String getArgument() {
        return argument;
    }

    /**
     * Sets the value of the (optional) argument attribute.
     */
    public void setArgument(String argument) {
        this.argument = argument;
    }
}
