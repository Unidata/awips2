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
package com.raytheon.uf.viz.app.launcher.runner;

import java.io.File;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.util.RunProcess;
import com.raytheon.uf.viz.app.launcher.bundle.Application;
import com.raytheon.uf.viz.app.launcher.bundle.Launcher;
import com.raytheon.uf.viz.app.launcher.bundle.Settings;
import com.raytheon.uf.viz.app.launcher.dialogs.AppArgsDlg;
import com.raytheon.uf.viz.app.launcher.exception.AppLauncherException;
import com.raytheon.uf.viz.app.launcher.utilities.AppLauncherUtilities;

/**
 * Application runner implementation. Executes an arbitrary application as a
 * separate executable.
 * <P>
 * Usage:
 * 
 * <pre>
 *    AppRunner runner = new AppRunner();
 *    Launcher launcher = ...
 *    runner.initialize(launcher);
 *    runner.execute();
 * </pre>
 * 
 * or
 * 
 * <pre>
 *    Launcher launcher = ...
 *    AppRunner runner = new AppRunner(launcher);
 *    runner.execute();
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009 2081       mfegan     Initial creation
 * Sep 19, 2011 10955      rferrel    Use RunProcess
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */
public class AppRunner {
    /** array representing the runtime environment for the application */
    private String[] environment = null;

    /** the complete command needed to execute the application */
    private String command = null;

    /** the run directory for the application */
    private String runDir = null;

    /**
     * Constructor. Constructs an empty Application launcher. when this
     * constructor is used, the Application Launcher must be initialized using
     * {@link #initialize(String, String, String[])} or
     * {@link #initialize(Launcher)}
     */
    public AppRunner() {
        super();
    }

    /**
     * Constructor. Creates an Application Launcher based on the specified
     * Launcher bundle.
     * 
     * @param launcher
     *            contains the launch bundle to execute
     */
    public AppRunner(Launcher launcher) {
        super();
        initialize(launcher);
    }

    /**
     * Constructor. Creates an Application Launcher for the specified command
     * line, run directory and environment. Note that the command line must
     * include any arguments.
     * 
     * @param command
     *            the complete command line to run
     * @param appRunDir
     *            the run directory
     * @param appEnv
     *            the execution environment
     */
    public AppRunner(String command, String appRunDir, String[] appEnv) {
        super();
        initialize(command, appRunDir, appEnv);
    }

    /**
     * Initializes the Application Launcher based on the contents of the
     * Launcher bundle. Basically extracts the command, run directory and
     * environment from the bundle and then calls
     * {@link #initialize(String, String, String[])}
     * 
     * @param launcher
     *            bundle describing the application to launch
     */
    public void initialize(Launcher launcher) {
        String appName = launcher.getTitle();
        Application application = launcher.getApplication();
        String command = null;
        String appRunDir = null;
        int additional = 0;
        String[] appEnv = null;
        Settings settings = launcher.getSettings();
        if (settings != null) {
            appEnv = settings.getSettings();
        } else {
            appEnv = Settings.getInstance().getSettings();
        }
        if (application != null) {
            additional = application.getAdditionalArgCount();
            if (additional != 0) {
                boolean optional = application.areArgsOptional();
                Shell shell = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell();
                AppArgsDlg dlg = new AppArgsDlg(shell, appName, additional,
                        optional);
                Object newArgs = dlg.open();
                if (newArgs != null && newArgs instanceof String[]) {
                    application.addArguments((String[]) newArgs);
                }
            }
            if (appEnv != null) {
                application.updateApplicationData(appEnv);
            }
            command = application.getCommandLine();
            appRunDir = application.getRunDir();
        }
        initialize(command, appRunDir, appEnv);
    }

    /**
     * Shared initialization routine. Sets the instance variables to specified
     * values.
     * 
     * @param command
     *            the complete command line to run
     * @param appRunDir
     *            the run directory
     * @param appEnv
     *            the execution environment
     */
    public void initialize(String command, String appRunDir, String[] appEnv) {
        this.command = command;
        if (appRunDir == null) {
            this.runDir = "";
        } else {
            this.runDir = appRunDir;
        }
        if (appEnv == null) {
            this.environment = new String[] {};
        } else {
            this.environment = appEnv;
        }
    }

    /**
     * Runs the previously configured application.
     * 
     * @throws AppLauncherException
     *             if any error occurs
     */
    public void execute() throws AppLauncherException {
        File runDir;
        if (this.runDir.isEmpty()) {
            runDir = null;
        } else {
            runDir = new File(this.runDir);
        }
        try {
            // DR#10955
            RunProcess.getRunProcess().exec(this.command, this.environment,
                    runDir);
        } catch (Exception e) {
            String msg = "Execution failed {" + this.command + "}";
            throw new AppLauncherException(msg, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String format = "App: %s\n\nArgs: %s\nEnv: %s\n\nRun Dir: %s";
        String[] parts = this.command.split(" ", 2);
        String appName = null;
        String appArgs = null;
        switch (parts.length) {
        case 2:
            appName = parts[0];
            appArgs = parts[1];
            break;
        case 1:
            appName = parts[0];
            appArgs = "";
            break;
        default:
            appName = "";
            appArgs = "";
        }
        return String.format(format, appName, appArgs,
                AppLauncherUtilities.join(environment), runDir);
    }
}
