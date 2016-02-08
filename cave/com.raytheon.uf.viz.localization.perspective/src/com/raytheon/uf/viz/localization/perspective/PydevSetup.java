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
package com.raytheon.uf.viz.localization.perspective;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.python.pydev.core.IInterpreterInfo;
import org.python.pydev.core.IInterpreterManager;
import org.python.pydev.editor.PydevShowBrowserMessage;
import org.python.pydev.plugin.PydevPlugin;
import org.python.pydev.plugin.preferences.PydevRootPrefs;
import org.python.pydev.runners.SimplePythonRunner;
import org.python.pydev.shared_core.string.StringUtils;
import org.python.pydev.shared_core.structure.Tuple;
import org.python.pydev.ui.pythonpathconf.InterpreterInfo;

import com.python.pydev.analysis.AnalysisPlugin;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Utilities that configure pydev to work properly in the Viz Localization
 * Perspective. Works around deficiencies in the pydev code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 01, 2013  1967     njensen     Initial creation
 * Oct 11, 2013  2441     bsteffen    Make initialize async.
 * May 09, 2014  3075     njensen     Updates for pydev 3.4.1
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PydevSetup {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PydevSetup.class);

    /**
     * Prepares pydev to be used in the localization perspective with as few
     * annoyances as possible.
     */
    public static void preparePydev() {
        preventPydevEclipsePopup();
        preventFundingPopup();
        initializeInterpreterAsync();
    }

    public static void initializeInterpreterAsync() {
        new Job("Preparing python interpreter.") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                initializeInterpreter(monitor);
                return Status.OK_STATUS;
            }
        }.schedule();
    }

    /**
     * Attempts to initialize the python interpreter based on the PATH
     * environment, saving the user the trouble of initializing it through the
     * preference page.
     */
    public static void initializeInterpreter(IProgressMonitor monitor) {
        try {
            // Attempt the initialization twice before reporting errors,
            // sometimes it can fail unexpectedly and recover a second time.
            // Goal is to auto setup python interpreter to use for editing
            // python files
            for (int i = 0; i < 2; ++i) {
                boolean retry = i > 0;
                // Setup python environment for pydev
                IInterpreterManager mgr = PydevPlugin
                        .getPythonInterpreterManager();
                String persistedStr = mgr.getPersistedString();
                if ((persistedStr == null) || "".equals(persistedStr.trim())) {
                    IInterpreterInfo iinfo = null;
                    String pathToFile = null;
                    String LD_PATH = System.getenv("PATH");
                    String[] folders = LD_PATH.split(File.pathSeparator); // Win32
                    for (String folder : folders) {
                        File python = new File(folder, "python");
                        if (python.exists() && python.isFile()
                                && python.canExecute()) {
                            pathToFile = python.getAbsolutePath();
                            break;
                        }
                    }

                    if (pathToFile != null) {
                        try {
                            // Taken from pydev source to get rid of UI prompt
                            File script = PydevPlugin
                                    .getScriptWithinPySrc("interpreterInfo.py");
                            Tuple<String, String> outTup = new SimplePythonRunner()
                                    .runAndGetOutputWithInterpreter(pathToFile,
                                            script.getAbsolutePath(), null,
                                            null, null,
                                            new NullProgressMonitor(), null);
                            iinfo = InterpreterInfo
                                    .fromString(outTup.o1, false);
                            // end taken
                        } catch (Throwable e) {
                            if (retry) {
                                throw e;
                            }
                            continue;
                        }
                        if (iinfo == null) {
                            if (retry) {
                                throw new Exception(
                                        "Could not generate python info, python editors may not function 100%");
                            } else {
                                continue;
                            }
                        }
                        IInterpreterInfo[] infoList = new IInterpreterInfo[] { iinfo };
                        setupStupidFile(mgr, iinfo);
                        /*
                         * Calling setInfo will not begin a task, but it will
                         * complete work. There isn't any way to get the amount
                         * of work that will be completed so this is an
                         * estimate. 3832 is the amount of work was being done
                         * at the time of this writing.
                         */
                        monitor.beginTask("Examining python path.", 3832);
                        Set<String> interpretersToRestore = new HashSet<String>();
                        interpretersToRestore.add(pathToFile);
                        mgr.setInfos(infoList, interpretersToRestore, monitor);
                    } else {
                        if (retry) {
                            throw new Exception(
                                    "Could not find python on PATH, be sure to set environment variable");
                        } else {
                            continue;
                        }
                    }
                }

                // We made it here? success!
                break;
            }
        } catch (Throwable t) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Error setting up python interpreter: "
                            + t.getLocalizedMessage(), t);
        }

    }

    /**
     * Initializes a file to prevent pydev from sending out an error message
     * that is not actually an error. Pydev looks for a particular file and if
     * it does not exist, sends an error. However, if the file exists but has
     * the wrong contents, it only sends out an info. Either way, pydev
     * continues on and takes care of things, so the error message is useless.
     * This code is derived from pydev's AdditionalSystemInterpreterInfo
     * constructor.
     * 
     * @param manager
     * @param info
     */
    private static void setupStupidFile(IInterpreterManager manager,
            IInterpreterInfo info) {
        try {
            File base = null;
            IPath stateLocation = AnalysisPlugin.getDefault()
                    .getStateLocation();
            base = stateLocation.toFile();

            String additionalInfoInterpreter = info.getExecutableOrJar();
            File file = new File(base, manager.getManagerRelatedName() + "_v1_"
                    + StringUtils.md5(additionalInfoInterpreter));

            if (!file.exists()) {
                file.mkdirs();

                File persistingFolder = file;
                File persistingLocation = new File(persistingFolder,
                        manager.getManagerRelatedName() + ".pydevsysteminfo");
                FileUtil.bytes2File(new byte[0], persistingLocation);
            }
        } catch (IOException e) {
            // no need to log this notably since this whole method only exists
            // to prevent a stupid error message from being seen
            statusHandler.debug(e.getLocalizedMessage());
        }
    }

    /**
     * Sets the preference value to prevent pydev from popping up the dialog
     * that requests money for the developer's personal projects
     */
    public static void preventFundingPopup() {
        IPreferenceStore pydevPrefStore = PydevPlugin.getDefault()
                .getPreferenceStore();
        pydevPrefStore.setValue(PydevShowBrowserMessage.PYDEV_FUNDING_SHOWN,
                "true");
    }

    /**
     * Sets the preference values to prevent pydev from popping up a dialog
     * about some settings for running python files in Eclipse. This only
     * pertains to running python inside Eclipse, not viz, so we don't want the
     * popup.
     */
    public static void preventPydevEclipsePopup() {
        PydevRootPrefs.setCheckPreferredPydevSettings(false);
        IEclipsePreferences debug = InstanceScope.INSTANCE
                .getNode("org.eclipse.debug.ui");
        debug.put("org.eclipse.debug.ui.wait_for_build", "never");
        debug.put("org.eclipse.debug.ui.build_before_launch", "false");
        debug.put("org.eclipse.debug.ui.UseContextualLaunch", "");
    }
}
