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
package com.raytheon.viz.texteditor.scripting.runner;

import java.io.File;
import java.nio.file.Path;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonLocalizationPathBuilder;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;

import jep.Jep;
import jep.JepConfig;
import jep.JepException;
import jep.NamingConventionClassEnquirer;

/**
 * Thread that runs a single TextWS Python script
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 29, 2009           mfegan    Initial creation
 * Apr 26, 2015  4259     njensen   Updated for new JEP API
 * Apr 28, 2016  5236     njensen   Use Jep redirectOutput for python prints
 * Jan 02, 2018  6804     tgurney   Convert to Thread; rename from
 *                                  TextWsPythonScript
 * Nov 05, 2018  6804     tgurney   Add waitFor()
 * Feb 26, 2019  7746     randerso  Change to use Path instead of String for
 *                                  TextWS script path.
 * Mar 04, 2019  7601     tgurney   Add script path to the Python module
 *                                  globals. Add doContinue method impl.
 *                                  Add script start notifier. Remove waitFor
 *
 * </pre>
 *
 * @author mfegan
 */

public class TextWsScriptThread extends Thread
        implements ITextWsScriptController {
    private static final String BASE_PATH = "textws" + IPathManager.SEPARATOR
            + "scripting";

    private static final String BASE_SCRIPT = BASE_PATH + IPathManager.SEPARATOR
            + "twsScripting.py";

    private static final String EDITOR_FMT = "editor = \"Text %s\"";

    private static final String SCRIPT_CANCELLED = "ScriptCancelled";

    private Path scriptPath;

    private IScriptRunnerObserver observer;

    private String token;

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    public TextWsScriptThread(String threadName, Path scriptPath,
            IScriptRunnerObserver observer, String token) {
        super(threadName);
        this.scriptPath = scriptPath;
        this.observer = observer;
        this.token = token;
    }

    @Override
    public void run() {
        try {
            observer.scriptStarted();

            File bundle = PathManagerFactory.getPathManager()
                    .getStaticFile(BASE_SCRIPT);
            String includePath = new PythonLocalizationPathBuilder()
                    .append(BASE_PATH, LocalizationType.CAVE_STATIC)
                    .getPathString();
            JepConfig jepConfig = new JepConfig().setInteractive(false)
                    .setIncludePath(includePath)
                    .setClassLoader(TextWsScriptThread.class.getClassLoader())
                    .setClassEnquirer(new NamingConventionClassEnquirer())
                    .setRedirectOutputStreams(true);
            boolean cancelled = false;

            try (Jep jep = new Jep(jepConfig)) {
                jep.runScript(bundle.getPath());
                jep.set("observer", observer);
                jep.set("scriptPath", scriptPath.toString());
                jep.eval(String.format(EDITOR_FMT, token));
                jep.runScript(scriptPath.toString());
                statusHandler.info("Ran script " + scriptPath);
            } catch (JepException e) {
                if (e.getMessage().contains(SCRIPT_CANCELLED)) {
                    cancelled = true;
                } else {
                    statusHandler.warn("Error in TextWS script: " + scriptPath,
                            e);
                    throw e;
                }
            } finally {
                if (observer.isScriptError()) {
                    observer.writeErrMsg("Text WorkStation Scripting Error");
                    observer.writeText(
                            "--- Script Error!: " + scriptPath + " ---\n");
                } else if (cancelled) {
                    observer.writeText(
                            "--- Script Cancelled: " + scriptPath + " ---\n");
                } else {
                    observer.writeText(
                            "--- Script Complete: " + scriptPath + " ---\n");
                }
            }
        } catch (Throwable t) {
            observer.writeText("--- Script Error!: " + scriptPath + " ---\n");
            observer.showErrorMessage(
                    "Text WorkStation Scripting Error\n" + t.getMessage(), t);
        } finally {
            if (!observer.isScriptError()) {
                observer.showScriptStatus(
                        "Script \"" + scriptPath + "\" complete");
            }
            observer.scriptComplete();

            TextWsScriptThreadManager.getInstance().threadFinished(this);
        }
    }

    @Override
    public void cancel() {
        observer.cancel();
    }

    @Override
    public void doContinue() {
        observer.doContinue();
    }
}
