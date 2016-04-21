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
package com.raytheon.uf.edex.remote.script;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.remote.script.RemoteScriptConstants;
import com.raytheon.uf.common.remote.script.RemoteScriptRequest;
import com.raytheon.uf.common.remote.script.RemoteScriptRunRequest;
import com.raytheon.uf.common.remote.script.RemoteScriptRunResponse;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.RunProcess;

/**
 * Handler to Run a remote script and return the results.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2014 2743       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class RemoteScriptRunHandler extends AbstractRemoteScriptHandler {

    /**
     * Time to back off from socket time out to allow completion of a timeout
     * script.
     */
    private static final int BACKOFF_MSEC = 100;

    /**
     * Resource timeout.
     */
    private final int defaultTimeoutSec;

    /**
     * Resource flag value.
     */
    private final boolean defaultUseStdErrFlag;

    /**
     * Resource set up exit status.
     */
    private final int defaultSetupExit;

    /**
     * Constructor setup roleId and resource property values.
     */
    public RemoteScriptRunHandler() {
        // The role id in the common remoteScriptAdminRoles.xml
        super("remote.script.run");

        // Set up default values.
        String defaultTimeoutStr = System.getProperty(
                RemoteScriptConstants.scriptTimeoutKey,
                RemoteScriptConstants.scriptTimeoutDefault);
        int defaultTimeoutSec = -1;
        try {
            defaultTimeoutSec = Integer.parseInt(defaultTimeoutStr);
        } catch (NumberFormatException ex) {
            defaultTimeoutSec = -1;
        } finally {
            if (defaultTimeoutSec <= 0) {
                defaultTimeoutSec = Integer
                        .parseInt(RemoteScriptConstants.scriptTimeoutDefault);
            }
            this.defaultTimeoutSec = defaultTimeoutSec;
        }

        String defaultUseStdErr = System.getProperty(
                RemoteScriptConstants.scriptUseStdErrKey,
                RemoteScriptConstants.scriptUseStdErrDefault);
        this.defaultUseStdErrFlag = Boolean.parseBoolean(defaultUseStdErr);

        String defaultSetupErrorStr = System.getProperty(
                RemoteScriptConstants.scriptSetupErrrorKey,
                RemoteScriptConstants.scriptUseStdErrDefault);
        int defaultSetupExit = 0;
        try {
            defaultSetupExit = Integer.parseInt(defaultSetupErrorStr);

        } catch (NumberFormatException ex) {
            defaultSetupExit = 0;
        } finally {
            if (defaultSetupExit <= 0) {
                defaultSetupExit = Integer
                        .parseInt(RemoteScriptConstants.scriptSetUpErrorDefault);
            }
            this.defaultSetupExit = defaultSetupExit;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(RemoteScriptRequest request) throws Exception {
        return super.handleRequest(request);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.remote.script.RemoteScriptHandler#performRequest
     * (com.raytheon.uf.common.remote.script.RemoteScriptRequest)
     */
    public Object performRequest(RemoteScriptRequest request) {
        RemoteScriptRunRequest req = (RemoteScriptRunRequest) request;
        RemoteScriptRunResponse result = new RemoteScriptRunResponse();

        Map<String, String> propMap = req.getPropertyMap();

        String timeoutValue = propMap
                .get(RemoteScriptConstants.scriptTimeoutKey);
        int timeoutSec = -1;
        if (timeoutValue == null) {
            timeoutSec = defaultTimeoutSec;
        } else {
            try {
                timeoutSec = Integer.parseInt(timeoutValue);
                if (timeoutSec <= 0) {
                    timeoutSec = defaultTimeoutSec;
                }
            } catch (NumberFormatException ex) {
                statusHandler.handle(Priority.PROBLEM,
                        String.format("Bad timeout value %s", timeoutValue));
                timeoutSec = defaultTimeoutSec;
            } finally {
                if (timeoutSec <= 0) {
                    timeoutSec = defaultTimeoutSec;
                }
            }
        }

        long timeout = timeoutSec * TimeUtil.MILLIS_PER_SECOND;

        String useStdErrString = propMap
                .get(RemoteScriptConstants.scriptUseStdErrKey);

        boolean useStdErr = defaultUseStdErrFlag;
        if (useStdErrString != null) {
            useStdErr = Boolean.parseBoolean(useStdErrString);
        }

        String setupExitValue = propMap
                .get(RemoteScriptConstants.scriptSetupErrrorKey);
        int setupExit = -1;

        if (setupExitValue == null) {
            setupExit = defaultSetupExit;
        } else {

            try {
                setupExit = Integer.parseInt(setupExitValue);
            } catch (NumberFormatException ex) {
                statusHandler.handle(Priority.PROBLEM, String.format(
                        "Bad setup Error exit value %s", setupExitValue));
                setupExit = defaultSetupExit;
            } finally {
                if (setupExit <= 0) {
                    setupExit = defaultSetupExit;
                }
            }
        }

        List<String> arguments = req.getScriptArguments();
        String script = req.getScript();
        LocalizationContext context = req.getContext();

        IPathManager pm = PathManagerFactory.getPathManager();
        String name = scriptsDirectory + IPathManager.SEPARATOR + script;
        LocalizationFile lFile = pm.getLocalizationFile(context, name);
        File file = lFile.getFile();
        File dir = file.getParentFile();

        if (!file.canExecute()) {
            String message = String.format("Not an executable script: \"%s\".",
                    lFile);
            return sendMessage(result, message, useStdErr, setupExit);
        }

        int maxTimeout = HttpClient.getInstance().getSocketTimeout()
                - BACKOFF_MSEC;
        if (maxTimeout <= 0) {
            String message = String
                    .format("HttpClient's socket timeout of %d msec not enough time to run a remote script.",
                            HttpClient.getInstance().getSocketTimeout());
            return sendMessage(result, message, useStdErr, setupExit);
        } else if (timeout > maxTimeout) {
            timeout = maxTimeout;
        }

        List<String> args = new ArrayList<String>();
        args.add(file.getAbsolutePath());
        if (arguments != null && (arguments.size() > 0)) {
            args.addAll(arguments);
        }

        ProcessBuilder pb = new ProcessBuilder(args);
        pb.redirectErrorStream(!useStdErr);
        pb.directory(dir);
        Process p = null;
        RunProcess rp = RunProcess.getRunProcess();
        String errorMessage = null;

        // TODO - The timeout/destroy should be placed in RunProcess along with
        // limiting the size of stdout/stderr.
        try {
            p = pb.start();
            rp.setProcess(p);
            synchronized (rp) {
                rp.wait(timeout);
                if (!rp.isExecComplete()) {
                    p.destroy();
                    result.setTimedOut(true);
                    rp.notify();
                    errorMessage = "Script timed out.";
                }
            }
        } catch (Exception ex) {
            errorMessage = "Problem running script: "
                    + ex.getLocalizedMessage().trim();
            statusHandler.handle(Priority.PROBLEM, errorMessage, ex);

        } finally {
            if (p != null) {
                result.setOutput(rp.getStdout());
                result.setError(rp.getStderr());
            }
            result.setExitStatus(rp.waitFor());
            if (errorMessage != null) {
                if (useStdErr) {
                    result.setError(result.getError() + "\n" + errorMessage);
                } else {
                    result.setOutput(result.getOutput() + "\n" + errorMessage);
                }
            }
        }
        return result;
    }

    /**
     * Report a problem in running the script.
     * 
     * @param result
     * @param message
     * @param useStdErr
     * @param setupExit
     * @return result
     */
    private RemoteScriptRunResponse sendMessage(RemoteScriptRunResponse result,
            String message, boolean useStdErr, int setupExit) {
        statusHandler.handle(Priority.PROBLEM, message);

        if (useStdErr) {
            result.setError(message);
        } else {
            result.setOutput(message);
        }

        result.setExitStatus(setupExit);
        return result;
    }
}
