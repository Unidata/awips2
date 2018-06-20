package com.raytheon.uf.viz.alertviz.python;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.python.concurrent.IPythonExecutor;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;

import jep.JepException;

/**
 * This is the main class for the alert visualization.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date        Ticket#    Engineer      Description
 * ----------  ---------  ------------  --------------------------
 * 2017-03-24  DR 16985   D. Friedman   Restore Python script functionality
 *
 * </pre>
 *
 */
public class AlertVizPythonExecutor implements IPythonExecutor<PythonScript, Object> {

    private static String METHOD_NAME = "process";

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertVizPythonExecutor.class, "GDN_ADMIN",
                    "GDN_ADMIN");

    StatusMessage statMsg;
    AlertMetadata alertMetadata;
    TrayConfiguration gConfig;

    public AlertVizPythonExecutor(StatusMessage statMsg, AlertMetadata alertMetadata, TrayConfiguration gConfig) {
        this.statMsg = statMsg;
        this.alertMetadata = alertMetadata;
        this.gConfig = gConfig;
    }

    @Override
    public Object execute(PythonScript script) throws JepException {
        Map<String, Object> args = new HashMap<>();
        args.put("statusMessage", statMsg);
        args.put("alertMetadata", alertMetadata);
        args.put("globalConfiguration", gConfig);
        try {
            return script.execute(METHOD_NAME, args);
        } catch (JepException e) {
            statusHandler.error(String.format("Error executing AlertViz script %s:",
                    alertMetadata.getPythonScript()), e);
            return null;
        }
    }

}
