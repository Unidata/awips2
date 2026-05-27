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
package com.raytheon.viz.aviation.editor.tools;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.aviation.monitor.AvnPyUtil;

import jep.JepConfig;
import jep.JepException;

/**
 * Interface to python for running the taf editor tools
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 12, 2009            njensen     Initial creation
 * Mar 29. 2010  4904      randerso    Fixed getting list of tools
 * Jan 13, 2017  5959      njensen     Cleaned up warnings
 * Mar 22, 2017  6183      tgurney     Move python files to common_static
 * Jun 03, 2019  7852      dgilling    Update code for jep 3.8.
 *
 * </pre>
 *
 * @author njensen
 */

public class ToolPython {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ToolPython.class);

    private static String INSTANCE_NAME = "toolInst";

    private PythonScript python;

    public ToolPython() {
        python = ToolPython.initializePython();
    }

    @SuppressWarnings("unchecked")
    private static final synchronized PythonScript initializePython() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        File runner = pathMgr
                .getStaticFile("aviation/python/toolpy/TafToolInterface.py");
        String filePath = runner.getPath();
        LocalizationContext baseCtx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        // need getFile to pull whole python dir from localization server
        pathMgr.getLocalizationFile(baseCtx, "aviation/python").getFile();
        String includePath = PyUtil.buildJepIncludePath(runner.getParentFile()
                .getPath(), runner.getParentFile().getParentFile().getPath(),
                AvnPyUtil.getLoggingHandlerDir(), AvnPyUtil.getPointDataDir(),
                AvnPyUtil.getCommonPythonDir());
        HashMap<String, Object> args = new HashMap<>();
        args.put("scriptPath", runner.getParent());
        PythonScript python = null;
        try {
            List<String> list = new ArrayList<>();
            list.add("sys.argv = ['AvnFPSTool']");
            JepConfig config = new JepConfig().setIncludePath(includePath)
                    .setClassLoader(ToolPython.class.getClassLoader());
            python = new PythonScript(config, filePath, list);
            python.instantiatePythonClass(INSTANCE_NAME, "TafToolInterface",
                    args);
            List<String> errors = (List<String>) python.execute(
                    "getStartupErrors", INSTANCE_NAME, null);
            if (!errors.isEmpty()) {
                statusHandler.warn("AvnFPS tool startup errors: "
                        + String.join("\n", errors));
            }
        } catch (JepException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }
        return python;
    }

    @SuppressWarnings("unchecked")
    public final String[] getTafTools() {
        try {
            Object result = python.execute("getScripts", INSTANCE_NAME, null);
            List<String> scripts = (List<String>) result;
            Collections.sort(scripts);
            return scripts.toArray(new String[scripts.size()]);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
        return new String[0];
    }

    public final String runTool(String toolName, String bbb, List<String> fcsts) {
        try {
            Map<String, Object> args = new HashMap<>();
            args.put("moduleName", toolName);
            args.put("bbb", bbb);
            args.put("fcsts", fcsts);

            Object result = python.execute("runTool", INSTANCE_NAME, args);
            return (String) result;
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }

        return null;

    }

    public final synchronized void dispose() {
        if (python != null) {
            try {
                python.dispose();
            } catch (JepException e) {
                statusHandler.debug("Failed to dispose script instance.", e);
            }
            python = null;
        }
    }

}
