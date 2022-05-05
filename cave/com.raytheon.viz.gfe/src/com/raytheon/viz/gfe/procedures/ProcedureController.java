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
package com.raytheon.viz.gfe.procedures;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.BaseGfePyController;
import com.raytheon.viz.gfe.core.DataManager;

import jep.JepException;

/**
 * Manages and runs the procedures in the system.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 05, 2008           njensen   Initial creation
 * Jan 08, 2013  1486     dgilling  Support changes to BaseGfePyController.
 * Feb 12, 2013  1597     randerso  Added logging to support GFE Performance
 *                                  metrics
 * Jul 27, 2015  4263     dgilling  Refactor and make abstract.
 * Feb 13, 2018  6906     randerso  Merge metadata and runner controllers into a
 *                                  single controller
 * Dec 12, 2018  6906     randerso  Kick off metadata update immediately on file
 *                                  change
 * Jun 03, 2019  7852     dgilling  Update code for jep 3.8.
 *
 * </pre>
 *
 * @author njensen
 */

public class ProcedureController extends BaseGfePyController {

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    protected final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    private final LocalizationFile proceduresDir;

    private final LocalizationFile utilitiesDir;

    /**
     * Constructor
     *
     * @param filePath
     * @param anIncludePath
     * @param classLoader
     * @param dataManager
     * @throws JepException
     */
    public ProcedureController(String filePath, String anIncludePath,
            ClassLoader classLoader, DataManager dataManager)
            throws JepException {
        super(filePath, anIncludePath, classLoader, dataManager, "Procedure");

        String scriptPath = GfePyIncludeUtil.getProceduresIncludePath();

        jep.eval(INTERFACE + " = ProcedureInterface('" + scriptPath + "')");

        List<String> errors = getStartupErrors();
        if (!errors.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append("Error importing the following procedures:\n");
            for (String s : errors) {
                sb.append(s);
                sb.append("\n");
            }
            statusHandler.handle(Priority.PROBLEM, sb.toString());
        }
        jep.eval("import TimeRange");
        jep.eval("import sys");
        jep.eval("sys.argv = ['ProcedureInterface']");

        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE);
        proceduresDir = GfePyIncludeUtil.getProceduresLF(baseCtx);
        proceduresDir.addFileUpdatedObserver(this);
        utilitiesDir = GfePyIncludeUtil.getUtilitiesLF(baseCtx);
        utilitiesDir.addFileUpdatedObserver(this);

    }

    @Override
    public void dispose() {
        try {
            super.dispose();
        } catch (JepException e) {
            statusHandler.debug("Failed to dispose script instance.", e);
        }
        proceduresDir.removeFileUpdatedObserver(this);
        utilitiesDir.removeFileUpdatedObserver(this);
    }

    /**
     * Runs the procedure's execute method
     *
     * @param procedureName
     *            the name of the procedure being executed
     * @param args
     *            the arguments to the tool
     * @return the result of the tool's execution
     * @throws JepException
     */
    public Object executeProcedure(String procedureName,
            Map<String, Object> args) throws JepException {
        ITimer timer = TimeUtil.getTimer();
        timer.start();

        if (!isInstantiated(procedureName)) {
            instantiatePythonScript(procedureName);
        }
        args.put(PyConstants.METHOD_NAME, "execute");
        args.put(PyConstants.MODULE_NAME, procedureName);
        args.put(PyConstants.CLASS_NAME, pythonClassName);

        statusHandler.debug("Running procedure: " + procedureName);

        internalExecute("runProcedure", INTERFACE, args);
        Object result = getExecutionResult();

        timer.stop();
        perfLog.logDuration("Running procedure " + procedureName,
                timer.getElapsedTime());
        return result;
    }

    @Override
    protected void evaluateArgument(String argName, Object argValue)
            throws JepException {
        if (argValue instanceof TimeRange) {
            jep.set("timeRangeTemp", argValue);
            jep.eval(argName + " = TimeRange.TimeRange(timeRangeTemp)");
        } else if (argName.startsWith("varDict")) {
            jep.eval(argName + " = varDict");
        } else {
            super.evaluateArgument(argName, argValue);
        }
    }

    @Override
    public void fileUpdated(FileUpdatedMessage message) {
        super.fileUpdated(message);

        // kick off metadata update
        this.dataMgr.getProcedureInterface().getMetadata();
    }
}
