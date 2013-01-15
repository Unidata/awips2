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

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.BaseGfePyController;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * Manages and runs the procedures in the system
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2008             njensen      Initial creation
 * Jan 8, 2013  1486       dgilling     Support changes to BaseGfePyController.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ProcedureController extends BaseGfePyController {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcedureController.class);

    private LocalizationFile proceduresDir;

    private LocalizationFile utilitiesDir;

    public ProcedureController(String filePath, String anIncludePath,
            ClassLoader classLoader, DataManager dataManager)
            throws JepException {
        super(filePath, anIncludePath, classLoader, dataManager, "Procedure");

        LocalizationContext baseCtx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.BASE);

        proceduresDir = GfePyIncludeUtil.getProceduresLF(baseCtx);
        proceduresDir.addFileUpdatedObserver(this);

        utilitiesDir = GfePyIncludeUtil.getUtilitiesLF(baseCtx);
        utilitiesDir.addFileUpdatedObserver(this);

        String scriptPath = GfePyIncludeUtil.getProceduresIncludePath();

        jep.eval(INTERFACE + " = ProcedureInterface('" + scriptPath + "')");

        List<String> errors = getStartupErrors();
        if (errors.size() > 0) {
            StringBuffer sb = new StringBuffer();
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
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.python.PythonInterpreter#dispose()
     */
    @Override
    public void dispose() {
        proceduresDir.removeFileUpdatedObserver(this);
        utilitiesDir.removeFileUpdatedObserver(this);

        super.dispose();
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
        if (!isInstantiated(procedureName)) {
            instantiatePythonScript(procedureName);
        }
        args.put(PyConstants.METHOD_NAME, "execute");
        args.put(PyConstants.MODULE_NAME, procedureName);
        args.put(PyConstants.CLASS_NAME, pythonClassName);

        statusHandler.handle(Priority.DEBUG, "Running procedure: "
                + procedureName);

        internalExecute("runProcedure", INTERFACE, args);
        return getExecutionResult();
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

}
