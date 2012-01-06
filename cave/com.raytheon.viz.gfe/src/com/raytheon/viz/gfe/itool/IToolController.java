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
package com.raytheon.viz.gfe.itool;

import java.util.HashMap;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.viz.gfe.BaseGfePyController;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class IToolController extends BaseGfePyController {

    protected IToolController(String filePath, String anIncludePath,
            ClassLoader classLoader, DataManager dataManager)
            throws JepException {
        super(filePath, anIncludePath, classLoader, dataManager, "ITool");
        jep.eval(INTERFACE + " = IToolInterface('"
                + GfePyIncludeUtil.getIToolIncludePath() + "')");
    }

    public Object execute(String toolName) throws JepException {
        HashMap<String, Object> args = new HashMap<String, Object>();
        args.put(PyConstants.METHOD_NAME, "execute");
        args.put(PyConstants.MODULE_NAME, toolName);
        args.put(PyConstants.CLASS_NAME, pythonClassName);
        args.put("varDict", null); // TODO is this safe?

        internalExecute("runITool", INTERFACE, args);

        return getExecutionResult();
    }

    @Override
    protected void evaluateArgument(String argName, Object argValue)
            throws JepException {
        if (argName.startsWith("varDict")) {
            jep.eval(argName + " = varDict");
        } else {
            super.evaluateArgument(argName, argValue);
        }
    }

}
