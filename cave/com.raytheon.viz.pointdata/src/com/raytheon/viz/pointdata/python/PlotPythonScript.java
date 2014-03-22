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
package com.raytheon.viz.pointdata.python;

import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.python.PythonScript;

/**
 * A python interpreter that is expecting to receive and retain a multi-line
 * script of plot delegate python extracted from a plot model SVG file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2014 2868       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PlotPythonScript extends PythonScript {

    protected String plotDelegateName;

    /**
     * Constructor
     * 
     * @param filePath
     *            the path to the plot model interface python
     * @param anIncludePath
     *            the include path for relevant python modules
     * @param scriptText
     *            the text of the python code extracted from the plot model svg
     * @param plotDelegateName
     *            the instance name of the plot delegate
     * @param svgFilename
     *            the name of the SVG file the python code came from, this is
     *            used in exception stacktraces
     * 
     * @throws JepException
     */
    public PlotPythonScript(String filePath, String anIncludePath,
            String scriptText, String plotDelegateName, String svgFilename)
            throws JepException {
        super(filePath, anIncludePath, PlotPythonScript.class.getClassLoader());
        /*
         * jep.eval() won't evaluate more than a single line of python code,
         * unless that first line is a python class or method definition, so we
         * work around that by compiling it, then eval-ing it
         * 
         * TODO contemplate building the workaround into PythonInterpreter,
         * PythonScript, or PythonEval
         */
        jep.set("scriptToRun", scriptText);
        jep.eval("eval(compile(scriptToRun, '" + svgFilename + "', 'exec'))");
        this.plotDelegateName = plotDelegateName;
    }

    @Override
    public Object execute(String methodName, Map<String, Object> args)
            throws JepException {
        return execute(methodName, plotDelegateName, args);
    }

}
