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
package com.raytheon.edex.plugin.gfe.smartinit;

import java.util.Map;

import com.raytheon.uf.common.python.PythonEval;

import jep.JepConfig;
import jep.JepException;

/**
 * A wrapper for running smart init python
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 28, 2008           njensen   Initial creation
 * Oct 23, 2012  1291     randerso  Changed to extend PythonEval instead of
 *                                  PythonScript so it doesn't get run causing
 *                                  it to load as module __main__ instead of
 *                                  Init.
 * Nov 01, 2016  5979     njensen   Cast to Number where applicable
 * Dec 19, 2017  7149     njensen   Use JepConfig and shared modules in
 *                                  constructor
 * Dec 02, 2019  7986     randerso  Remove redundant setting of shared modules
 *
 * </pre>
 *
 * @author njensen
 */

public class SmartInitScript extends PythonEval {

    private static final String METHOD_NAME = "runFromJava";

    /**
     * Constructor
     *
     * @param aFilePath
     *            the path of the script to run
     * @param anIncludePath
     *            the python include path
     * @param aClassLoader
     *            the java classloader
     * @throws JepException
     */
    public SmartInitScript(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader) throws JepException {
        super(new JepConfig().setIncludePath(anIncludePath)
                .setClassLoader(aClassLoader));
        jep.eval("from Init import *");
    }

    /**
     * Runs the smart init with the specified arguments
     *
     * @param args
     *            the arguments to pass the script
     * @return
     * @throws JepException
     */
    public Object execute(Map<String, Object> args) throws JepException {
        return this.execute(METHOD_NAME, args);
    }

    /**
     * Adds the specified site path to the python interpreter's include path
     * directly before the specified base path.
     *
     * @param sitePath
     *            Path to add to include path.
     * @param basePath
     *            Path to add new path directly before.
     * @throws JepException
     */
    public void addSitePath(String sitePath, String basePath)
            throws JepException {
        boolean inList = (Boolean) jep
                .getValue("'" + basePath + "' in sys.path");
        int index;
        if (inList) {
            index = ((Number) jep
                    .getValue("sys.path.index('" + basePath + "')")).intValue();
        } else {
            index = ((Number) jep.getValue("len(sys.path)")).intValue();
        }
        jep.eval("sys.path.insert(" + index + ", '" + sitePath + "')");
    }

    public void removeSitePath(String path) throws JepException {
        jep.eval("sys.path.remove('" + path + "')");
    }

}
