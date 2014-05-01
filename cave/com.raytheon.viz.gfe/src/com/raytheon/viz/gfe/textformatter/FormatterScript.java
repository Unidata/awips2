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
package com.raytheon.viz.gfe.textformatter;

import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.python.PythonScript;

/**
 * A script that runs the text formatter python
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 2, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FormatterScript extends PythonScript {

    private static final String METHOD_NAME = "executeFromJava";

    /**
     * Constructor
     * 
     * @param aFilePath
     *            the script to run
     * @param anIncludePath
     *            the python include path
     * @param aClassLoader
     *            the java classloader
     * @throws JepException
     */
    protected FormatterScript(String aFilePath, String anIncludePath,
            ClassLoader aClassLoader) throws JepException {
        super(aFilePath, anIncludePath, aClassLoader);
    }

    @Override
    public Object execute(String methodName, Map<String, Object> args)
            throws JepException {
        internalExecute(methodName, null, args);
        Object result = jep.getValue(RESULT);

        return result;
    }

    public Object execute(Map<String, Object> args) throws JepException {
        return execute(METHOD_NAME, args);
    }

}
