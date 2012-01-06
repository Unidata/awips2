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
package com.raytheon.edex.plugin.gfe.isc;

import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.python.PythonScript;

/**
 * A wrapper for running the python-based ISC send script.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IscSendScript extends PythonScript {

    private static final String METHOD_NAME = "executeIscExtract";

    /**
     * @param aFilePath
     * @param anIncludePath
     * @throws JepException
     */
    public IscSendScript(String aFilePath, String anIncludePath)
            throws JepException {
        super(aFilePath, anIncludePath);
    }

    public Object execute(Map<String, Object> args) throws JepException {
        return this.execute(METHOD_NAME, args);
    }
}
