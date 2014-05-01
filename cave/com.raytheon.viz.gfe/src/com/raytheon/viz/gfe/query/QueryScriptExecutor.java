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
package com.raytheon.viz.gfe.query;

import java.util.Map;

import jep.JepException;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.python.concurrent.IPythonExecutor;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * {@link IPythonExecutor} class for {@link QueryScript} that runs an execute
 * method in the script using the ExecutorService
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 7, 2013            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class QueryScriptExecutor implements
        IPythonExecutor<QueryScript, ReferenceData> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(QueryScriptExecutor.class);

    private String methodName;

    private Map<String, Object> args;

    /**
     * 
     */
    public QueryScriptExecutor(String methodName, Map<String, Object> args) {
        this.methodName = methodName;
        this.args = args;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.python.concurrent.IPythonExecutor#execute(com.
     * raytheon.uf.common.python.PythonInterpreter)
     */
    @Override
    public ReferenceData execute(QueryScript script) {
        ReferenceData retVal = null;
        try {
            retVal = (ReferenceData) script.execute(methodName, args);
        } catch (JepException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error evaluating edit area query: " + e.getMessage(), e);
        }
        return retVal;
    }
}
