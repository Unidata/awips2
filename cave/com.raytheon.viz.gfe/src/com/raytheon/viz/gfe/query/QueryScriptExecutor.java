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

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.python.concurrent.IPythonExecutor;

import jep.JepException;

/**
 * {@link IPythonExecutor} class for {@link QueryScript} that runs an execute
 * method in the script using the ExecutorService
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 07, 2013           mnash     Initial creation
 * Apr 27, 2018  7256     randerso  Allow exceptions to propagate back to caller
 *
 * </pre>
 *
 * @author mnash
 */

public class QueryScriptExecutor
        implements IPythonExecutor<QueryScript, ReferenceData> {

    private String methodName;

    private Map<String, Object> args;

    /**
     * Constructor
     *
     * @param methodName
     * @param args
     */
    public QueryScriptExecutor(String methodName, Map<String, Object> args) {
        this.methodName = methodName;
        this.args = args;
    }

    @Override
    public ReferenceData execute(QueryScript script) throws JepException {
        ReferenceData retVal = null;
        retVal = (ReferenceData) script.execute(methodName, args);
        return retVal;
    }
}
