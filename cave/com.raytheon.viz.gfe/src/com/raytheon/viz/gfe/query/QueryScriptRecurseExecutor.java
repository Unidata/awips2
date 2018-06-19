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

import com.raytheon.uf.common.python.concurrent.IPythonExecutor;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

import jep.JepException;

/**
 * Calls the willRecurse method for QueryScript and returns the correct type
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2013            mnash       Initial creation
 * Nov 02, 2016    5979    njensen     Cast to Number where applicable
 * Apr 14, 2017    5979    njensen     Safer null check after cast
 * 
 * </pre>
 * 
 * @author mnash
 */

public class QueryScriptRecurseExecutor implements
        IPythonExecutor<QueryScript, Integer> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(QueryScriptRecurseExecutor.class);

    private Map<String, Object> args;

    public QueryScriptRecurseExecutor(Map<String, Object> args) {
        this.args = args;
    }

    @Override
    public Integer execute(QueryScript script) {
        Integer retVal = null;
        try {
            Number n = (Number) script.execute("willRecurse", args);
            if (n != null) {
                retVal = n.intValue();
            }
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM, "Unable to execute script",
                    e);
        }
        return retVal;
    }
}
