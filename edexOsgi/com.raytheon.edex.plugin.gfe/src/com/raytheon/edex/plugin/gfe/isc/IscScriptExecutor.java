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

import com.raytheon.uf.common.python.concurrent.IPythonExecutor;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * {@link IPythonExecutor} class for {@link IscScript} that runs an execute
 * method in the script using the ExecutorService.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class IscScriptExecutor implements IPythonExecutor<IscScript, String> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(IscScriptExecutor.class);

    private String methodName;

    private String siteId;

    private Map<String, Object> args;

    public IscScriptExecutor(String methodName, String siteId,
            Map<String, Object> args) {
        this.methodName = methodName;
        this.siteId = siteId;
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
    public String execute(IscScript script) {
        String retVal = null;
        try {
            script.execute(methodName, args, siteId);
        } catch (JepException e) {
            String msg = "Python error executing GFE ISC script ["
                    + script.getScriptName() + "]: " + e.getLocalizedMessage();
            statusHandler.handle(Priority.ERROR, msg, e);
            retVal = msg;
        } catch (Throwable t) {
            String msg = "Unexpected Java Exception executing GFE ISC script ["
                    + script.getScriptName() + "]: " + t.getLocalizedMessage();
            statusHandler.handle(Priority.ERROR, msg, t);
            retVal = msg;
        }
        return retVal;
    }

}
