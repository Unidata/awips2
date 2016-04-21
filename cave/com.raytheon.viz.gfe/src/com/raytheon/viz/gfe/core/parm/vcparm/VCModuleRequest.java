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
package com.raytheon.viz.gfe.core.parm.vcparm;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Semaphore;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.viz.core.jobs.QueueJobRequest;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class VCModuleRequest extends QueueJobRequest<Object> {

    private String moduleName;

    private String methodName;

    private Map<String, Object> argMap;

    private GridType type;

    private Object result;

    private Semaphore completionMutex;

    public VCModuleRequest(String moduleName, String method,
            Map<String, Object> argMap) {
        this(moduleName, method, argMap, null);
    }

    public VCModuleRequest(String moduleName, String method,
            Map<String, Object> argMap, GridType type) {
        this.moduleName = moduleName;
        this.methodName = method;
        this.argMap = argMap;
        this.type = type;
        this.completionMutex = new Semaphore(1);
        // always leave the mutex locked until we're ready to return a result
        try {
            this.completionMutex.acquire();
        } catch (InterruptedException e) {
            // don't care
        }
    }

    public String getModuleName() {
        return moduleName;
    }

    public String getMethodName() {
        return methodName;
    }

    public Map<String, Object> getArgMap() {
        return argMap;
    }

    public Map<String, Object> getJepArgs() {
        if (argMap != null) {
            return new HashMap<String, Object>(argMap);
        } else {
            return new HashMap<String, Object>();
        }
    }

    public GridType getType() {
        return type;
    }

    /**
     * Returns the result from executing this request. Because
     * <code>VCModuleRequest</code>s are executed asynchronously, this method
     * blocks until the <code>VCModule</code> method has completed execution.
     * 
     * @return The result of executing the request.
     * @throws Throwable
     *             If an exception occurred while processing the request.
     */
    public Object getResult() throws Throwable {
        try {
            // because we initially locked the mutex in the constructor, this
            // will block until something has called setResult
            completionMutex.acquire();
            if (!(result instanceof Throwable)) {
                return result;
            } else {
                throw (Throwable) result;
            }
        } catch (InterruptedException e) {
            throw e;
        } finally {
            completionMutex.release();
        }
    }

    public void setResult(Object result) {
        this.result = result;
        completionMutex.release();
    }
}
