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

import java.util.concurrent.TimeUnit;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.dataplugin.gfe.StatusConstants;
import com.raytheon.uf.common.python.PyConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.jobs.AbstractQueueJob;
import com.raytheon.viz.gfe.core.DataManager;

/**
 * <code>Job</code> which allows <code>VCModule</code> python calls to run off a
 * common thread.
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

public class VCModuleJob extends AbstractQueueJob<VCModuleRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VCModuleJob.class);

    private VCModuleController python = null;

    private DataManager dataMgr;

    public VCModuleJob(DataManager dataMgr) {
        super("GFE Virtual ISC Python executor");
        setSystem(true);
        this.dataMgr = dataMgr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        try {
            try {
                python = VCModuleControllerFactory.buildInstance(dataMgr);
            } catch (JepException e) {
                return new Status(IStatus.ERROR, StatusConstants.PLUGIN_ID,
                        "Error initializing VCModule python object", e);
            }

            while (!monitor.isCanceled()) {
                VCModuleRequest request = null;
                try {
                    request = queue.poll(1000L, TimeUnit.MILLISECONDS);
                    if (request != null) {
                        processRequest(request);
                    }

                    // TODO: Reinstate this call, if we ever want to support
                    // dynamic editing of VCMODULE files through the
                    // Localization perspective.
                    // python.processFileUpdates();
                } catch (InterruptedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "VC Module python thread interrupted.", e);
                    break;
                } catch (Throwable t) {
                    // statusHandler.handle(Priority.DEBUG,
                    // "Error running VCModule method.", t);
                    request.setResult(t);
                }
            }
        } finally {

            if (python != null) {
                python.dispose();
                python = null;
            }
        }

        return Status.OK_STATUS;
    }

    private void processRequest(VCModuleRequest request) throws JepException {
        Object result = null;

        if (request.getMethodName().equals("getMethodArgs")) {
            result = python.getMethodArguments(request.getModuleName(),
                    (String) request.getArgMap().get(PyConstants.METHOD_NAME));
        } else {
            result = python.executeMethod(request.getModuleName(),
                    request.getMethodName(), request.getJepArgs(),
                    request.getType());
        }

        request.setResult(result);
    }
}
