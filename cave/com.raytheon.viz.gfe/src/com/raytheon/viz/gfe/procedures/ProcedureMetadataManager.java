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
package com.raytheon.viz.gfe.procedures;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.python.concurrent.IPythonJobListener;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IAsyncStartupObjectListener;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;

import jep.JepException;

/**
 * Manages the procedure inventory and the associated metadata for each
 * procedure for the current GFE session.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 24, 2015  4263     dgilling  Initial creation
 * Dec 14, 2015  4816     dgilling  Support refactored PythonJobCoordinator API.
 * Feb 13, 2018  6906     randerso  Refactored for on demand update of metadata.
 * Dec 12, 2018  6906     randerso  Changed getMetaData to package scope
 *
 * </pre>
 *
 * @author dgilling
 */

public final class ProcedureMetadataManager {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final Semaphore semaphore;

    private Map<String, ProcedureMetadata> metadata;

    private MetadataControllerJob job;

    /**
     * Constructor
     *
     * @param dataMgr
     */
    public ProcedureMetadataManager(final DataManager dataMgr) {
        this.metadata = new HashMap<>();
        this.semaphore = new Semaphore(0);

        this.job = new MetadataControllerJob(dataMgr);
        this.job.schedule();
    }

    /**
     * Initialize procedure metadata
     *
     * @param startupListener
     *            listener to notify when initialization is complete
     */
    public void initialize(final IAsyncStartupObjectListener startupListener) {
        IPythonJobListener<Object> listener = new IPythonJobListener<Object>() {

            @Override
            public void jobFinished(Object result) {
                @SuppressWarnings("unchecked")
                Map<String, ProcedureMetadata> castResult = (Map<String, ProcedureMetadata>) result;
                updateMetadata(castResult);
                startupListener.objectInitialized();
                semaphore.release();
            }

            @Override
            public void jobFailed(Throwable e) {
                statusHandler.error("Error initializing procedure inventory.",
                        e);
                startupListener.objectInitialized();
                semaphore.release();
            }
        };

        try {
            job.getMetadata(listener);

        } catch (Exception e) {
            statusHandler.error("Error initializing procedure metadata.", e);
        }
    }

    /**
     * Called when object is no longer needed
     */
    public void dispose() {
        job.cancel();
        try {
            job.join();
        } catch (InterruptedException e) {
            // ignore since we're shutting down anyway
        }
    }

    private void updateMetadata(Map<String, ProcedureMetadata> newMetadata) {
        this.metadata = newMetadata;
    }

    /**
     * All access to the metadata should be done through this method to ensure
     * it is up-to-date
     *
     * @return the procedure metadata
     */
    Map<String, ProcedureMetadata> getMetadata() {
        if (job.needsUpdated()) {
            semaphore.acquireUninterruptibly();

            IPythonJobListener<Object> listener = new IPythonJobListener<Object>() {

                @Override
                public void jobFinished(Object result) {
                    @SuppressWarnings("unchecked")
                    Map<String, ProcedureMetadata> castResult = (Map<String, ProcedureMetadata>) result;
                    updateMetadata(castResult);
                    semaphore.release();
                }

                @Override
                public void jobFailed(Throwable e) {
                    statusHandler.error("Error updating procedure metadata.",
                            e);
                    semaphore.release();
                }
            };
            job.getMetadata(listener);
        }

        semaphore.acquireUninterruptibly();
        Map<String, ProcedureMetadata> retVal = this.metadata;
        semaphore.release();
        return retVal;
    }

    /**
     * Lists all the procedures that correspond to the menu name
     *
     * @param menuName
     *            the name of the menu
     * @return the names of procedures that should appear in the menu.
     */
    public List<String> getMenuItems(String menuName) {
        List<String> proceduresForMenu = new ArrayList<>();
        for (ProcedureMetadata procMetadata : getMetadata().values()) {
            if (procMetadata.getMenuNames().contains(menuName)) {
                proceduresForMenu.add(procMetadata.getName());
            }
        }
        Collections.sort(proceduresForMenu);
        return proceduresForMenu;
    }

    /**
     * @param procedureName
     * @return the varDict widgets for the specified procedure
     */
    public List<FieldDefinition> getVarDictWidgets(String procedureName) {
        ProcedureMetadata procMetadata = null;
        procMetadata = getMetadata().get(procedureName);

        if (procMetadata != null) {
            return procMetadata.getVarDictWidgets();
        }
        return Collections.emptyList();
    }

    /**
     * @param procedureName
     * @return the arguments for the specified procedure
     */
    public Collection<String> getMethodArguments(String procedureName) {
        ProcedureMetadata procMetadata = null;
        procMetadata = getMetadata().get(procedureName);

        if (procMetadata != null) {
            return procMetadata.getArgNames();
        }
        return Collections.emptyList();
    }

    private static class MetadataControllerJob extends Job {
        private static class MetadataRequest {
            private String methodName;

            private Map<String, Object> args;

            private IPythonJobListener<Object> listener;

            public MetadataRequest(String methodName, Map<String, Object> args,
                    IPythonJobListener<Object> listener) {
                this.methodName = methodName;
                this.args = args;
                this.listener = listener;
            }

            public String getMethodName() {
                return methodName;
            }

            public Map<String, Object> getArgs() {
                return args;
            }

            public IPythonJobListener<Object> getListener() {
                return listener;
            }

        }

        private static final BlockingQueue<MetadataRequest> workQueue = new LinkedBlockingQueue<>();

        private final IUFStatusHandler statusHandler = UFStatus
                .getHandler(getClass());

        private ProcedureController controller;

        private final DataManager dataMgr;

        private MetadataControllerJob(DataManager dataMgr) {
            super("GFE-ProcedureMataDataControllerJob");
            this.dataMgr = dataMgr;
            this.setSystem(true);
        }

        public boolean needsUpdated() {
            return controller.needsUpdated();
        }

        public void getMetadata(IPythonJobListener<Object> listener) {
            MetadataRequest request = new MetadataRequest("getScripts", null,
                    listener);
            workQueue.offer(request);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                controller = new ProcedureScriptFactory(dataMgr)
                        .createPythonScript();
            } catch (JepException e) {
                statusHandler.error("Error initializing procedure controller",
                        e);
                return Status.CANCEL_STATUS;
            }

            IStatus statusCode = Status.OK_STATUS;
            try {
                while (!monitor.isCanceled()) {
                    try {
                        MetadataRequest request = null;
                        try {
                            request = workQueue.poll(TimeUtil.MILLIS_PER_SECOND,
                                    TimeUnit.MILLISECONDS);
                        } catch (InterruptedException e) {
                            statusCode = Status.CANCEL_STATUS;
                            break;
                        }

                        if (monitor.isCanceled()) {
                            statusCode = Status.CANCEL_STATUS;
                            break;
                        }

                        if (request != null) {
                            controller.processFileUpdates();
                            if (monitor.isCanceled()) {
                                statusCode = Status.CANCEL_STATUS;
                                break;
                            }

                            String methodName = request.getMethodName();
                            Map<String, Object> args = request.getArgs();
                            Object result = controller.execute(methodName,
                                    "interface", args);

                            IPythonJobListener<Object> listener = request
                                    .getListener();
                            if (listener != null) {
                                listener.jobFinished(result);
                            }
                        }
                    } catch (Throwable t) {
                        statusHandler.error(
                                "Unhandled exception in ProcedureInterface ",
                                t);
                    }
                }
            } finally {
                if (controller != null) {
                    controller.dispose();
                    controller = null;
                }
            }

            return statusCode;
        }
    }
}
