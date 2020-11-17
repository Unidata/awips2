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
package com.raytheon.viz.gfe.smarttool.script;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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
import com.raytheon.viz.gfe.core.msgs.ISmartToolInventoryChanged;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.smartscript.FieldDefinition;

import jep.JepException;

/**
 * Manages the smart tool inventory and the associated metadata for each tool
 * for the current GFE session.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 20, 2015  4263     dgilling  Initial creation
 * Dec 14, 2015  4816     dgilling  Support refactored PythonJobCoordinator API.
 * Feb 13, 2018  6906     randerso  Refactored for on demand update of metadata.
 *
 * </pre>
 *
 * @author dgilling
 */

public class SmartToolMetadataManager {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final Semaphore semaphore;

    private Map<String, SmartToolMetadata> metaData;

    private MetadataControllerJob job;

    /**
     * Constructor
     *
     * @param dataMgr
     */
    public SmartToolMetadataManager(final DataManager dataMgr) {
        this.metaData = new HashMap<>();
        this.semaphore = new Semaphore(0);

        this.job = new MetadataControllerJob(dataMgr);
        this.job.schedule();
    }

    /**
     * Initialize smart tool metadata
     *
     * @param startupListener
     *            listener to notify when initialization is complete
     */
    public void initialize(final IAsyncStartupObjectListener startupListener) {
        IPythonJobListener<Object> listener = new IPythonJobListener<Object>() {

            @Override
            public void jobFinished(Object result) {
                @SuppressWarnings("unchecked")
                Map<String, SmartToolMetadata> castResult = (Map<String, SmartToolMetadata>) result;
                updateMetadata(castResult);
                startupListener.objectInitialized();
                semaphore.release();
            }

            @Override
            public void jobFailed(Throwable e) {
                statusHandler.error("Error initializing smart tool inventory.",
                        e);
                startupListener.objectInitialized();
                semaphore.release();
            }
        };
        try {
            job.getMetadata(listener);
        } catch (Exception e1) {
            statusHandler.error("Error initializing smart tool metadata.", e1);
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

    private void updateMetadata(Map<String, SmartToolMetadata> newMetadata) {
        this.metaData = newMetadata;
    }

    /**
     * All access to the metadata should be done through this method to ensure
     * it is up-to-date
     *
     * @return the smart tool metadata
     */
    private Map<String, SmartToolMetadata> getMetadata() {
        if (job.needsUpdated()) {
            semaphore.acquireUninterruptibly();

            IPythonJobListener<Object> listener = new IPythonJobListener<Object>() {

                @Override
                public void jobFinished(Object result) {
                    @SuppressWarnings("unchecked")
                    Map<String, SmartToolMetadata> castResult = (Map<String, SmartToolMetadata>) result;
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
        Map<String, SmartToolMetadata> retVal = this.metaData;
        semaphore.release();
        return retVal;
    }

    /**
     * Lists all the tools that apply to the specified parm
     *
     * @param parm
     *            the parm to find applicable tools for
     * @return the names of tools that apply to the parm
     */
    public String[] listTools(Parm parm) {
        String parmName = null;
        String parmTypeName = null;
        if (parm != null) {
            parmName = parm.getParmID().getParmName();
            parmTypeName = parm.getGridInfo().getGridType().name();
        }

        Collection<String> tools = new HashSet<>();
        boolean listAll = ((parmName == null) && (parmTypeName == null));
        for (SmartToolMetadata toolData : getMetadata().values()) {
            String toolName = toolData.getName();

            if (listAll) {
                tools.add(toolName);
            } else if (toolData.isHideTool()) {
                continue;
            } else if (parmName.equals(toolData.getWeatherElementEdited())) {
                tools.add(toolName);
            } else if (toolData.getScreenList() != null) {
                if ((toolData.getScreenList().contains(parmName))
                        || (toolData.getScreenList().contains(parmTypeName))) {
                    tools.add(toolName);
                }
            } else if ("variableElement"
                    .equals(toolData.getWeatherElementEdited())) {
                tools.add(toolName);
            }
        }

        String[] retVal = tools.toArray(new String[0]);
        Arrays.sort(retVal);
        return retVal;
    }

    /**
     * Gets the WeatherElementEdited variable from the smart tool
     *
     * @param toolName
     *            the name of the tool
     * @return the WeatherElementEdited
     */
    public String getWeatherElementEdited(String toolName) {
        SmartToolMetadata toolData = getMetadata().get(toolName);

        if (toolData != null) {
            return toolData.getWeatherElementEdited();
        }
        return null;
    }

    /**
     * @param toolName
     * @return the varDict widgets for the specified tool
     */
    public List<FieldDefinition> getVarDictWidgets(String toolName) {
        SmartToolMetadata toolData = getMetadata().get(toolName);

        if (toolData != null) {
            return toolData.getVarDictWidgets();
        }
        return Collections.emptyList();
    }

    /**
     * Gets the tool's execute() __doc__ string, explaining what the tool does
     *
     * @param toolName
     *            the tool to find info on
     * @return the tool's execute() documentation, or null if there isn't any
     */
    public String getInfo(String toolName) {
        SmartToolMetadata toolData = getMetadata().get(toolName);

        if (toolData != null) {
            return toolData.getDocString();
        }
        return null;
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

        private SmartToolController controller;

        private final DataManager dataMgr;

        private MetadataControllerJob(DataManager dataMgr) {
            super("GFE-SmartToolMetadataControllerJob");
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
                controller = new SmartToolScriptFactory(dataMgr)
                        .createPythonScript();
            } catch (JepException e) {
                statusHandler.error("Error initializing smart tool controller",
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
                                "Unhandled exception in SmartToolInterface ",
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

    /**
     * @param listener
     */
    public void addListener(ISmartToolInventoryChanged listener) {
        this.job.controller.addListener(listener);
    }

    /**
     * @param listener
     */
    public void removeListener(ISmartToolInventoryChanged listener) {
        this.job.controller.removeListener(listener);
    }
}
