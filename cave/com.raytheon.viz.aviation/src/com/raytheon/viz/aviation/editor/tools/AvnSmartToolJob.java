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
package com.raytheon.viz.aviation.editor.tools;

import java.util.concurrent.TimeUnit;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.jobs.AbstractQueueJob;
import com.raytheon.viz.aviation.activator.Activator;
import com.raytheon.viz.aviation.activator.StatusConstants;

/**
 * Job for running smart tools off the UI thread
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2010            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class AvnSmartToolJob extends AbstractQueueJob<AvnSmartToolRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(AvnSmartToolJob.class);

    private static AvnSmartToolJob instance = null;

    private boolean shutdown = false;

    private AvnSmartToolJob() {
        super("AvnFPS Smart Tool Job");
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        ToolPython toolPython = new ToolPython();
        Thread.currentThread().setName("AvnSmartToolJob");
        shutdown = false;
        AvnSmartToolRequest request = null;
        try {
            while (!shutdown) {
                request = queue.poll(1000L, TimeUnit.MILLISECONDS);
                if (!shutdown && request != null) {
                    String result = null;
                    // Determine which ToolPython method to call.
                    if (request.getToolName().equals("getTafTools") == false) {
                        result = toolPython.runTool(request.getToolName(),
                                request.getBbb(), request.getFcsts());
                        // System.out.println(result);
                    } else {
                        String[] r = toolPython.getTafTools();
                        // System.out.println(r);
                        StringBuilder res = new StringBuilder();
                        String prefix = "";
                        for (String s : r) {
                            res.append(prefix);
                            res.append(s);
                            prefix = ":";
                        }
                        result = res.toString();
                    }

                    if (request.getListener() != null) {
                        request.getListener().requestComplete(result);
                    }
                }
            }
        } catch (InterruptedException e) {
            // Do nothing
        } finally {
            queue.clear();
            toolPython.dispose();
        }

        return Status.OK_STATUS;
    }

    /**
     * There can be only one.
     * 
     * @return instance
     */
    public static synchronized final AvnSmartToolJob getInstance() {
        if (instance == null) {
            instance = new AvnSmartToolJob();
            instance.setSystem(true);
            instance.schedule();
        }
        return instance;
    }

    /**
     * This allows for graceful closing of the instance and disposing of
     * resources.
     */
    // This potentially could be made a method in AbstractQueueJob that would
    // set a flag, clear the queue and even notify listeners about
    // the shut down.
    public static synchronized final void shutdown() {
        if (instance != null) {
            instance.shutdown = true;
            instance.queue.clear();
            instance = null;
        }
    }

    /**
     * This is a wrapper to allow the ToolPython's getTafTools to run in the
     * same thread as its runTool.
     * 
     * @return runTools
     */
    public static final String[] getTafTools() {
        AvnSmartToolRequest request = new AvnSmartToolRequest();
        request.setToolName("getTafTools");
        AvnSmartToolFinishedListener finish = new AvnSmartToolFinishedListener();
        request.setListener(finish);
        AvnSmartToolJob job = AvnSmartToolJob.getInstance();
        job.enqueue(request);

        // Should not take long so just wait on current thread.
        try {
            while (!job.shutdown) {
                if (finish.isDone() == false) {
                    Thread.sleep(10L);
                } else {
                    String r = finish.getResult();
                    return r.split(":");
                }
            }
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Avn Smart tool thread interrupted", e);
        }
        return null;
    }
}
