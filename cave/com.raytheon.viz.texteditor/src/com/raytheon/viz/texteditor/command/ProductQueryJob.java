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
package com.raytheon.viz.texteditor.command;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.services.textdbsrv.IQueryTransport;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.texteditor.util.TextEditorUtil;

/**
 * Job to perform queries for text products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2013            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class ProductQueryJob extends Job {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProductQueryJob.class);

    private final IProductQueryCallback callback;

    /**
     * flag to indicate request accumulate.
     */
    private boolean accumulate;

    /**
     * List queries to perform.
     */
    private final List<Request> requests;

    /**
     * Transport to use for the queries.
     */
    private final IQueryTransport queryTransport;

    /**
     * Flag to indicate cancel is being performed.
     */
    private final AtomicBoolean canceled;

    public ProductQueryJob(IProductQueryCallback callback) {
        super("Product Query");
        setSystem(true);
        this.callback = callback;
        accumulate = false;
        requests = new ArrayList<Request>();
        queryTransport = TextEditorUtil.getTextDbsrvTransport();
        canceled = new AtomicBoolean(false);
    }

    /**
     * Add request to queue and determine what needs to be done to schedule the
     * request.
     * 
     * @param command
     * @param isObsUpdated
     */
    public synchronized void addRequest(ICommand command, boolean isObsUpdated) {
        Request request = new Request(command, isObsUpdated);
        if (accumulate) {
            requests.add(request);
            if (getState() == Job.NONE) {
                schedule();
            }
        } else {
            requests.clear();
            requests.add(request);
            if (getState() == Job.NONE) {
                schedule();
            } else {
                cancel();
            }
        }
    }

    public boolean isAccumulate() {
        return accumulate;
    }

    /**
     * When set to true requests will accumulate and be processed in the order
     * received; otherwise the queue is purged and any current request is
     * canceled if a new request is received.
     * 
     * @param accumulate
     */
    public void setAccumulate(boolean accumulate) {
        if (this.accumulate != accumulate) {
            synchronized (this) {
                requests.clear();
                if (getState() != Job.NONE) {
                    cancel();
                }
                this.accumulate = accumulate;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        if (monitor.isCanceled()) {
            return Status.OK_STATUS;
        }

        Request request = null;
        while (!canceled.get()) {
            synchronized (this) {
                if (requests.size() > 0) {
                    request = requests.remove(0);
                } else {
                    break;
                }
            }

            try {
                final ICommand command = request.getCommand();
                final boolean isObsUpdated = request.isObsUpdated();
                final List<StdTextProduct> prodList = command
                        .executeCommand(queryTransport);
                // User may have canceled while long query is be performed.
                if (!canceled.get()) {
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            callback.requestDone(command, prodList,
                                    isObsUpdated);
                        }
                    });
                } else {
                    canceled.set(false);
                }
            } catch (CommandFailedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                callback.requestDone(null, null, false);
            }
        }
        canceled.set(false);
        return Status.OK_STATUS;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#canceling()
     */
    @Override
    protected void canceling() {
        canceled.set(true);
    }

    /*
     * Class to hold the query command and isObsUpdated flag needed for the
     * query and its callback.
     */
    private static class Request {
        private ICommand command;

        private boolean isObsUpdated;

        public Request(ICommand command, boolean isObsUpdated) {
            this.command = command;
            this.isObsUpdated = isObsUpdated;
        }

        public ICommand getCommand() {
            return command;
        }

        public boolean isObsUpdated() {
            return isObsUpdated;
        }
    }
}
