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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.dbsrv.IQueryTransport;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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
 * Aug 23, 2013 DR 16514   D. Friedman Fix accum/cancel logic.
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
     * List queries to perform.
     */
    private final List<Request> requests;

    /**
     * Set of queries main thread is waiting for.
     */
    private final Set<Request> expected;

    /**
     * Transport to use for the queries.
     */
    private final IQueryTransport queryTransport;

    public ProductQueryJob(IProductQueryCallback callback) {
        super("Product Query");
        setSystem(true);
        this.callback = callback;
        requests = new ArrayList<Request>();
        expected = new HashSet<Request>();
        queryTransport = TextEditorUtil.getTextDbsrvTransport();
    }

    /**
     * Add request to queue.  If not an incremental update, cancel
     * existing requests.
     * 
     * @param command
     * @param isObsUpdated
     * @param accumulate
     */
    public synchronized void addRequest(ICommand command, boolean isObsUpdated,
            boolean accumulate) {
        Request request = new Request(command, isObsUpdated);
        if (! accumulate && ! isObsUpdated) {
            // Cancel existing requests.
            expected.clear();
            requests.clear();
        }
        requests.add(request);
        expected.add(request);
        schedule();
    }

    public boolean isExpectingRequests() {
        return ! expected.isEmpty();
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

        while (true) {
            final Request request;
            synchronized (this) {
                if (requests.size() > 0) {
                    request = requests.remove(0);
                } else {
                    break;
                }
            }

            List<StdTextProduct> prodList = null;
            try {
                try {
                    prodList = request.getCommand().
                            executeCommand(queryTransport);
                } catch (CommandFailedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            } finally {
                final List<StdTextProduct> resultProdList = prodList;
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        if (expected.remove(request) && resultProdList != null) {
                            callback.requestDone(request.getCommand(), resultProdList,
                                    request.isObsUpdated());
                        } else {
                            callback.requestDone(null, null, false);
                        }
                    }
                });
            }
        }
        return Status.OK_STATUS;
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
