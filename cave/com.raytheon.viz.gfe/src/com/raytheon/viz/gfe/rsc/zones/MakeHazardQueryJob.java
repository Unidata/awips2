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
package com.raytheon.viz.gfe.rsc.zones;

import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * A job class to query the database (so the UI thread doesn't block waiting on
 * the db query)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 4, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class MakeHazardQueryJob extends Job {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MakeHazardQueryJob.class);

    private static final int QUEUE_SIZE = 1;

    /** The name of the DB from which maps should be read */
    private static final String MAP_DB = "maps";

    /**
     * A queue of database query requests.
     */
    protected ArrayBlockingQueue<QueryRequest> requestQueue = new ArrayBlockingQueue<QueryRequest>(
            QUEUE_SIZE);

    /**
     * A queue of responses from the database.
     */
    protected ArrayBlockingQueue<MakeHazardDbData[]> responseQueue = new ArrayBlockingQueue<MakeHazardDbData[]>(
            QUEUE_SIZE);

    /**
     * @param name
     */
    protected MakeHazardQueryJob(String name) {
        super(name);
    }

    @Override
    protected IStatus run(IProgressMonitor monitor) {
        if (monitor.isCanceled()) {
            return Status.CANCEL_STATUS;
        }
        // long runStart = System.currentTimeMillis();

        QueryRequest request = requestQueue.poll();
        if (request != null) {
            List<Object[]> qResponse = null;
            try {
                qResponse = DirectDbQuery.executeQuery(request.query, MAP_DB,
                        QueryLanguage.SQL);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying database", e);
            }
            if (qResponse != null) {
                int rsps = qResponse.size();
                MakeHazardDbData[] unsorted = new MakeHazardDbData[rsps];
                String tableName;
                String cwa;
                String zone;
                int col;
                int rowNum = 0;
                for (Object[] row : qResponse) {
                    col = 0;
                    tableName = (String) row[col++];
                    zone = (String) row[col++];
                    cwa = (String) row[col++];
                    unsorted[rowNum++] = new MakeHazardDbData(tableName, cwa,
                            zone);
                }
                long millis = 0L;
                boolean interrupted = false;
                while (!responseQueue.offer(unsorted) && !interrupted
                        && !monitor.isCanceled()) {
                    responseQueue.clear();
                    try {
                        Thread.sleep(millis);
                    } catch (InterruptedException e) {
                        interrupted = true;
                    }
                    millis = 100L;
                }
                // Trigger another paint() call now that DB data is present
                if (request.target != null) {
                    request.target.setNeedsRefresh(true);
                }
            }
        }
        // statusHandler.handle(Priority.VERBOSE, "query job completed in "
        // + (System.currentTimeMillis() - runStart) + " ms");
        return Status.OK_STATUS;
    }
}
