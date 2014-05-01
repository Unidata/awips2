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
package com.raytheon.uf.viz.core.jobs;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.comm.NetworkStatistics;
import com.raytheon.uf.common.comm.NetworkStatistics.NetworkTraffic;

/**
 * Stats job for thin client. Logs network stats every minute to console
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 2, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class StatsJob extends Job {

    private NetworkStatistics stats;

    private long lastSent = 0, lastReceived = 0, lastRequestCount = 0;

    private boolean run = false;

    private Thread runningThread;

    /**
     * @param name
     */
    public StatsJob(String name, NetworkStatistics stats) {
        super(name);
        setSystem(true);
        this.stats = stats;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        runningThread = Thread.currentThread();
        run = true;
        while (run) {
            NetworkTraffic total = stats.getTotalTrafficStats();
            long sentInLastMinute = total.getBytesSent() - lastSent;
            long receivedInLastMinute = total.getBytesReceived() - lastReceived;
            long requestCountInLastMinute = total.getRequestCount()
                    - lastRequestCount;

            boolean printed = false;
            if (sentInLastMinute != 0.0 || receivedInLastMinute != 0.0
                    || requestCountInLastMinute != 0.0) {
                printed = true;
                System.out.println();
                System.out.println("Last minute sent "
                        + requestCountInLastMinute
                        + " messages for a total of "
                        + NetworkStatistics.toString(sentInLastMinute)
                        + " sent and "
                        + NetworkStatistics.toString(receivedInLastMinute)
                        + " received");
            }
            lastSent = total.getBytesSent();
            lastReceived = total.getBytesReceived();
            lastRequestCount = total.getRequestCount();
            if (printed) {
                System.out.println("Total sent " + total.getRequestCount()
                        + " messages for a total of "
                        + NetworkStatistics.toString(lastSent) + " sent and "
                        + NetworkStatistics.toString(lastReceived)
                        + " received");
                NetworkTraffic[] mapped = stats.getMappedTrafficStats();
                for (NetworkTraffic nt : mapped) {
                    System.out.println(nt);
                }

                System.out.println();
            }

            try {
                Thread.sleep(60 * 1000);
            } catch (InterruptedException e) {
            }
        }
        return Status.OK_STATUS;
    }

    public void shutdown() {
        if (run) {
            run = false;
            runningThread.interrupt();
        }
    }
}
