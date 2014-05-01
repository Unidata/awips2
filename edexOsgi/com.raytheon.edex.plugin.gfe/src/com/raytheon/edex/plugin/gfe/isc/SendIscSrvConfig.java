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

import java.util.concurrent.Executor;

/**
 * Send ISC service configuration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            dgilling    Initial creation
 * Apr 30, 2013 1949       rjpeter     Removed initial delay.
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SendIscSrvConfig {

    protected int threads;

    protected Executor executor;

    protected int runningTimeOutMillis;

    protected int threadSleepInterval;

    public int getThreads() {
        return threads;
    }

    public void setThreads(int threads) {
        this.threads = threads;
    }

    public Executor getExecutor() {
        return executor;
    }

    public void setExecutor(Executor executor) {
        this.executor = executor;
    }

    public int getRunningTimeOutMillis() {
        return runningTimeOutMillis;
    }

    public void setRunningTimeOutMillis(int runningTimeOutMillis) {
        this.runningTimeOutMillis = runningTimeOutMillis;
    }

    public int getThreadSleepInterval() {
        return threadSleepInterval;
    }

    public void setThreadSleepInterval(int threadSleepInterval) {
        this.threadSleepInterval = threadSleepInterval;
    }
}
