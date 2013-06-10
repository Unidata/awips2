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
 * Service that runs ISC send jobs. Along with IscSendQueue, this class roughly
 * replaces AWIPS1's SendISCMgr.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class SendIscSrv {

    private final SendIscSrvConfig cfg;

    private final Executor executor;

    public SendIscSrv(SendIscSrvConfig config) {
        this.cfg = config;
        this.executor = config.getExecutor();
        for (int i = 0; i < cfg.getThreads(); i++) {
            IscSendJob thread = new IscSendJob();
            thread.setRunningTimeOutMillis(cfg.getRunningTimeOutMillis());
            thread.setThreadSleepInterval(cfg.getThreadSleepInterval());
            executor.execute(thread);
        }
    }
}
