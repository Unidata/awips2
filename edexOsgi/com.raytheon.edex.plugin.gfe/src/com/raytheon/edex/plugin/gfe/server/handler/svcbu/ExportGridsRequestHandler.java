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
package com.raytheon.edex.plugin.gfe.server.handler.svcbu;

import java.util.Date;
import java.util.Random;
import java.util.Timer;

import com.raytheon.edex.plugin.gfe.svcbackup.ExportGridsTask;
import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Request handler for {@code ExportGridsRequest}. This handler will export the
 * GFE grids for a site and leaves them in the GFESuite/exportgrids folder for
 * the central server to download via rsync.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2011            bphillip     Initial creation
 * Apr 30, 2013  #1761     dgilling     Support changes made to 
 *                                      ExportGridsRequest.
 * Mar 17, 2015  #4103     dgilling     Stop using ServiceBackupNotificationManager,
 *                                      supoprt new Service Backup GUI.
 * Oct 24, 2016  #5951     dgilling     Clean up error log message.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public final class ExportGridsRequestHandler
        implements IRequestHandler<ExportGridsRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExportGridsRequestHandler.class);

    private static Timer exportGridsTimer = new Timer(true);

    @Override
    public JobProgress handleRequest(final ExportGridsRequest request)
            throws Exception {
        JobProgress progress = JobProgress.SUCCESS;

        try {
            SvcBackupUtil.execute("export_grids",
                    request.getMode().getCmdLineArg(),
                    request.getSite().toLowerCase());
            statusHandler.info("Digital data successfully exported.");
        } catch (Exception e) {
            statusHandler.error("Error exporting digital data for site "
                    + request.getSite(), e);
        }

        return progress;
    }

    public void exportGridsCron() throws Exception {
        Random rand = new Random(System.currentTimeMillis());
        Date executionDate = new Date(System.currentTimeMillis()
                + Math.abs(rand.nextInt() % 45) * 60 * 1000);
        exportGridsTimer.schedule(new ExportGridsTask(executionDate),
                executionDate);
    }
}
