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
import com.raytheon.edex.plugin.gfe.svcbackup.ServiceBackupNotificationManager;
import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportGridsRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ExportGridsRequestHandler implements
        IRequestHandler<ExportGridsRequest> {

    private static Timer exportGridsTimer = new Timer(true);

    @Override
    public Object handleRequest(ExportGridsRequest request) throws Exception {
        ServerResponse<String> sr = new ServerResponse<String>();

        SvcBackupUtil.execute("export_grids", request.getMode(), request
                .getSite().toLowerCase());

        ServiceBackupNotificationManager
                .sendMessageNotification("Digital data successfully exported.");
        ServiceBackupNotificationManager.sendProgressNotification(100);
        return sr;
    }

    public void exportGridsCron() throws Exception {
        Random rand = new Random(System.currentTimeMillis());
        Date executionDate = new Date(System.currentTimeMillis()
                + Math.abs(rand.nextInt() % 45) * 60 * 1000);
        exportGridsTimer.schedule(new ExportGridsTask(executionDate),
                executionDate);
    }
}
