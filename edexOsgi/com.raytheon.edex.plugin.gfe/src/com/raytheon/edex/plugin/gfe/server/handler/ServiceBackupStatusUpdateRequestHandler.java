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
package com.raytheon.edex.plugin.gfe.server.handler;

import com.raytheon.edex.plugin.gfe.svcbackup.ServiceBackupNotificationManager;
import com.raytheon.uf.common.dataplugin.gfe.request.ServiceBackupStatusUpdateRequest;
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
 * Oct 26, 2010            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class ServiceBackupStatusUpdateRequestHandler implements
        IRequestHandler<ServiceBackupStatusUpdateRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<String> handleRequest(
            ServiceBackupStatusUpdateRequest request) throws Exception {
        ServerResponse<String> sr = new ServerResponse<String>();
        String msg = request.getStatusMessage();
        try {
            int percent = Integer.parseInt(msg);
            ServiceBackupNotificationManager.sendProgressNotification(percent);
        } catch (NumberFormatException e) {
            ServiceBackupNotificationManager.sendMessageNotification(msg);
        }
        return sr;
    }

}
