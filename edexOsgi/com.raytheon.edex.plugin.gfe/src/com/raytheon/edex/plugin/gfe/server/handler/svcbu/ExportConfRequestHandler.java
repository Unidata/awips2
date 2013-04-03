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

import com.raytheon.edex.plugin.gfe.svcbackup.ServiceBackupNotificationManager;
import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportConfRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;

/**
 * Exports configuration?
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

public class ExportConfRequestHandler extends
        AbstractPrivilegedRequestHandler<ExportConfRequest> {

    @Override
    public Object handleRequest(ExportConfRequest request) throws Exception {
        AuthorizationResponse response = authorized(request.getUser(), request);
        ServerResponse<String> sr = new ServerResponse<String>();
        if (response.isAuthorized()) {
            SvcBackupUtil.execute("export_configuration", request.getSite()
                    .toLowerCase());
            ServiceBackupNotificationManager
                    .sendMessageNotification("Configuration successfully sent to central server");
            ServiceBackupNotificationManager.sendProgressNotification(100);
        } else {
            ServiceBackupNotificationManager.sendMessageNotification("User "
                    + request.getUser()
                    + " is not authorized to perform this operation");
            sr.addMessage(response.getResponseMessage());
        }
        return sr;
    }

    @Override
    public AuthorizationResponse authorized(IUser user,
            ExportConfRequest request) throws AuthorizationException {
        return SvcBackupUtil.authorizeWithLocalization(user, request);
    }
}
