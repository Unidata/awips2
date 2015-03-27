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

import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.auth.exception.AuthorizationException;
import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.dataplugin.gfe.request.ExportConfRequest;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.auth.req.AbstractPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.resp.AuthorizationResponse;

/**
 * Request handler for {@code ExportConfRequest}. This handler will export the
 * local site's configuration data to the central server for use in a service
 * backup scenario.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2011            bphillip     Initial creation
 * Mar 17, 2015  #4103     dgilling     Stop using ServiceBackupNotificationManager,
 *                                      supoprt new Service Backup GUI.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public final class ExportConfRequestHandler extends
        AbstractPrivilegedRequestHandler<ExportConfRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExportConfRequestHandler.class);

    @Override
    public JobProgress handleRequest(final ExportConfRequest request)
            throws Exception {
        AuthorizationResponse response = authorized(request.getUser(), request);
        if (!response.isAuthorized()) {
            statusHandler.error("User " + request.getUser()
                    + " is not export configuration for site "
                    + request.getSite());
            return JobProgress.FAILED;
        }

        try {
            SvcBackupUtil.execute("export_configuration", request.getSite()
                    .toLowerCase());
            statusHandler
                    .info("Configuration successfully sent to central server");
        } catch (Exception e) {
            statusHandler.error("Error exporting configuration for site "
                    + request.getSite(), e);
            return JobProgress.FAILED;
        }

        return JobProgress.SUCCESS;
    }

    @Override
    public AuthorizationResponse authorized(IUser user,
            ExportConfRequest request) throws AuthorizationException {
        return SvcBackupUtil.authorizeWithLocalization(user, request);
    }
}
