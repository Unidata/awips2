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

import java.util.Properties;

import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.GetGfeStartCmdRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Request handler for {@code GetGfeStartCmdRequest}. Using the configuration
 * file svcbu.properties, this determines the command needed to launch GFE as
 * the failed site and returns it to the requester.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 09, 2011            bphillip    Initial creation
 * Feb 13, 2015  #4103     dgilling    Rewrite to stop using createGFEStartScript.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public final class GetGfeStartCmdRequestHandler implements
        IRequestHandler<GetGfeStartCmdRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CleanupSvcBuRequestHandler.class);

    @Override
    public String handleRequest(final GetGfeStartCmdRequest request)
            throws Exception {
        Properties svcbuProps = SvcBackupUtil.getSvcBackupProperties();

        String svcbuUser = (svcbuProps.getProperty("SVCBU_USER") != null) ? svcbuProps
                .getProperty("SVCBU_USER").trim() : "0";
        String svcbuUserName = (svcbuProps.getProperty("SVCBU_USER_ID") != null) ? svcbuProps
                .getProperty("SVCBU_USER_ID").trim() : "";

        if (svcbuUser.equals("1") && svcbuUserName.isEmpty()) {
            statusHandler
                    .info("You do not have a user id configured for ServiceBackup");
            statusHandler.info("GFE will start with your regular user id");
        }

        StringBuilder cmdLine = new StringBuilder(
                svcbuProps.getProperty("CAVE_LAUNCH_SCRIPT"));
        cmdLine.append(" -site ").append(request.getSite());
        if ((svcbuUser.equals("1")) && (!svcbuUserName.isEmpty())) {
            cmdLine.append(" -u ").append(svcbuUserName);
        }
        cmdLine.append(" -perspective GFE");

        return cmdLine.toString();
    }
}
