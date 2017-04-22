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
import com.raytheon.uf.common.dataplugin.gfe.request.ImportDigitalDataRequest;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Request handler for {@code ImportDigitalDataRequest}. Causes an MHS request
 * to be sent to have the GFE grids for the specified site to be sent to this
 * server. Should be used only during service backup mode.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 04, 2011            bphillip     Initial creation
 * Mar 17, 2015   4103     dgilling     Support new Service Backup GUI.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public final class ImportDigitalDataRequestHandler implements
        IRequestHandler<ImportDigitalDataRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ImportDigitalDataRequestHandler.class);

    @Override
    public JobProgress handleRequest(final ImportDigitalDataRequest request)
            throws Exception {
        try {
            statusHandler.info("Requesting GFE grids for site "
                    + request.getFailedSite());
            SvcBackupUtil.execute("request_grids", request.getFailedSite()
                    .toLowerCase());
        } catch (Exception e) {
            statusHandler.error("Error executing request_grids for site "
                    + request.getFailedSite(), e);
            return JobProgress.FAILED;
        }

        return JobProgress.SUCCESS;
    }
}
