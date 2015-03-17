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

import java.nio.file.Files;
import java.nio.file.Path;

import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.AbortOperationRequest;
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
 * Aug 12, 2011            bphillip     Initial creation
 * Feb 13, 2015  #4103     dgilling     Use site id.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public final class AbortOperationRequestHandler implements
        IRequestHandler<AbortOperationRequest> {

    @Override
    public ServerResponse<Object> handleRequest(AbortOperationRequest request)
            throws Exception {
        ServerResponse<Object> sr = new ServerResponse<>();
        Path lockFilePath = SvcBackupUtil.getLockDir(request.getSiteID())
                .resolve(request.getOperation());

        if (!Files.deleteIfExists(lockFilePath)) {
            sr.addMessage("Unable to delete lock file: "
                    + request.getOperation());
        }

        return sr;
    }
}
