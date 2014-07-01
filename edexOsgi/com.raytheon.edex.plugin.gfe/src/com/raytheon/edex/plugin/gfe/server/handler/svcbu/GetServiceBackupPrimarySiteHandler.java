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

import java.util.Set;

import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.gfe.request.GetServiceBackupPrimarySiteRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Handler for <code>CheckServiceBackupPrimarySiteRequest</code>. Determines
 * whether the specified site id has been configured as one of service backup's
 * primary sites.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date           Ticket#    Engineer    Description
 * ------------   ---------- ----------- --------------------------
 * Jun 10, 2014   DR-17401     lshi        Initial creation
 * 
 * </pre>
 * 
 * @author lshi
 * @version 1.0
 */

public class GetServiceBackupPrimarySiteHandler implements
        IRequestHandler<GetServiceBackupPrimarySiteRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<Set<String>> handleRequest(
            GetServiceBackupPrimarySiteRequest request) throws Exception {
        ServerResponse<Set<String>> sr = new ServerResponse<Set<String>>();
        Set<String> primarySites = SvcBackupUtil.getPrimarySites();
        sr.setPayload(primarySites);
        
        return sr;
    }
}

