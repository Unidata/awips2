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

import java.util.List;

import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.dataplugin.gfe.request.GetActiveTableRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.activetable.ActiveTable;

/**
 * Gets the VTEC active table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 19, 2009            njensen     Initial creation
 * 09/22/09     3058       rjpeter     Converted to IRequestHandler
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
public class GetActiveTableHandler implements
        IRequestHandler<GetActiveTableRequest> {
    @Override
    public ServerResponse<List<ActiveTableRecord>> handleRequest(
            GetActiveTableRequest request) throws Exception {
        ServerResponse<List<ActiveTableRecord>> sr = new ServerResponse<List<ActiveTableRecord>>();
        sr.setPayload(ActiveTable.getActiveTable(request.getRequestedSiteId(),
                request.getMode(), null, null, null, request.getWfos()));
        return sr;
    }
}
