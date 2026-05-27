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
package com.raytheon.uf.edex.activetable;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.ActiveTableUtil;
import com.raytheon.uf.common.activetable.GetActiveTableDictRequest;
import com.raytheon.uf.common.activetable.GetActiveTableDictResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Gets the VTEC active table as a List&lt;Map&lt;String,Object&gt;&gt;,
 * relieving Python clients of the need to convert a list of ActiveTableRecords
 * to a Python dictionary (and thus the need for direct access to several
 * server-side scripts).
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
public class GetActiveTableDictHandler implements
        IRequestHandler<GetActiveTableDictRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetActiveTableDictHandler.class);

    @Override
    public GetActiveTableDictResponse handleRequest(
            GetActiveTableDictRequest request) throws Exception {
        String site = request.getRequestedSiteId();
        GetActiveTableDictResponse response = new GetActiveTableDictResponse();
        List<ActiveTableRecord> records = ActiveTable.getActiveTable(site,
                request.getMode(), null, null, null, request.getWfos());
        List<Map<String, Object>> table = ActiveTableUtil.convertToDict(records,
                site);
        response.setActiveTable(table);
        statusHandler.handle(Priority.INFO, "ActiveTable for " + site
                + " sent as dict");
        return response;
    }

}
