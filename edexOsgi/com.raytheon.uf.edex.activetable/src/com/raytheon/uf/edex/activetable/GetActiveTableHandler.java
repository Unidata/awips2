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

import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.GetActiveTableRequest;
import com.raytheon.uf.common.activetable.GetActiveTableResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GetActiveTableHandler implements
        IRequestHandler<GetActiveTableRequest> {

    @Override
    public GetActiveTableResponse handleRequest(GetActiveTableRequest request)
            throws Exception {
        List<ActiveTableRecord> list = ActiveTable.getActiveTable(
                request.getSiteID(),
                request.getMode(),
                request.getPhensigList(),
                request.getAct(),
                request.getEtn(),
                request.getRequestValidTimes() != null
                        && request.getRequestValidTimes());
        GetActiveTableResponse resp = new GetActiveTableResponse();
        resp.setActiveTable(list);
        resp.setMode(request.getMode());
        return resp;
    }

}
