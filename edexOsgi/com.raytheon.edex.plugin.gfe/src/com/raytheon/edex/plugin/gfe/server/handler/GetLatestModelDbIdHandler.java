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

import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetLatestModelDbIdRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Handler for getting the latest DatabaseID for a given model name and site ID.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2010            dgilling     Initial creation
 * May 22, 2013  2025      dgilling     Re-implement for new GFE db schema.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GetLatestModelDbIdHandler implements
        IRequestHandler<GetLatestModelDbIdRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public DatabaseID handleRequest(GetLatestModelDbIdRequest request)
            throws Exception {
        GFEDao dao = new GFEDao();
        return dao.getLatestDbIdByModelName(request.getSiteID(),
                request.getModelName());
    }

}
