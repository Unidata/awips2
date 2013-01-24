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
package com.raytheon.uf.edex.plugin.grid.handler;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.grid.request.DeleteAllGridDataRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Request handler for <code>DeleteAllGridDataRequest</code>.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 2, 2012            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class DeleteAllGridDataHandler implements
        IRequestHandler<DeleteAllGridDataRequest> {

    private static final String PLUGIN_PURGED_TOPIC = "jms-generic:topic:pluginPurged";

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Boolean handleRequest(DeleteAllGridDataRequest request)
            throws Exception {
        Map<String, String> productKeys = new HashMap<String, String>();
        productKeys.put("info.datasetId", request.getModelName());
        GridDao dao = new GridDao();
        dao.purgeAllData(productKeys);

        EDEXUtil.getMessageProducer().sendAsyncUri(PLUGIN_PURGED_TOPIC, "grid");
        return Boolean.TRUE;
    }
}
