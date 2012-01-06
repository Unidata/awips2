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

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2010            wkwock     Initial creation
 *
 * </pre>
 *
 * @author wkwock
 * @version 1.0	
 */

import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.gfe.request.ClearPracticeVTECTableRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.activetable.ActiveTable;
import com.raytheon.uf.edex.database.DataAccessLayerException;

public class ClearPracticeVTECTableHandler implements
        IRequestHandler<ClearPracticeVTECTableRequest> {

    @Override
    public ServerResponse<List<String>> handleRequest(
            ClearPracticeVTECTableRequest request) throws Exception {

        ServerResponse<List<String>> sr = new ServerResponse<List<String>>();
        Log logger = LogFactory.getLog(getClass());

        try {
            ActiveTable.clearPracticeTable(request.getRequestedSiteId(),
                    request.getMode());

        } catch (DataAccessLayerException e) {
            logger.error("Error failed to clear practice VTEC table", e);
            sr.addMessage("Error failed to clear practice VTEC table");
        }

        logger
                .info("Practice VTEC table (practice_activetable in DB) has been cleared.");
        return sr;
    }
}
