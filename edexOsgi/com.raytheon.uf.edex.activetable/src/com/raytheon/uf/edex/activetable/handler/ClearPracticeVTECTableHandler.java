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
package com.raytheon.uf.edex.activetable.handler;

import com.raytheon.uf.common.activetable.request.ClearPracticeVTECTableRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.activetable.ActiveTable;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Request handler for clearing the practice VTEC active table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2010            wkwock      Initial creation
 * Apr 09, 2014  #3004     dgilling    Move to activetable plugin, remove GFE
 *                                     dependencies.
 * Apr 28, 2015  #4027     randerso    Added clearing of practice warning table
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */
public class ClearPracticeVTECTableHandler implements
        IRequestHandler<ClearPracticeVTECTableRequest> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClearPracticeVTECTableHandler.class);

    @Override
    public Boolean handleRequest(ClearPracticeVTECTableRequest request)
            throws Exception {
        try {
            ActiveTable.clearPracticeTable(request.getSiteID());
            PluginDao dao = PluginFactory.getInstance().getPluginDao(
                    "practicewarning");
            dao.purgeAllData();

        } catch (DataAccessLayerException e) {
            statusHandler.error("Error failed to clear practice VTEC table", e);
            throw new Exception("Unable to clear practice VTEC table.", e);
        }

        statusHandler
                .info("Practice VTEC table (practice_activetable in DB) has been cleared.");
        return Boolean.TRUE;
    }
}
