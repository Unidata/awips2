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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.smartinit.SmartInitRecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.request.SmartInitRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Request handler for SmartInitRequest
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 12, 2010           dgilling  Initial creation
 * Jun 13, 2013  2044     randerso  Refactored to use IFPServer
 * Sep 13, 2013  2368     rjpeter   Used durable jms settings.
 * Sep 12, 2016  5861     randerso  Remove references to IFPServerConfigManager
 *                                  which was largely redundant with IFPServer.
 *
 * </pre>
 *
 * @author dgilling
 */

public class SmartInitRequestHandler extends BaseGfeRequestHandler
        implements IRequestHandler<SmartInitRequest> {

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public ServerResponse<Object> handleRequest(SmartInitRequest request)
            throws Exception {
        ServerResponse<Object> sr = new ServerResponse<Object>();
        IFPServer ifpServer = getIfpServer(request);
        GridParmManager gridParmMgr = ifpServer.getGridParmMgr();

        String modelTime = request.getModelTime();
        String smartInit = request.getModuleName();
        boolean calcAll = request.isCalculateAll();

        IFPServerConfig config = ifpServer.getConfig();
        List<String> affectedModels = config.gfeModelsFromInit(smartInit);
        if (affectedModels == null) {
            sr.addMessage(
                    "Invalid smart init module " + smartInit + " specified.");
            return sr;
        }

        List<DatabaseID> inventory = getD2DDatabases(gridParmMgr);

        for (String model : affectedModels) {
            DatabaseID dbId = findDatabase(model, modelTime, inventory);

            if (dbId != null) {
                StringBuilder manualInitString = new StringBuilder(
                        dbId.toString());
                manualInitString.append((calcAll ? ":1:" : ":0:"));
                manualInitString.append(smartInit);
                manualInitString.append(":");
                manualInitString
                        .append(SmartInitRecord.MANUAL_SMART_INIT_PRIORITY);

                EDEXUtil.getMessageProducer().sendAsyncUri(
                        "jms-durable:queue:manualSmartInit",
                        manualInitString.toString());
            } else {
                sr.addMessage(
                        "No valid model data could be retrieved for model "
                                + model);
            }
        }

        return sr;
    }

    /**
     * @param d2dModel
     * @param modelTime
     * @param inventory
     * @return
     */
    private DatabaseID findDatabase(String d2dModel, String modelTime,
            List<DatabaseID> inventory) {
        if (!modelTime.isEmpty()) {
            for (DatabaseID dbId : inventory) {
                if ((dbId.getModelName().equals(d2dModel))
                        && (dbId.getModelTime().equals(modelTime))) {
                    return dbId;
                }
            }
            return null;
        } else {
            Date newestModelTime = new Date(0);
            DatabaseID newestModel = null;
            for (DatabaseID dbId : inventory) {
                Date toCheck = dbId.getModelDate();
                if ((dbId.getModelName().equals(d2dModel))
                        && (newestModelTime.compareTo(toCheck) < 1)) {
                    newestModel = dbId;
                    newestModelTime = toCheck;
                }
            }
            return newestModel;
        }
    }

    /**
     * @return
     */
    private List<DatabaseID> getD2DDatabases(GridParmManager gridParmMgr) {
        List<DatabaseID> d2dDbIds = new ArrayList<DatabaseID>();
        List<DatabaseID> gridDbIds = gridParmMgr.getDbInventory().getPayload();
        for (DatabaseID dbId : gridDbIds) {
            if (dbId.getDbType().equals("D2D")) {
                d2dDbIds.add(dbId);
            }
        }

        return d2dDbIds;
    }
}
