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
package com.raytheon.uf.edex.ingest;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Checks database for duplicates of data. Does not account for clustering.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2013 2478       rjpeter     Initial creation
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DupElimSrv {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DupElimSrv.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("DupElim:");

    /**
     * Checks the passed pdos against database for existence. If duplicates
     * found returns a new array containing only the new plugin data objects. If
     * an errors occurs the original pdos array will be returned.
     * 
     * @param pluginName
     * @param pdos
     * @return
     */
    public PluginDataObject[] dupElim(PluginDataObject[] pdos) {
        if ((pdos == null) || (pdos.length == 0)) {
            return new PluginDataObject[0];
        }

        ITimer dupCheckTimer = TimeUtil.getTimer();
        dupCheckTimer.start();

        int numBefore = pdos.length;
        String pluginName = pdos[0].getPluginName();

        try {

            PluginDao dao = PluginFactory.getInstance()
                    .getPluginDao(pluginName);
            List<PluginDataObject> newPdos = new ArrayList<PluginDataObject>(
                    pdos.length);

            // TODO: Bulk querying, groups of 100 using IN lists?
            for (PluginDataObject pdo : pdos) {
                DatabaseQuery dbQuery = new DatabaseQuery(pdo.getClass());
                Map<String, Object> dataUriFields = DataURIUtil
                        .createDataURIMap(pdo);
                for (Map.Entry<String, Object> field : dataUriFields.entrySet()) {
                    String fieldName = field.getKey();
                    // ignore pluginName
                    if (!PluginDataObject.PLUGIN_NAME_ID.equals(fieldName)) {
                        dbQuery.addQueryParam(field.getKey(), field.getValue());
                    }
                }

                @SuppressWarnings("unchecked")
                List<PluginDataObject> dbPdos = (List<PluginDataObject>) dao
                        .queryByCriteria(dbQuery);
                if (CollectionUtil.isNullOrEmpty(dbPdos)) {
                    newPdos.add(pdo);
                } else {
                    // shouldn't be more than 1
                    PluginDataObject dbPdo = dbPdos.get(0);
                    if ((dbPdo == null)
                            || !pdo.getDataURI().equals(dbPdo.getDataURI())) {
                        newPdos.add(pdo);
                    }
                }
            }
            if (pdos.length != newPdos.size()) {
                pdos = newPdos.toArray(new PluginDataObject[newPdos.size()]);
            }
        } catch (Exception e) {
            statusHandler
                    .error("Error occurred during duplicate elimination processing",
                            e);
        }
        dupCheckTimer.stop();

        perfLog.logDuration(pluginName + ": Eliminated "
                + (numBefore - pdos.length) + " of " + numBefore
                + " record(s): Time to process", dupCheckTimer.getElapsedTime());
        return pdos;
    }
}
