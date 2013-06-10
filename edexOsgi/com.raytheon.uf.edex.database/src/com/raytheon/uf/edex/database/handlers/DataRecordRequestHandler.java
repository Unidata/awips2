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
package com.raytheon.uf.edex.database.handlers;

import java.util.List;
import java.util.ArrayList;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.request.DataRecordRequest;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Handler class for DataRecordRequests. Utilizes IDataStore to retrieve
 * IDataRecords for the specified PluginDataObject.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 9, 2013            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class DataRecordRequestHandler implements
        IRequestHandler<DataRecordRequest> {

    @Override
    public Object handleRequest(DataRecordRequest request) throws Exception {
        PluginDataObject pdo = request.getPdo();
        PluginDao pluginDao = PluginFactory.getInstance().getPluginDao(
                pdo.getPluginName());
        IDataStore dataStore = pluginDao.getDataStore((IPersistable) pdo);

        IDataRecord[] dataRecords = dataStore.retrieve(pdo.getDataURI());
        // Make the response generic
        List<Object> results = new ArrayList<Object>();
        for (IDataRecord dataRecord : dataRecords) {
            results.add(dataRecord);
        }
        return results;
    }
}