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
package com.raytheon.edex.plugin.gfe.server.handler.svcbu;

import java.io.File;

import com.raytheon.edex.plugin.gfe.db.dao.GFEDao;
import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.edex.plugin.gfe.svcbackup.ServiceBackupNotificationManager;
import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.request.CleanupSvcBuRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class CleanupSvcBuRequestHandler implements
        IRequestHandler<CleanupSvcBuRequest> {

    @Override
    public Object handleRequest(CleanupSvcBuRequest request) {

        ServerResponse<String> sr = new ServerResponse<String>();
        SvcBackupUtil.removeLocks();
        GFEDao dao = null;
        try {
            dao = new GFEDao();
            ServiceBackupNotificationManager
                    .sendMessageNotification("Purging database for "
                            + request.getFailedSite());
            dao.purgeDatabaseForSite(request.getFailedSite());
            ServiceBackupNotificationManager
                    .sendMessageNotification("Purging HDF5 for "
                            + request.getFailedSite());
            purgeHDF5(request.getFailedSite());

            ServiceBackupNotificationManager
                    .sendMessageNotification("Running cleanup script for for "
                            + request.getFailedSite());
            SvcBackupUtil.execute("cleanup_svcbk", request.getPrimarySite()
                    .toLowerCase(), request.getFailedSite().toLowerCase());
        } catch (PluginException e) {
            sr.addMessage("Error instantiating GFE dao! "
                    + e.getLocalizedMessage());
        } catch (DataAccessLayerException e) {
            sr.addMessage("Error purging data for " + request.getFailedSite()
                    + " " + e.getLocalizedMessage());
        } catch (Exception e) {
            sr.addMessage("Error executing cleanup script! "
                    + e.getLocalizedMessage());
        }
        return sr;
    }

    private void purgeHDF5(String failedSite) throws DataAccessLayerException {
        File siteDataStoreFile = new File(GridDatabase.gfeBaseDataDir
                + failedSite.toUpperCase());
        IDataStore ds = DataStoreFactory.getDataStore(siteDataStoreFile);
        try {
            ds.deleteFiles(null);
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Error deleting HDF5 files for site " + failedSite, e);
        }
    }
}
