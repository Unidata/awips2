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
import com.raytheon.edex.plugin.gfe.svcbackup.SvcBackupUtil;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.gfe.request.CleanupSvcBuRequest;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Request handler for {@code CleanupSvcBuRequest}. This handler will delete all
 * data for the failed site from the local server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bphillip     Initial creation
 * Feb 12, 2015  #4103     dgilling     Move lock deletion to cleanup script.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public final class CleanupSvcBuRequestHandler implements
        IRequestHandler<CleanupSvcBuRequest> {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CleanupSvcBuRequestHandler.class);

    @Override
    public JobProgress handleRequest(final CleanupSvcBuRequest request) {
        JobProgress progress = JobProgress.SUCCESS;

        try {
            statusHandler.info("Running cleanup script for for "
                    + request.getFailedSite());
            SvcBackupUtil.execute("cleanup_svcbk", request.getFailedSite()
                    .toLowerCase());
        } catch (Exception e) {
            statusHandler.error("Error executing cleanup script!", e);
            progress = JobProgress.FAILED;
        }

        try {
            statusHandler.info("Purging database for "
                    + request.getFailedSite());
            new GFEDao().purgeDatabaseForSite(request.getFailedSite());
        } catch (DataAccessLayerException e) {
            statusHandler.error(
                    "Error purging GFE data for site "
                            + request.getFailedSite(), e);
            progress = JobProgress.FAILED;
        } catch (PluginException e) {
            statusHandler.error("Error instantiating GFEDao!", e);
            progress = JobProgress.FAILED;
        }

        try {
            statusHandler.info("Purging HDF5 for " + request.getFailedSite());
            purgeHDF5(request.getFailedSite());
        } catch (DataAccessLayerException e) {
            statusHandler.error("Error purging GFE HDF5 data for site "
                    + request.getFailedSite(), e);
            progress = JobProgress.FAILED;
        }

        return progress;
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
