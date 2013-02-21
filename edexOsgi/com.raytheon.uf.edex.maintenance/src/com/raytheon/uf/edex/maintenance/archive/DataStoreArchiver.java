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
package com.raytheon.uf.edex.maintenance.archive;

import java.io.File;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.maintenance.archive.config.DataArchiveConfig;

/**
 * Uses the repack feature of IDataStore to archive data by repacking it to a
 * specified compression at the hdf5 dataset level and moving the resulting file
 * to the archive dir.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2011            njensen     Initial creation
 * Jan 14, 2013 1469      bkowal      Removed the hdf5 data directory.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataStoreArchiver {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataStoreArchiver.class);

    private Compression compression = Compression.NONE;

    public DataStoreArchiver(String compression) {
        this.compression = Compression.valueOf(compression);
    }

    public void archiveFiles(String[] hdf5Files, String archiveDir,
            DataArchiveConfig conf) {
        for (String hdf5File : hdf5Files) {
            IDataStore ds = DataStoreFactory.getDataStore(new File(hdf5File));
            String outputDir = archiveDir; // + dirs of hdf5 file

            try {
                // data must be older than 30 minutes, and no older than hours
                // to keep hours need to lookup plugin and see if compression
                // matches, or embed in configuration the compression level on
                // archive, but would still need to lookup plugin
                ds.copy(outputDir, compression, "lastArchived", 1800000,
                        conf.getHoursToKeep() * 60000 + 1800000);
            } catch (StorageException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage());
            }
        }
    }
}
