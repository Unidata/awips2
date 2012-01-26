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
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
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
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataStoreArchiver implements IPluginArchiver {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataStoreArchiver.class);

    private String hdf5Dir;

    private Compression compression = Compression.NONE;

    public DataStoreArchiver(String compression) {
        EnvProperties properties = PropertiesFactory.getInstance()
                .getEnvProperties();
        hdf5Dir = properties.getEnvValue("HDF5DIR");
        this.compression = Compression.valueOf(compression);
    }

    @Override
    public void archivePlugin(String pluginName, String archiveDir,
            DataArchiveConfig conf) {
        String dirToArchive = hdf5Dir + File.separator + pluginName;
        IDataStore ds = DataStoreFactory.getDataStore(new File(dirToArchive));
        String outputDir = archiveDir + File.separator + pluginName;
        statusHandler.info("Archiving " + dirToArchive);

        try {
            ds.repack(compression, outputDir, "lastArchived");
        } catch (StorageException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage());
        }

    }
}
