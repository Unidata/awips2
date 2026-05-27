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
package com.raytheon.uf.edex.maintenance;

import java.io.File;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.registry.GenericRegistry;
import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Repacks all data hdf5 files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 1, 2011             njensen     Initial creation
 * Jan 14, 2013 1469       bkowal      Removed the hdf5 data directory
 * Mar 21, 2013 1814       rjpeter     Fixed logging of exception.
 * Feb 16, 2016 5363       dhladky     Added ability to register non-point data plugins for repack.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataStoreRepacker extends
        GenericRegistry<String, PluginProperties> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataStoreRepacker.class);

    private boolean isScanned = false;

    private Compression compression = Compression.NONE;

    public DataStoreRepacker(String compression) {
        this.compression = Compression.valueOf(compression);
    }

    /**
     * repack the list of registered plugins
     */
    public void repack() {
        if (!isScanned) {
            scanForPointDataPlugins();
        }

        for (String plugin : getRegisteredObjects()) {

            IDataStore ds = DataStoreFactory.getDataStore(new File(plugin));
            try {
                statusHandler.info("Starting repack of " + plugin
                        + " datastore");
                long time = TimeUtil.currentTimeMillis();
                ds.repack(compression);
                long etime = TimeUtil.currentTimeMillis();
                statusHandler.info("Completed repack of " + plugin
                        + " datastore. Took: " + (etime - time) + " ms");
            } catch (StorageException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to repack datastore for plugin " + plugin, e);
            }
        }
    }

    /**
     * Finds point data plugins and adds them to the repack registry
     */
    private void scanForPointDataPlugins() {

        Set<String> plugins = PluginRegistry.getInstance()
                .getRegisteredObjects();
        for (String plugin : plugins) {
            PluginProperties props = PluginRegistry.getInstance()
                    .getRegisteredObject(plugin);
            Class<?> daoClass = props.getDao();
            if (PointDataPluginDao.class.isAssignableFrom(daoClass)) {
                try {
                    register(plugin, props);
                } catch (RegistryException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed registering plugin " + plugin
                                    + " for repack.", e);
                }
            }
        }

        isScanned = true;
    }

}
