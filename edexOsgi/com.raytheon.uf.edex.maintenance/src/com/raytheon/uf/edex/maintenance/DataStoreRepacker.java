package com.raytheon.uf.edex.maintenance;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

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

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 1, 2011            njensen     Initial creation
 * Jan 14, 2013 1469      bkowal      Removed the hdf5 data directory
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataStoreRepacker {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataStoreRepacker.class);

    private List<String> pluginsToRepack;

    private Compression compression = Compression.NONE;

    public DataStoreRepacker(String compression) {
        this.compression = Compression.valueOf(compression);
    }

    public void repack() {
        if (pluginsToRepack == null) {
            scanForPluginsToRepack();
        }

        // TODO change log statement if more than pointdata is hooked into this
        statusHandler.info("Starting repack of pointdata datastore");
        for (String plugin : pluginsToRepack) {
            IDataStore ds = DataStoreFactory.getDataStore(new File(plugin));
            try {
                ds.repack(compression);
            } catch (StorageException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage());
            }
        }
        // TODO change log statement if more than pointdata is hooked into this
        statusHandler.info("Completed repack of pointdata datastore");
    }

    private void scanForPluginsToRepack() {
        // TODO currently this is set up to just detect pointdata plugins, this
        // can be changed in the future. If this is changed, you need to
        // change the log statements in repack()
        pluginsToRepack = new ArrayList<String>();
        Set<String> plugins = PluginRegistry.getInstance()
                .getRegisteredObjects();
        for (String plugin : plugins) {
            PluginProperties props = PluginRegistry.getInstance()
                    .getRegisteredObject(plugin);
            Class<?> daoClass = props.getDao();
            if (PointDataPluginDao.class.isAssignableFrom(daoClass)) {
                pluginsToRepack.add(plugin);
            }
        }
    }
}
