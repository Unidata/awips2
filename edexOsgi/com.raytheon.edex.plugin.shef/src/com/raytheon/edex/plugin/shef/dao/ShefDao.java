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
package com.raytheon.edex.plugin.shef.dao;

import java.io.File;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.ohd.AppsDefaults;

public class ShefDao extends DefaultPluginDao {

    /**
     * 
     * @param pluginName
     * @throws PluginException
     */
    public ShefDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public void purgeAllData() {
        logger
                .warn("Shef Purge All behavior not specified. No data will be purged.");
    }

    @Override
    public void purgeExpiredData() throws PluginException {
        /*
         * Purge the file system
         */
        logger.info("Purging Hydro file system...");
        File metarInputFile = new File(AppsDefaults.getInstance()
                .getToken("whfs_local_data_dir")
                + "/metar_input");
        if (!metarInputFile.exists()) {
            metarInputFile.mkdir();
        }
    }
}
