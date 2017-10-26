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
package com.raytheon.uf.edex.plugin.goesr.dmw.dao;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.goesr.dmw.DMWRecord;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.StorageStatus;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * Dao for Derived Motion Winds.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2015  4334       nabowle     Initial creation
 * 
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public class DMWDao extends PointDataPluginDao<DMWRecord> {

    /**
     * @param pluginName
     * @throws PluginException
     */
    public DMWDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getKeysRequiredForFileName
     * ()
     */
    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.edex.pointdata.PointDataPluginDao#newObject()
     */
    @Override
    public DMWRecord newObject() {
        return new DMWRecord();
    }



    /*
     * The following methods are overridden to effectively be NOOPs in order to
     * bypass HDF5.
     */

    /**
     * Does nothing and returns a new StorageStatus.
     */
    @Override
    public StorageStatus persistToHDF5(PluginDataObject... records) {
        return new StorageStatus();
    }

    /**
     * Does nothing and returns a new PointDataDescription.
     */
    @Override
    public PointDataDescription getPointDataDescription(Map<String, Object> obj) {
        return new PointDataDescription();
    }

    /**
     * Does nothing and returns a new PointDataContainer.
     */
    @Override
    public PointDataContainer getPointData(File file, int[] indexes, int[] ids,
            String[] attributes, LevelRequest request) throws StorageException,
            FileNotFoundException {
        return new PointDataContainer();
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.edex.pointdata.PointDataPluginDao#getPointDataFileName
     * (com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public String getPointDataFileName(DMWRecord p) {
        // not needed for this datatype, but must be overridden.
        return "dmw.h5";
    }

}
