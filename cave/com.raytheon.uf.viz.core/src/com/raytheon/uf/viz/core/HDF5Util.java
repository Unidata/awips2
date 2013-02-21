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
package com.raytheon.uf.viz.core;

import java.io.File;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.localization.IPathManager;

public class HDF5Util {

    /**
     * Finds the insert hour of the data record at the specified index.
     * 
     * @param object
     *            data record to process
     * @return the file to open
     */
    public static File findHDF5Location(PluginDataObject object) {
        File file = null;
        if (object instanceof IPersistable) {
            IPersistable persistable = (IPersistable) object;

            IHDFFilePathProvider pathProvider = persistable
                    .getHDFPathProvider();

            String path = pathProvider.getHDFPath(object.getPluginName(),
                    persistable);
            String fileName = pathProvider.getHDFFileName(
                    object.getPluginName(), persistable);

            file = new File(object.getPluginName() + IPathManager.SEPARATOR
                    + path + IPathManager.SEPARATOR + fileName);
        }

        return file;
    }
}
