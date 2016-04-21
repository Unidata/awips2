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
package com.raytheon.edex.plugin.gfe.db.dao;

import java.io.File;

import com.raytheon.edex.plugin.gfe.server.database.GridDatabase;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.util.GfeUtil;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;

/**
 * Provider for GFE that uses the GfeUtil to get the HDF5 path and file names.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2013            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class GFEPathProvider extends DefaultPathProvider {

    /**
     * Constructor.
     */
    public GFEPathProvider() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider#getHDFFileName
     * (java.lang.String,
     * com.raytheon.uf.common.dataplugin.persist.IPersistable)
     */
    @Override
    public String getHDFFileName(String pluginName, IPersistable persistable) {
        String name = null;
        if (persistable instanceof GFERecord) {
            GFERecord gfeRecord = (GFERecord) persistable;
            File hdf5File = GfeUtil.getHdf5File(GridDatabase.gfeBaseDataDir,
                    gfeRecord.getParmId(), gfeRecord.getTimeRange());
            name = hdf5File.toString();
            name = name.substring(name.lastIndexOf(File.separator) + 1);
        }
        return name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider#getHDFPath
     * (java.lang.String,
     * com.raytheon.uf.common.dataplugin.persist.IPersistable)
     */
    @Override
    public String getHDFPath(String pluginName, IPersistable persistable) {
        String path = null;
        path = super.getHDFPath(pluginName, persistable);
        if (persistable instanceof GFERecord) {
            GFERecord gfeRecord = (GFERecord) persistable;
            File pathDir = GfeUtil.getHdf5Dir(GridDatabase.gfeBaseDataDir,
                    gfeRecord.getDbId());
            path = pathDir.toString();
            path = path.substring(pluginName.length() + 1);
        }
        return path;
    }
}
