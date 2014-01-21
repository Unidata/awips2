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
package com.raytheon.edex.plugin.text.maintenance.archiver;

import java.util.Date;

import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.archive.IPluginArchiveFileNameFormatter;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * Properly stores StdTextProducts by time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2012            dgilling    Initial creation
 * Nov 05, 2013 2499       rjpeter     Moved IPluginArchiveFileNameFormatter.
 * Dec 13, 2013 2555       rjpeter     Refactored.
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class TextArchiveFileNameFormatter implements
        IPluginArchiveFileNameFormatter {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextArchiveFileNameFormatter.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.archive.IPluginArchiveFileNameFormatter#getFilename
     * (java.lang.String, com.raytheon.uf.edex.database.plugin.PluginDao,
     * com.raytheon.uf.common.dataplugin.persist.PersistableDataObject)
     */
    @Override
    public String getFilename(String pluginName, PluginDao dao,
            PersistableDataObject<?> pdo) {
        String path = null;
        if (pdo instanceof StdTextProduct) {
            StdTextProduct casted = (StdTextProduct) pdo;

            // no refTime to use, so we use creation time
            Date time = new Date(casted.getRefTime());
            path = pluginName
                    + DefaultPathProvider.fileNameFormat.get().format(time);
        } else {
            statusHandler.error("Invalid PersistableDataObject class "
                    + pdo.getClass()
                    + "sent to TextArchiveFileNameFormatter to archive");
        }
        return path;
    }
}
