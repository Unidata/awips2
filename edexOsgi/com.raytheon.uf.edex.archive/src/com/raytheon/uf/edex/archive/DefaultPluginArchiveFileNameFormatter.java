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
package com.raytheon.uf.edex.archive;

import java.io.File;
import java.util.Date;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * The default implementation of IPluginArchiveFileNameFormatter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2012            dgilling    Initial creation
 * Mar 12, 2013 1783       rferrel     Replace ArrayList with LinkedList to
 *                                      remove excess capacity and reduce
 *                                      time to resize a growing list.
 * Nov 05, 2013 2499       rjpeter     Repackaged
 * Dec 14, 2013 2555       rjpeter     Refactored
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class DefaultPluginArchiveFileNameFormatter implements
        IPluginArchiveFileNameFormatter {
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
        if (pdo instanceof IPersistable) {
            IPersistable persistable = (IPersistable) pdo;
            IHDFFilePathProvider pathProvider = dao.pathProvider;
            path = pathProvider.getHDFPath(pluginName, persistable)
                    + File.separator
                    + pathProvider.getHDFFileName(pluginName, persistable);
        } else {
            String timeString = null;
            PluginDataObject pluginDataObj = (PluginDataObject) pdo;
            if (pdo instanceof PluginDataObject) {
                Date time = pluginDataObj.getDataTime().getRefTimeAsCalendar()
                        .getTime();
                timeString = DefaultPathProvider.fileNameFormat.get().format(
                        time);
            } else {
                // no refTime, use current time as last resort
                timeString = DefaultPathProvider.fileNameFormat.get().format(
                        new Date());
            }

            path = pluginName + timeString;
        }
        return path;
    }
}
