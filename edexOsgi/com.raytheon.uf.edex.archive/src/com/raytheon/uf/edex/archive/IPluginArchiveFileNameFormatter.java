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

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * Interface for archive file name formatters.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2012            dgilling    Initial creation
 * Nov 05, 2013 2499       rjpeter     Repackaged
 * Dec 13, 2013 2555       rjpeter     Refactored
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public interface IPluginArchiveFileNameFormatter {
    /**
     * Returns base file name for the pdo. In the case of IPersistable objects,
     * it should match the h5 file.
     * 
     * @param pluginName
     *            The plugin name.
     * @param dao
     *            The dao for the object.
     * @param pdo
     *            The object to look up.
     * @return
     */
    public String getFilename(String pluginName, PluginDao dao,
            PersistableDataObject<?> pdo);
}
