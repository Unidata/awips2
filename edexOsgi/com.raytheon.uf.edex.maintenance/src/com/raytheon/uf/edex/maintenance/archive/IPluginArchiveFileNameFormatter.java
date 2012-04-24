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

import java.util.Calendar;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2012            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public interface IPluginArchiveFileNameFormatter {

    /**
     * 
     * @param pluginName
     * @param dao
     * @param pdoMap
     *            The current pdos by file. This map will be merged with pdos,
     *            if a key was not referenced by pdos it will be removed and
     *            returned in the returned map for storage.
     * @param startTime
     * @param endTime
     * @return The pdos to save to disk. If sortPdosByFiles did not store any
     *         entries from pdos into a file listed in currentPdoMap then that
     *         entry will be returned in a new map and removed from
     *         currentPdoMap.
     * @throws DataAccessLayerException
     *             If the DAO is unable to retrieve the records from the
     *             database.
     */
    public abstract Map<String, List<PersistableDataObject>> getPdosByFile(
            String pluginName, PluginDao dao,
            Map<String, List<PersistableDataObject>> pdoMap,
            Calendar startTime, Calendar endTime)
            throws DataAccessLayerException;
}
