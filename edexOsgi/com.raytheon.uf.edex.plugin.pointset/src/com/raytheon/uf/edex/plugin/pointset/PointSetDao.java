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
package com.raytheon.uf.edex.plugin.pointset;

import java.nio.Buffer;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.pointset.PointSetRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.edex.database.plugin.PluginDao;

/**
 * 
 * {@link PluginDao} for {@link PointSetRecord}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class PointSetDao extends PluginDao {

    public PointSetDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    public PointSetDao() throws PluginException {
        super("pointset");
    }

    @Override
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        if (obj instanceof PointSetRecord) {
            PointSetRecord points = (PointSetRecord) obj;
            Buffer data = points.getData();
            dataStore.addDataRecord(DataStoreFactory.createStorageRecord(
                    "Data", points.getDataURI(), data.array()));
        } else if (obj != null) {
            throw new IllegalArgumentException("Cannot handle "
                    + obj.getClass().getSimpleName());
        }
        return dataStore;
    }

}
