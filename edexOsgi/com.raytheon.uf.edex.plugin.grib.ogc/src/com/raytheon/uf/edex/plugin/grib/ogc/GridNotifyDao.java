/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.grib.ogc;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.edex.database.plugin.PurgeResults;
import com.raytheon.uf.edex.ogc.common.db.PurgeNotification;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;

/**
 * Dao with purge notification for grid
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 6, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GridNotifyDao extends GridDao {

    private final PurgeNotification notify;

    /**
     * @throws PluginException
     */
    public GridNotifyDao() throws PluginException {
        this(GridConstants.GRID);
    }

    /**
     * @param pluginName
     * @throws PluginException
     */
    public GridNotifyDao(String pluginName) throws PluginException {
        super(pluginName);
        this.notify = new PurgeNotification(pluginName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.plugin.grid.dao.GridDao#purgeExpiredData()
     */
    @Override
    public void purgeExpiredData() throws PluginException {
        PurgeResults res = super.purgeExpiredDataWithResults();
        notify.purgeExpiredData(res);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.plugin.grid.dao.GridDao#purgeAllData()
     */
    @Override
    public void purgeAllData() throws PluginException {
        super.purgeAllData();
        notify.purgeAllData();
    }

}
