/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.madis.ogc;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.madis.MadisRecord;
import com.raytheon.uf.edex.database.plugin.PurgeResults;
import com.raytheon.uf.edex.ogc.common.db.PurgeNotification;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2013            dhladky     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0 
 */
public class MadisNotifyDao extends PointDataPluginDao<MadisRecord> {


    private final PurgeNotification notify;

    /**
     * @throws PluginException
     */
    public MadisNotifyDao() throws PluginException {
        this("madis");
    }

    /**
     * @param pluginName
     * @throws PluginException
     */
    public MadisNotifyDao(String pluginName) throws PluginException {
        super(pluginName);
        notify = new PurgeNotification(pluginName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.database.plugin.PluginDao#purgeAllData()
     */
    @Override
    public void purgeAllData() throws PluginException {
        super.purgeAllData();
        notify.purgeAllData();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.database.plugin.PluginDao#purgeExpiredData()
     */
    @Override
    public void purgeExpiredData() throws PluginException {
        PurgeResults res = super.purgeExpiredDataWithResults();
        notify.purgeExpiredData(res);
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public MadisRecord newObject() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getPointDataFileName(MadisRecord p) {
        // TODO Auto-generated method stub
        return null;
    }

}

