/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.obs.ogc.metar;

import com.raytheon.edex.plugin.obs.ObsDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.plugin.PurgeResults;
import com.raytheon.uf.edex.ogc.common.db.PurgeNotification;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public class MetarNotifyDao extends ObsDao {


	private final PurgeNotification notify;

	/**
	 * @throws PluginException
	 */
	public MetarNotifyDao() throws PluginException {
		this("obs");
	}

	/**
	 * @param pluginName
	 * @throws PluginException
	 */
	public MetarNotifyDao(String pluginName) throws PluginException {
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

}
