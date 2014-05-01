/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.db;

import java.util.Date;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.plugin.PurgeResults;

/**
 * Basic purge notification that sends all kept times to notification topic
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
public class PurgeNotification {

    private final String pluginName;

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CoreDao.class);

    /**
     * @param pluginName
     */
    public PurgeNotification(String pluginName) {
        this.pluginName = pluginName;
    }

    public void purgeAllData() throws PluginException {
        try {
            EDEXUtil.getMessageProducer().sendAsyncUri(getTopicName("all"),
                    null);
        } catch (EdexException e) {
            statusHandler.error("Problem sending purge all message for "
                    + pluginName, e);
        }
    }

    public void purgeExpiredData(PurgeResults res) throws PluginException {
        try {
            if (res.didPurge()) {
                Map<String, Set<Date>> timesKept = res.getTimesKept();
                Set<Date> keepers = new TreeSet<Date>();
                if (timesKept != null) {
                    for (String pk : timesKept.keySet()) {
                        keepers.addAll(timesKept.get(pk));
                    }
                }
                EDEXUtil.getMessageProducer().sendAsyncUri(
                        getTopicName("expired"), keepers);
            }
        } catch (EdexException e) {
            statusHandler.error("Problem sending purge expired message for "
                    + pluginName, e);
        }
    }

    private String getTopicName(String purgeType) {
        return String.format("jms-generic:topic:Purge.%s.%s", purgeType,
                this.pluginName);
    }

}
