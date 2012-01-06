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

package com.raytheon.uf.edex.purgesrv;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginVersionDao;
import com.raytheon.uf.edex.database.purge.PurgeRule;

/**
 * This class is used to route messages intended for the purge service to the
 * correct JVM
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/15/11      #2469       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class PurgeRequest {

    private static final String PURGE_QUEUE = "jms-generic:queue:purgeRequest";

    /**
     * Routes the given message to the purge queue on the ingest JVM
     * 
     * @param message
     *            The message to route
     * @throws EdexException
     *             If errors occur while routing the message
     */
    public void routePurgeRequest(String message) throws EdexException {
        EDEXUtil.getMessageProducer().sendAsyncUri(PURGE_QUEUE, message);
    }

    public String[] getPluginPurgeInfo() throws EdexException {
        try {
            List<String> retVal = new ArrayList<String>();
            List<String> plugins = getAvailablePlugins();

            for (int i = 0; i < plugins.size(); i++) {
                List<PurgeRule> rules = PluginDao.getPurgeRulesForPlugin(plugins
                        .get(i));
                if (rules.isEmpty()) {
                    retVal.add(plugins.get(i));
                    retVal.add("No Rules Specified. Using default.");
                }else{
                    for (PurgeRule rule : rules) {
                        retVal.add(rule.getId().getPluginName());
                        retVal.add(rule.getRuleDescription());
                    }
                }
            }
            return retVal.toArray(new String[] {});
        } catch (Exception e) {
            throw new EdexException("Error getting plugin purge info", e);
        }
    }

    private List<String> getAvailablePlugins() throws PluginException {
        List<String> availablePlugins;
        List<String> validPlugins = new ArrayList<String>();
        try {
            availablePlugins = new PluginVersionDao().getAvailablePlugins();
        } catch (DataAccessLayerException e1) {
            throw new PluginException(e1);
        }

        for (String pluginName : availablePlugins) {
            if (PluginRegistry.getInstance().getRegisteredObject(pluginName) != null) {
                validPlugins.add(pluginName);
            }
        }
        return validPlugins;
    }

    public void reloadRules() throws EdexException {

    }
}
