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
import com.raytheon.uf.edex.database.purge.PurgeRuleSet;

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

            for (String plugin : plugins) {
                PurgeRuleSet rules = PluginDao.getPurgeRulesForPlugin(plugin);

                List<PurgeRule> defRules = rules.getDefaultRules();
                List<PurgeRule> ruleList = rules.getRules();
                List<String> purgeKeys = rules.getKeys();
                if ((defRules == null)
                        && ((ruleList == null) || ruleList.isEmpty())) {
                    retVal.add(plugin);
                    retVal.add("No Rules Specified. Using default.");
                } else {
                    if (defRules != null) {
                        for (PurgeRule rule : defRules) {
                            retVal.add(plugin);
                            retVal.add(rule.getRuleDescription(purgeKeys));
                        }
                    }
                    if (ruleList != null) {
                        for (PurgeRule rule : ruleList) {
                            retVal.add(plugin);
                            retVal.add(rule.getRuleDescription(purgeKeys));
                        }
                    }
                }
            }
            return retVal.toArray(new String[retVal.size()]);
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
