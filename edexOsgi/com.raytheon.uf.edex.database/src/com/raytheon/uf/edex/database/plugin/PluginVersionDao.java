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

package com.raytheon.uf.edex.database.plugin;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * The dao implementation associated with the PluginVersion class used for all
 * database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial Check in
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class PluginVersionDao extends CoreDao {

    private static final String DB_INITIALIZED_SQL = "select relname from pg_class where relname = 'plugin_info'";

    /**
     * Creates a new PluginVersionDao.
     */
    public PluginVersionDao() {
        super(DaoConfig.forClass(PluginVersion.class));
    }

    /**
     * Checks if the database has been initialized yet
     * 
     * @return True if database has been initialized, false if not
     */
    public boolean isDbInitialized() {
        return this.executeSQLQuery(DB_INITIALIZED_SQL).length != 0;
    }

    /**
     * Checks if a particular plugin has been initialized yet
     * 
     * @param plugin
     *            The plugin to check
     * @return true if the plugin has been initialized, else false
     * @throws DataAccessLayerException
     */
    public Boolean isPluginInitialized(String plugin)
            throws DataAccessLayerException {

        DatabaseQuery query = new DatabaseQuery(this.daoClass);
        query.addQueryParam("name", plugin);
        query.addReturnedField("initialized");
        List<?> results = queryByCriteria(query);
        if (results.isEmpty()) {
            return null;
        } else {
            return (Boolean) results.get(0);
        }
    }

    public PluginVersion getPluginInfo(String pluginName) {
        return (PluginVersion) this.queryById(pluginName);
    }

    /**
     * Loads all the plugin names currently registered in the plugin_info table
     * 
     * @return The names of the plugins
     * @throws DataAccessLayerException
     *             If problems occur during query
     */
    @SuppressWarnings("unchecked")
    public List<String> loadAllPluginNames() throws DataAccessLayerException {
        List<String> pluginNames = new ArrayList<String>();
        DatabaseQuery query = new DatabaseQuery(daoClass);
        query.addReturnedField("name");
        pluginNames = (List<String>) this.queryByCriteria(query);
        return pluginNames;
    }

    /**
     * Retrieves all plugins available in the system
     * 
     * @return A list of plugin names
     * @throws DataAccessLayerException
     *             If errors occur during query
     */
    @SuppressWarnings("unchecked")
    public List<String> getAvailablePlugins() throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(daoClass);
        query.addDistinctParameter("name");
        List<String> availablePlugins = (List<String>) queryByCriteria(query);
        return availablePlugins;

    }

    /**
     * Deletes a plugin info entry
     * 
     * @param pluginName
     *            The plugin name of the entry to remove
     * @throws DataAccessLayerException
     *             If errors occur during the database operation
     */
    @SuppressWarnings("unchecked")
    public void deletePluginVersionByName(String pluginName)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(daoClass);
        query.addQueryParam("name", pluginName);
        List<PluginVersion> pv = (List<PluginVersion>) queryByCriteria(query);
        if (!pv.isEmpty()) {
            this.delete(pv.get(0));
        }
    }

}
