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

package com.raytheon.edex.uengine.tasks.query;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Task for querying a plugin-specific database table. This class is intended to
 * be used when entire records need not be returned, only a subset of columns.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 6/05/08      #875        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 */
public class MetadataCatalogQuery extends CatalogQuery {

    /** The plugin name */
    private String plugin;

    /**
     * Creates a new MetadataCatalogQuery for a specified plugin
     * 
     * @param plugin
     *            The plugin name
     * @throws DataAccessLayerException
     *             If a data access object cannot be constructed
     * @throws PluginException
     */
    public MetadataCatalogQuery(String plugin) throws DataAccessLayerException,
            PluginException {
        super(PluginFactory.getInstance().getDatabase(plugin), PluginFactory
                .getInstance().getPluginRecordClass(plugin).getName());
        try {
            dao = new CoreDao(DaoConfig.forClass(className));
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Unable to instantiate data access object for: "
                            + className + " on database: " + database, e);
        }
        this.plugin = plugin;
    }

    public ResponseMessageCatalog execute() throws Exception {
        ResponseMessageCatalog rmc = super.execute();
        rmc.setDataURI(plugin);
        return rmc;
    }

    public String getPlugin() {
        return plugin;
    }

    public void setPlugin(String plugin) {
        this.plugin = plugin;
    }
}
