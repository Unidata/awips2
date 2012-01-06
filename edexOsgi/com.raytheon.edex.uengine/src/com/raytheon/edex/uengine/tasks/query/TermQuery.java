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

import java.util.List;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * TermQuery task derived from original uEngine TermQueryIndex task. Queries the
 * data store based on the parameters. If the script using TermQuery is a
 * subscription script, it should use the constructor that takes the hibernate
 * id. This way, it will only retrieve the one new piece of data that originally
 * triggered the script to run.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Mar 27, 2007                     njensen             Initial Creation.
 * 9/21/2007        368             grichard            Added plugin operations from superclass.
 * 6/04/2008        875             bphillip            Refactored to use DatabaseQuery
 * </PRE>
 * 
 * @author njensen
 */
public class TermQuery extends TableQuery {

    protected String plugin;

    /**
     * Constructor. This is a placeholder until scripts can be changed
     * 
     * @param aPlugin
     *            The plugin name
     * @param dummy1
     * @param dummy2
     * @throws DataAccessLayerException
     * @throws PluginException
     * @deprecated
     */
    public TermQuery(String aPlugin, String dummy1, String dummy2)
            throws DataAccessLayerException, PluginException {
        this(aPlugin);
    }

    /**
     * Constructor
     * 
     * @param aPlugin
     *            the plugin to query for
     * @throws PluginException
     */
    public TermQuery(String aPlugin) throws DataAccessLayerException,
            PluginException {
        super(PluginFactory.getInstance().getDatabase(aPlugin), PluginFactory
                .getInstance().getPluginRecordClass(aPlugin).getName());
        this.plugin = aPlugin;
    }

    @SuppressWarnings("unchecked")
    public List<PluginDataObject> execute() throws Exception {
        List<PluginDataObject> results = (List<PluginDataObject>) super
                .execute();

        if (results != null) {
            for (int i = 0; i < results.size(); i++) {
                results.get(i).setPluginName(plugin);
            }
        }
        return results;
    }

    /**
     * 
     * @return
     * @throws MicroEngineException
     */
    public String getMatchURI() throws MicroEngineException {

        // Map<String, String> terms = new HashMap<String, String>();
        // for (int i = 0; i < query.getParameters().size(); i++) {
        // terms.put(query.getParameters().get(i).getField(), query
        // .getParameters().get(i).getValue().toString());
        // }
        // try {
        // return Plugin.getMatchURI(plugin, terms);
        // } catch (Exception e) {
        // throw new MicroEngineException(
        // "Error creating match for data URI.", e);
        // }
        return "";
    }

    /**
     * @return the plugin
     */
    public String getPlugin() {
        return plugin;
    }

    /**
     * @param aPlugin
     *            the plugin to set
     */
    public void setPlugin(String aPlugin) {
        this.plugin = aPlugin;
    }
}
