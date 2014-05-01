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

package com.raytheon.edex.plugin;

import com.raytheon.uf.edex.database.plugin.IPluginInitializer;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Default implementation of the IPluginInitializer interface. This class may be
 * used for plugin initialization if no other things need to be initialized
 * besides the database schema
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/06/09      1990       bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class DefaultPluginInitializer implements IPluginInitializer {

    /** The name of the owning plugin */
    protected String pluginName;

    /**
     * Creates a new Initializer implementation for the specified plugin
     * 
     * @param pluginName
     *            The name of the owning plugin
     */
    public DefaultPluginInitializer(String pluginName) {
        this.pluginName = pluginName;
    }

    @Override
    public void initializePlugin() throws Exception {
        PluginDao dao = PluginFactory.getInstance().getPluginDao(pluginName);
        dao.initializePlugin();
    }
}
