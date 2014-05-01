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
package com.raytheon.uf.edex.database;

import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Properties of a database plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2010 #5050      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class DatabasePluginProperties {

    protected String pluginName;

    protected String pluginFQN;

    protected String database;

    protected List<String> dependencyFQNs;

    protected int initialRetentionTime;

    protected boolean forceCheck = false;

    protected String tableName;

    /**
     * Constructor that initializes values to default values
     */
    public DatabasePluginProperties() {
    }

    public DatabasePluginProperties(PluginProperties props)
            throws PluginException {
        pluginName = props.getPluginName();
        pluginFQN = props.getPluginFQN();
        database = props.getDatabase();
        dependencyFQNs = props.getDependencyFQNs();
        forceCheck = false;
        initialRetentionTime = props.getInitialRetentionTime();
        tableName = PluginFactory.getInstance().getPrimaryTable(
                props.getPluginName());
    }

    public String getPluginName() {
        String rval = pluginName;
        if (rval == null) {
            rval = pluginFQN;
        }

        return rval;
    }

    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    public String getPluginFQN() {
        return pluginFQN;
    }

    public void setPluginFQN(String pluginFQN) {
        this.pluginFQN = pluginFQN;
    }

    public String getDatabase() {
        return database;
    }

    public void setDatabase(String database) {
        this.database = database;
    }

    public List<String> getDependencyFQNs() {
        return dependencyFQNs;
    }

    public void setDependencyFQNs(List<String> dependencyFQNs) {
        this.dependencyFQNs = dependencyFQNs;
    }

    public boolean isForceCheck() {
        return forceCheck;
    }

    public void setForceCheck(boolean forceCheck) {
        this.forceCheck = forceCheck;
    }

    public int getInitialRetentionTime() {
        return initialRetentionTime;
    }

    public void setInitialRetentionTime(int initialRetentionTime) {
        this.initialRetentionTime = initialRetentionTime;
    }

    public String getTableName() {
        return tableName;
    }

    public void setTableName(String tableName) {
        this.tableName = tableName;
    }
}
