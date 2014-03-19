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
package com.raytheon.uf.common.dataplugin;

import java.util.List;

import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;

/**
 * Properties of a data plugin that are used by the PluginFactory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2009            njensen     Initial creation.
 * Mar 13, 2014 2726       rjpeter     Moved default values to set on plugin registration.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PluginProperties {

    protected String pluginName;

    protected String database;

    protected Class<?> initializer;

    // specifically want this to remain ? so we don't add a dependency on
    // PluginDao
    protected Class<?> dao;

    protected Class<PluginDataObject> record;

    protected Integer initialRetentionTime;

    protected String pluginFQN;

    protected List<String> dependencyFQNs;

    protected IHDFFilePathProvider pathProvider;

    /**
     * Set the defaults for any fields that haven't already been set to a not
     * null value.
     * 
     * @param defaults
     */
    public void setDefaults(PluginProperties defaults) {
        /*
         * pluginName, pluginFQN, dependencyFQNs, and record have no defaults to
         * inherit
         */

        if (database == null) {
            database = defaults.getDatabase();
        }
        if (initializer == null) {
            initializer = defaults.getInitializer();
        }
        if (dao == null) {
            dao = defaults.getDao();
        }
        if (initialRetentionTime == null) {
            initialRetentionTime = defaults.getInitialRetentionTime();
        }
        if (pathProvider == null) {
            pathProvider = defaults.getPathProvider();
        }
    }

    /**
     * Compression to use on storage, if null, no compression
     */
    protected String compression;

    /**
     * @return the database
     */
    public String getDatabase() {
        return database;
    }

    /**
     * @param database
     *            the database to set
     */
    public void setDatabase(String database) {
        this.database = database;
    }

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * @param pluginName
     *            the pluginName to set
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * @return the initialRetentionTime
     */
    public int getInitialRetentionTime() {
        if (initialRetentionTime != null) {
            return initialRetentionTime;
        }

        return 0;
    }

    /**
     * @param initialRetentionTime
     *            the initialRetentionTime to set
     */
    public void setInitialRetentionTime(int initialRetentionTime) {
        this.initialRetentionTime = initialRetentionTime;
    }

    /**
     * @return the initializer
     */
    public Class<?> getInitializer() {
        return initializer;
    }

    /**
     * @param initializer
     *            the initializer to set
     */
    public void setInitializer(Class<?> initializer) {
        this.initializer = initializer;
    }

    /**
     * @return the dao
     */
    public Class<?> getDao() {
        return dao;
    }

    /**
     * @param dao
     *            the dao to set
     */
    public void setDao(Class<?> dao) {
        this.dao = dao;
    }

    /**
     * @return the record
     */
    public Class<PluginDataObject> getRecord() {
        return record;
    }

    /**
     * @param record
     *            the record to set
     */
    public void setRecord(Class<PluginDataObject> record) {
        this.record = record;
    }

    public String getPluginFQN() {
        return pluginFQN;
    }

    public void setPluginFQN(String pluginFQN) {
        this.pluginFQN = pluginFQN;
    }

    public List<String> getDependencyFQNs() {
        return dependencyFQNs;
    }

    public void setDependencyFQNs(List<String> dependencyFQNs) {
        this.dependencyFQNs = dependencyFQNs;
    }

    public IHDFFilePathProvider getPathProvider() {
        return pathProvider;
    }

    public void setPathProvider(IHDFFilePathProvider pathProvider) {
        this.pathProvider = pathProvider;
    }

    public String getCompression() {
        return compression;
    }

    public void setCompression(String compression) {
        this.compression = compression;
    }
}
