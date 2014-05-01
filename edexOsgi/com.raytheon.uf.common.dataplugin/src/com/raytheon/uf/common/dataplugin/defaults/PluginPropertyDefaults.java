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
package com.raytheon.uf.common.dataplugin.defaults;

import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;

/**
 * Defaults for the data plugins. Setters should only be called once through
 * Spring XML configuration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PluginPropertyDefaults {

    protected static String database;

    protected static Class<?> initializer;

    protected static Class<?> dao;

    protected static int initialRetentionTime;

    protected static IHDFFilePathProvider pathProvider;

    /**
     * @return the database
     */
    public static String getDatabase() {
        return database;
    }

    /**
     * @param database
     *            the database to set
     */
    public void setDatabase(String database) {
        PluginPropertyDefaults.database = database;
    }

    /**
     * @return the initialRetentionTime
     */
    public static int getInitialRetentionTime() {
        return initialRetentionTime;
    }

    /**
     * @param initialRetentionTime
     *            the initialRetentionTime to set
     */
    public void setInitialRetentionTime(int initialRetentionTime) {
        PluginPropertyDefaults.initialRetentionTime = initialRetentionTime;
    }

    /**
     * @return the initializer
     */
    public static Class<?> getInitializer() {
        return initializer;
    }

    /**
     * @param initializer
     *            the initializer to set
     */
    public void setInitializer(Class<?> initializer) {
        PluginPropertyDefaults.initializer = initializer;
    }

    /**
     * @return the dao
     */
    public static Class<?> getDao() {
        return dao;
    }

    /**
     * @param dao
     *            the dao to set
     */
    public void setDao(Class<?> dao) {
        PluginPropertyDefaults.dao = dao;
    }

    public static IHDFFilePathProvider getPathProvider() {
        return pathProvider;
    }

    public void setPathProvider(IHDFFilePathProvider pathProvider) {
        PluginPropertyDefaults.pathProvider = pathProvider;
    }
}
