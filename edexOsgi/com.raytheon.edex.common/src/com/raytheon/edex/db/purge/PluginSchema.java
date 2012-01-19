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

package com.raytheon.edex.db.purge;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Container object for ddl statements
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/8/2008    1532        bphillip    Initial checkin
 * 2/6/2009     1990       bphillip     Modified error handling
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class PluginSchema {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The database name for which the ddl statements contained herein apply */
    private String dbName;

    /** The ddl statements for dropping tables defined in this container */
    private List<String> dropSqls;

    /** The ddl statements for creating tables defined in this container */
    private List<String> createSqls;

    /**
     * Creates a new PluginSchema object for a particular plugin
     * 
     * @param pluginName
     *            The plugin name
     * @throws PluginException
     */
    public PluginSchema(String pluginName) throws PluginException {
        dropSqls = new ArrayList<String>();
        createSqls = new ArrayList<String>();
        dbName = PluginFactory.getInstance().getDatabase(pluginName);

    }

    /**
     * Creates a default pluginSchema container using the default database name
     */
    public PluginSchema() {
        dropSqls = new ArrayList<String>();
        createSqls = new ArrayList<String>();
        this.dbName = DaoConfig.DEFAULT_DB_NAME;
    }

    /**
     * Adds a drop ddl statement
     * 
     * @param sql
     *            The drop ddl statement
     */
    public void addDropSql(String sql) {
        if (!dropSqls.contains(sql)) {
            dropSqls.add(sql);
        }

    }

    /**
     * Adds a create ddl statement
     * 
     * @param sql
     *            The create ddl statement
     */
    public void addCreateSql(String sql) {
        if (!createSqls.contains(sql)) {
            createSqls.add(sql);
        }
    }

    /**
     * Export tables contained in the createSqls list to the database
     */
    public void export() {
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(dbName));
        for (String sql : createSqls) {
            try {
                dao.executeSQLUpdate(sql);
            } catch (RuntimeException e) {
                e.printStackTrace();
                // Ignore
            }
        }
    }

    /**
     * Drop tables contained in the dropSqls list from the database
     */
    public void drop() {
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(dbName));
        for (String sql : dropSqls) {
            try {
                dao.executeSQLUpdate(sql);
            } catch (RuntimeException e) {
                // Ignore
            }
        }
    }

}
