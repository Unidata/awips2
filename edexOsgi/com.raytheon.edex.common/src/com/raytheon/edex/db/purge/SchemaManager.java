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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.AnnotationException;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializableManager;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.database.DatabasePluginProperties;
import com.raytheon.uf.edex.database.DatabasePluginRegistry;
import com.raytheon.uf.edex.database.DatabaseSessionFactoryBean;
import com.raytheon.uf.edex.database.IDatabasePluginRegistryChanged;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.plugin.PluginVersion;
import com.raytheon.uf.edex.database.plugin.PluginVersionDao;

/**
 * Manages the ddl statements used to generate the database tables
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/8/2008    1532        bphillip    Initial checkin
 * 2/9/2009     1990       bphillip     Fixed index creation
 * 03/20/09                njensen      Implemented IPluginRegistryChanged
 * Mar 29, 2013 1841       djohnson     Remove unused method, warnings, and close streams with utility method.
 * Mar 02, 2013 1970       bgonzale    Added check for abstract entities in sql index naming.
 *                                     Removed unused private method populateSchema.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SchemaManager implements IDatabasePluginRegistryChanged {

    /** The logger */
    private static final Log logger = LogFactory
            .getLog(SchemaManager.class);

    private static final String resourceSelect = "select relname from pg_class where relname = '";

    /**
     * Plugin lock time out override, 2 minutes
     */
    private static final long pluginLockTimeOutMillis = 120000;

    private static final String TABLE = "%TABLE%";

    /** The singleton instance */
    private static SchemaManager instance;

    /** The directory which the plugins reside */
    private final String pluginDir;

    private final DatabasePluginRegistry dbPluginRegistry;

    private final Map<String, ArrayList<String>> pluginCreateSql = new HashMap<String, ArrayList<String>>();

    private final Map<String, ArrayList<String>> pluginDropSql = new HashMap<String, ArrayList<String>>();

    private final Pattern createResourceNamePattern = Pattern
            .compile("^create (?:table |index |sequence )(?:[A-Za-z_0-9]*\\.)?(.+?)(?: .*)?$");

    private final Pattern createIndexTableNamePattern = Pattern
            .compile("^create index %TABLE%.+? on (.+?) .*$");

    /**
     * Gets the singleton instance
     * 
     * @return The singleton instance
     */
    public static synchronized SchemaManager getInstance() {
        if (instance == null) {
            instance = new SchemaManager();
        }
        return instance;
    }

    /**
     * Creates a new SchemaManager instance<br>
     * This constructor creates a temporary file and exports the ddl statements
     * into this file. These statements are subsequently read back in and
     * assigned to the correct PluginSchema container object based on the plugin
     */
    private SchemaManager() {
        dbPluginRegistry = DatabasePluginRegistry.getInstance();
        pluginDir = PropertiesFactory.getInstance().getEnvProperties()
                .getEnvValue("PLUGINDIR");
    }

    /**
     * Runs all scripts for a particular plugin
     * 
     * @param pluginName
     *            The plugin to run the scripts for
     * @throws PluginException
     *             If errors occur accessing the database
     */
    public void runPluginScripts(DatabasePluginProperties props)
            throws PluginException {
        JarFile jar = null;
        String pluginFQN = props.getPluginFQN();

        try {
            jar = new JarFile(pluginDir + pluginFQN + ".jar");
        } catch (IOException e) {
            throw new PluginException("Unable to find jar for plugin FQN "
                    + pluginFQN, e);
        }

        Enumeration<JarEntry> entries = jar.entries();
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(props.getDatabase()));

        while (entries.hasMoreElements()) {
            JarEntry entry = entries.nextElement();
            String name = entry.getName();
            if (name.startsWith("res/scripts") && name.endsWith(".sql")) {
                BufferedReader reader = null;
                InputStream stream = null;

                try {
                    stream = jar.getInputStream(entry);
                    reader = new BufferedReader(new InputStreamReader(stream));
                    String line = null;
                    StringBuilder buffer = new StringBuilder();
                    while ((line = reader.readLine()) != null) {
                        buffer.append(line);
                    }
                    dao.runScript(buffer.toString());
                } catch (Exception e) {
                    throw new PluginException(
                            "Unable to execute scripts for plugin FQN "
                                    + pluginFQN);
                } finally {
                    Util.close(reader);
                    Util.close(stream);
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public void pluginAdded(String pluginName) throws PluginException {
        boolean haveLock = false;
        DatabasePluginProperties props = DatabasePluginRegistry.getInstance()
                .getRegisteredObject(pluginName);
        ClusterTask ct = null;

        try {
            String sessFactoryName = "&" + props.getDatabase()
                    + "SessionFactory";
            DatabaseSessionFactoryBean sessFactory = (DatabaseSessionFactoryBean) EDEXUtil
                    .getESBComponent(sessFactoryName);

            // handle plugin versioning
            if (props.isForceCheck()) {
                // use direct dialog to figure out
                int rowsUpdated = exportSchema(props, sessFactory, true);
                if (rowsUpdated > 0) {
                    runPluginScripts(props);
                }
            } else {
                ct = ClusterLockUtils.lock("pluginVersion",
                        props.getPluginFQN(), pluginLockTimeOutMillis, true);
                int failedCount = 0;

                while (!LockState.SUCCESSFUL.equals(ct.getLockState())) {
                    switch (ct.getLockState()) {
                    case FAILED: {
                        failedCount++;
                        if (failedCount > 5) {
                            logger.error("Unabled to grab cluster for plugin versioning plugin: "
                                    + pluginName);
                            return;
                        }
                        break;
                    }
                    case OLD: {
                        // no need to check plugin version
                        return;
                    }
                    }

                    ct = ClusterLockUtils
                            .lock("pluginVersion", props.getPluginFQN(),
                                    pluginLockTimeOutMillis, true);
                }

                haveLock = true;
                PluginVersionDao pvd = new PluginVersionDao();
                Boolean initialized = pvd.isPluginInitialized(props
                        .getPluginName());

                if (initialized == null) {
                    logger.info("Exporting DDL for " + pluginName
                            + " plugin...");
                    exportSchema(props, sessFactory, false);
                    runPluginScripts(props);
                    String database = props.getDatabase();
                    PluginVersion pv = new PluginVersion(props.getPluginName(),
                            true, props.getTableName(), database);
                    pvd.saveOrUpdate(pv);
                    logger.info(pluginName + " plugin initialization complete!");
                } else if (initialized == false) {
                    logger.info("Exporting DDL for " + pluginName
                            + " plugin...");
                    dropSchema(props, sessFactory);
                    exportSchema(props, sessFactory, false);
                    runPluginScripts(props);
                    PluginVersion pv = pvd.getPluginInfo(props.getPluginName());
                    pv.setInitialized(true);
                    pv.setTableName(props.getTableName());
                    pvd.saveOrUpdate(pv);
                    logger.info(pluginName + " plugin initialization complete!");
                }
            }
        } catch (Exception e) {
            logger.error("Error processing hibernate objects for plugin "
                    + pluginName, e);
            throw new PluginException(e);
        } finally {
            if (haveLock) {
                ClusterLockUtils.unlock(ct, false);
            }
        }
    }

    /**
     * 
     * @param props
     * @param sessFactory
     * @param hibernatables
     * @return
     */
    protected List<String> getRawCreateSql(DatabasePluginProperties props,
            DatabaseSessionFactoryBean sessFactory) throws AnnotationException {
        String fqn = props.getPluginFQN();
        ArrayList<String> createSql = pluginCreateSql.get(fqn);
        if (createSql == null) {
            // need the full dependency tree to generate the sql
            Set<Class<ISerializableObject>> hibernatables = new HashSet<Class<ISerializableObject>>();
            getAllRequiredHibernatables(props, hibernatables);
            String[] sqlArray = sessFactory.getCreateSql(hibernatables);
            createSql = new ArrayList<String>(sqlArray.length);
            for (String sql : sqlArray) {
                createSql.add(sql);
            }

            for (int i = 0; i < createSql.size(); i++) {
                String sql = createSql.get(i);
                if (sql.startsWith("create index")) {
                    Matcher matcher = createIndexTableNamePattern.matcher(sql);
                    if (matcher.matches()) {
                        createSql.set(i, StringUtil.replace(sql, TABLE,
                                matcher.group(1)));
                    } else if (sql.contains(TABLE)) {
                        // replace %TABLE% in sql statements with an empty
                        // string
                        createSql.set(i, StringUtil.replace(sql, TABLE, ""));
                    }
                }
            }
            createSql.trimToSize();

            // only truly want the sql for just this plugin
            removeAllDependentCreateSql(props, sessFactory, createSql);

            pluginCreateSql.put(fqn, createSql);
        }
        return createSql;
    }

    /**
     * 
     * @param props
     * @param sessFactory
     * @param hibernatables
     * @return
     */
    protected List<String> getRawDropSql(DatabasePluginProperties props,
            DatabaseSessionFactoryBean sessFactory) throws AnnotationException {
        String fqn = props.getPluginFQN();
        ArrayList<String> dropSql = pluginDropSql.get(fqn);
        if (dropSql == null) {
            // need the full dependency tree to generate the sql
            Set<Class<ISerializableObject>> hibernatables = new HashSet<Class<ISerializableObject>>();
            getAllRequiredHibernatables(props, hibernatables);
            String[] sqlArray = sessFactory.getDropSql(hibernatables);
            dropSql = new ArrayList<String>(sqlArray.length);
            for (String sql : sqlArray) {
                dropSql.add(sql);
            }

            // only truly want the sql for just this plugin
            removeAllDependentDropSql(props, sessFactory, dropSql);

            dropSql.trimToSize();

            pluginDropSql.put(fqn, dropSql);
        }
        return dropSql;
    }

    protected void getAllRequiredHibernatables(DatabasePluginProperties props,
            Set<Class<ISerializableObject>> hibernatables) {
        hibernatables.addAll(SerializableManager.getInstance()
                .getHibernatablesForPluginFQN(props.getPluginFQN()));
        List<String> fqns = props.getDependencyFQNs();
        if (fqns != null && fqns.size() > 0) {
            for (String fqn : fqns) {
                DatabasePluginProperties dProps = dbPluginRegistry
                        .getRegisteredObject(fqn);

                // recurse, may need to add short circuit logic by tracking
                // plugins already processed
                getAllRequiredHibernatables(dProps, hibernatables);
            }
        }
    }

    protected void removeAllDependentCreateSql(DatabasePluginProperties props,
            DatabaseSessionFactoryBean sessFactory, List<String> createSql) {
        List<String> fqns = props.getDependencyFQNs();
        if (fqns != null && fqns.size() > 0) {
            for (String fqn : fqns) {
                DatabasePluginProperties dProps = dbPluginRegistry
                        .getRegisteredObject(fqn);
                createSql.removeAll(getRawCreateSql(dProps, sessFactory));
                // recurse to all dependents
                removeAllDependentCreateSql(dProps, sessFactory, createSql);
            }
        }
    }

    protected void removeAllDependentDropSql(DatabasePluginProperties props,
            DatabaseSessionFactoryBean sessFactory, List<String> dropSql) {
        List<String> fqns = props.getDependencyFQNs();
        if (fqns != null && fqns.size() > 0) {
            for (String fqn : fqns) {
                DatabasePluginProperties dProps = dbPluginRegistry
                        .getRegisteredObject(fqn);
                dropSql.removeAll(getRawDropSql(dProps, sessFactory));
                // recurse to all dependents
                removeAllDependentDropSql(dProps, sessFactory, dropSql);
            }
        }
    }

    protected int exportSchema(DatabasePluginProperties props,
            DatabaseSessionFactoryBean sessFactory, boolean forceResourceCheck)
            throws PluginException {
        List<String> ddls = getRawCreateSql(props, sessFactory);
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(props.getDatabase()));
        int rows = 0;

        for (String sql : ddls) {
            boolean valid = true;
            // sequences should always be checked
            if (forceResourceCheck || sql.startsWith("create sequence ")) {
                valid = false;
                Matcher matcher = createResourceNamePattern.matcher(sql);
                if (matcher.matches() && matcher.groupCount() >= 1) {
                    String resourceName = matcher.group(1).toLowerCase();
                    StringBuilder tmp = new StringBuilder(resourceSelect);
                    tmp.append(resourceName);
                    tmp.append("'");
                    try {
                        Object[] vals = dao.executeSQLQuery(tmp.toString());
                        if (vals.length == 0) {
                            valid = true;
                        }
                    } catch (RuntimeException e) {
                        logger.warn("Error occurred checking if resource ["
                                + resourceName + "] exists", e);
                    }
                } else {
                    logger.warn("Matcher could not find name for create sql ["
                            + sql + "]");
                }
            }
            if (valid) {
                try {
                    dao.executeSQLUpdate(sql);
                    rows++;
                } catch (RuntimeException e) {
                    throw new PluginException(
                            "Error occurred exporting schema, sql [" + sql
                                    + "]", e);
                }
            }
        }

        return rows;
    }

    protected void dropSchema(DatabasePluginProperties props,
            DatabaseSessionFactoryBean sessFactory) throws PluginException {
        List<String> ddls = getRawDropSql(props, sessFactory);
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(props.getDatabase()));

        for (String sql : ddls) {
            boolean valid = true;

            // never drop sequences
            if (sql.startsWith("drop sequence ")) {
                valid = false;
            } else if (sql.startsWith("drop table ")) {
                sql = sql.replace("drop table ", "drop table if exists ");
                sql = sql.replace(";", " cascade;");
            } else if (sql.startsWith("alter table")) {
                // dropping the table drops the index
                valid = false;
            }
            if (valid) {
                try {
                    dao.executeSQLUpdate(sql);
                } catch (RuntimeException e) {
                    throw new PluginException(
                            "Error occurred dropping schema, sql [" + sql + "]",
                            e);
                }
            }
        }
    }
}
