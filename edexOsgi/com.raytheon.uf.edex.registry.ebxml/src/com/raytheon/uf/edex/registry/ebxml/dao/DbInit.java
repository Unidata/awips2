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

package com.raytheon.uf.edex.registry.ebxml.dao;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Field;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.apache.commons.beanutils.PropertyUtils;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.impl.SessionFactoryImpl;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.ReflectionUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.registry.acp.xacml.XACMLPolicyAdministrator;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.LifecycleManagerImpl;

/**
 * The DbInit class is responsible for ensuring that the appropriate tables are
 * present in the database registry implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/9/2012     184         bphillip    Initial Coding
 * 3/18/2013    1082        bphillip    Changed to use transactional boundaries and spring injection
 * 4/9/2013     1802       bphillip     Changed submitObjects method call from submitObjectsInternal
 * Apr 15, 2013 1693       djohnson     Use a strategy to verify the database is up to date.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Repository
@Transactional
public class DbInit implements ApplicationListener {

    private static volatile boolean INITIALIZED = false;

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbInit.class);

    /** Constant used for table regeneration */
    private static final String DROP_TABLE = "drop table ";

    /** Constant used for table regeneration */
    private static final String DROP_SEQUENCE = "drop sequence ";

    /** Constant used for table regeneration */
    private static final String IF_EXISTS = "if exists ";

    /** Constant used for table regeneration */
    private static final String CASCADE = " cascade ";

    private LifecycleManagerImpl lcm;

    private SessionFactory sessionFactory;

    private XACMLPolicyAdministrator xacmlAdmin;

    private IEbxmlDatabaseValidationStrategy dbValidationStrategy;

    public DbInit() {
    }

    /**
     * Initializes the RegRep database. This method compares the existing tables
     * in the database to verify that they match the tables that Hibernate is
     * aware of. If the existing tables in the database do not match the tables
     * Hibernate is expecting, the tables are regenerated. During the
     * regeneration process, the minimum database objects are reloaded into the
     * database.
     * 
     * @throws EbxmlRegistryException
     */
    protected void initDb() throws EbxmlRegistryException {
        /*
         * Create a new configuration object which holds all the classes that
         * this Hibernate SessionFactory is aware of
         */
        AnnotationConfiguration aConfig = new AnnotationConfiguration();
        for (Object obj : sessionFactory.getAllClassMetadata().keySet()) {
            try {
                Class<?> clazz = Class.forName((String) obj);
                if (clazz.getName().startsWith("oasis")) {
                    aConfig.addAnnotatedClass(clazz);
                }
            } catch (ClassNotFoundException e) {
                statusHandler.error(
                        "Error initializing RegRep database. Class not found: "
                                + obj, e);
            }
        }

        /*
         * Check to see if the database is valid.
         */
        boolean dbIsValid = true;
        try {
            dbIsValid = isDbValid(aConfig);
        } catch (SQLException e) {
            throw new EbxmlRegistryException("Error checking if db is valid!",
                    e);
        }

        if (dbIsValid) {
            // Database is valid.
            statusHandler.info("RegRep database is up to date!");
        } else {
            // Database is not valid. Drop and regenerate the tables defined by
            // Hibernate
            statusHandler
                    .info("RegRep database is out of sync with defined java classes.  Regenerating default database tables...");
            statusHandler.info("Dropping existing tables...");
            try {
                dropTables(aConfig);
            } catch (SQLException e) {
                throw new EbxmlRegistryException(
                        "An unexpected database error occurred while dropping existing RegRep database tables.",
                        e);
            }
            statusHandler.info("Recreating tables...");
            try {
                createTables(aConfig);
            } catch (SQLException e) {
                throw new EbxmlRegistryException(
                        "An unexpected database error occurred while creating RegRep database tables.",
                        e);
            }

            statusHandler.info("Executing additional registry SQL...");
            try {
                executeRegistrySql();
            } catch (EbxmlRegistryException e) {
                throw new EbxmlRegistryException(
                        "An unexpected database error occurred while executing additional sql on the registry",
                        e);
            }

            try {
                populateDB();
            } catch (SerializationException e) {
                throw new EbxmlRegistryException(
                        "Serialization error populating RegRep database with minDB objects",
                        e);
            } catch (MsgRegistryException e) {
                throw new EbxmlRegistryException(
                        "SubmitObjects encountered an error while populating RegRep database with minDB",
                        e);
            }
            statusHandler
                    .info("RegRep database tables have been successfully regenerated!");
        }

    }

    /**
     * Drops the union set of tables defined by Hibernate and exist in the
     * database.
     * 
     * @param aConfig
     *            The Hibernate annotation configuration holding the metadata
     *            for all Hibernate-aware classes
     * @throws SQLException
     *             If the drop sql strings cannot be executed
     * @throws EbxmlRegistryException
     */
    private void dropTables(final AnnotationConfiguration aConfig)
            throws SQLException, EbxmlRegistryException {

        final String[] dropSqls = aConfig
                .generateDropSchemaScript(((SessionFactoryImpl) sessionFactory)
                        .getDialect());

        for (String sql : dropSqls) {
            if (sql.startsWith(DROP_TABLE)) {
                // Modify the drop string to add the 'if exists'
                // and
                // 'cascade' clauses to avoid any errors if the
                // tables
                // do not exist already
                sql = sql.replace(DROP_TABLE, DROP_TABLE + IF_EXISTS);
                sql += CASCADE;
                sessionFactory.getCurrentSession().createSQLQuery(sql)
                        .executeUpdate();
            } else if (sql.startsWith(DROP_SEQUENCE)) {
                // Modify the drop string to add the 'if exists'
                // and
                // 'cascade' clauses to avoid any errors if the
                // tables
                // do not exist already
                sql = sql.replace(DROP_SEQUENCE, DROP_SEQUENCE + IF_EXISTS);
                sql += CASCADE;
                sessionFactory.getCurrentSession().createSQLQuery(sql)
                        .executeUpdate();
            }

        }
    }

    /**
     * Creates the database tables based on the Class metadata that Hibernate is
     * aware of
     * 
     * @param aConfig
     *            The Hibernate annotation configuration holding the metadata
     *            for all Hibernate-aware classes
     * @throws SQLException
     *             If the drop sql strings cannot be executed
     * @throws EbxmlRegistryException
     */
    private void createTables(final AnnotationConfiguration aConfig)
            throws SQLException, EbxmlRegistryException {

        final String[] createSqls = aConfig
                .generateSchemaCreationScript(((SessionFactoryImpl) sessionFactory)
                        .getDialect());
        for (String createSql : createSqls) {
            sessionFactory.getCurrentSession().createSQLQuery(createSql)
                    .executeUpdate();
        }
    }

    /**
     * Checks to see if the database is valid. The RegRep database is considered
     * to be valid if the set of tables defined by Hibernate contains the set of
     * tables already in existance in the database
     * 
     * @param aConfig
     *            The Hibernate annotation configuration holding the metadata
     *            for all Hibernate-aware classes
     * @return True if the database is valid, else false
     * @throws SQLException
     *             If the drop sql strings cannot be executed
     * @throws EbxmlRegistryException
     */
    private boolean isDbValid(AnnotationConfiguration aConfig)
            throws SQLException, EbxmlRegistryException {
        return dbValidationStrategy.isDbValid(aConfig, sessionFactory);
    }

    /**
     * Populates the RegRep database with the minimum set of objects. The
     * objects are defined in the localization directory.
     * 
     * @throws SerializationException
     *             If the objects cannot be deserialized from the XML files
     * @throws MsgRegistryException
     *             If errors occur during the object submission process
     * @throws EbxmlRegistryException
     */
    private void populateDB() throws SerializationException,
            MsgRegistryException, EbxmlRegistryException {
        LocalizationFile[] files = PathManagerFactory.getPathManager()
                .listStaticFiles("ebxml/minDB", new String[] { ".xml" }, true,
                        true);
        File[] fileList = new File[files.length];
        for (int i = 0; i < fileList.length; i++) {
            fileList[i] = files[i].getFile();
        }
        statusHandler.info("Populating RegRep database from " + fileList.length
                + " files...");

        for (int i = 0; i < fileList.length; i++) {
            statusHandler.info("Populating RegRep database from file: "
                    + fileList[i].getName());

            SubmitObjectsRequest obj = SerializationUtil
                    .jaxbUnmarshalFromXmlFile(SubmitObjectsRequest.class, fileList[i]);

            // Ensure an owner is assigned
            for (RegistryObjectType regObject : obj.getRegistryObjectList()
                    .getRegistryObject()) {
                try {
                    checkOwner(regObject);
                } catch (Exception e) {
                    throw new EbxmlRegistryException(
                            "Error assigning default owner", e);
                }
            }
            lcm.submitObjects(obj);
        }

    }

    /**
     * Executes any additional SQL statements contained in the res/scripts
     * directory of this jar. The purpose of this method is primarily to add
     * additional indices that cannot be automaically be generated by Hibernate
     * 
     * @throws EbxmlRegistryException
     */
    private void executeRegistrySql() throws EbxmlRegistryException {
        JarFile jar = null;

        try {
            jar = new JarFile(PropertiesFactory.getInstance()
                    .getEnvProperties().getEnvValue("PLUGINDIR")
                    + "com.raytheon.uf.edex.registry.ebxml.jar");
        } catch (IOException e) {
            throw new EbxmlRegistryException("Unable to find registry jar!", e);
        }

        Enumeration<JarEntry> entries = jar.entries();
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
                    final StringBuilder buffer = new StringBuilder();
                    while ((line = reader.readLine()) != null) {
                        buffer.append(line);
                    }
                    sessionFactory.getCurrentSession()
                            .createSQLQuery(buffer.toString()).executeUpdate();
                } catch (Exception e) {
                    throw new EbxmlRegistryException(
                            "Unable to execute SQL Scripts for registry", e);
                } finally {
                    if (reader != null) {
                        try {
                            reader.close();
                        } catch (IOException e) {
                            throw new EbxmlRegistryException(
                                    "Unable to close file reader while reading registry SQL files",
                                    e);
                        }
                    }
                    if (stream != null) {
                        try {
                            stream.close();
                        } catch (IOException e) {
                            throw new EbxmlRegistryException(
                                    "Unable to close file input stream while reading registry SQL files",
                                    e);
                        }
                    }
                }
            }
        }
    }

    /**
     * Method used to ensure that all objects added during the registry
     * initialization have an owner assigned to them. If no owner is assigned,
     * the default owner is used
     * 
     * @param obj
     *            The object to check
     * @throws Exception
     *             If errors occur while examining the fields
     */
    private void checkOwner(Object obj) throws Exception {
        // Check to make sure that the object to check is a registry object
        if (obj instanceof RegistryObjectType) {
            RegistryObjectType regObj = (RegistryObjectType) obj;

            // If no owner is assigned, assign the defaul owner
            if (regObj.getOwner() == null) {
                regObj.setOwner(RegistryUtil.DEFAULT_OWNER);
            }

            /*
             * Iterate over all the fields and check each member of any
             * collection members that contain registry objects
             */
            List<Field> fields = ReflectionUtil.getAllFields(regObj.getClass());
            boolean isCollection = false;
            for (Field field : fields) {
                isCollection = Collection.class.isAssignableFrom(field
                        .getType());
                if (isCollection) {
                    @SuppressWarnings("unchecked")
                    Collection<Object> coll = Collection.class
                            .cast(PropertyUtils.getProperty(regObj,
                                    field.getName()));
                    for (Object object : coll) {
                        checkOwner(object);
                    }
                }
            }
        }
    }

    public LifecycleManagerImpl getLcm() {
        return lcm;
    }

    public void setLcm(LifecycleManagerImpl lcm) {
        this.lcm = lcm;
    }

    public void setSessionFactory(SessionFactory sessionFactory) {
        this.sessionFactory = sessionFactory;
    }

    @Override
    @Transactional(propagation = Propagation.REQUIRED)
    public void onApplicationEvent(ApplicationEvent event) {
        if (!INITIALIZED) {
            try {
                initDb();
                xacmlAdmin.loadAccessControlPolicies();
            } catch (EbxmlRegistryException e) {
                statusHandler.fatal("Error initializing EBXML database!", e);
            } catch (MsgRegistryException e) {
                statusHandler.fatal("Error initializing EBXML database!", e);
            }

            statusHandler.info("Executing post initialization actions");
            @SuppressWarnings("unchecked")
            Map<String, RegistryInitializedListener> beans = EDEXUtil
                    .getSpringContext().getBeansOfType(
                            RegistryInitializedListener.class);
            for (RegistryInitializedListener listener : beans.values()) {
                listener.executeAfterRegistryInit();
            }
            INITIALIZED = true;
        }
    }

    public void setXacmlAdmin(XACMLPolicyAdministrator xacmlAdmin) {
        this.xacmlAdmin = xacmlAdmin;
    }

    /**
     * @return the dbValidationStrategy
     */
    public IEbxmlDatabaseValidationStrategy getDbValidationStrategy() {
        return dbValidationStrategy;
    }

    /**
     * @param dbValidationStrategy
     *            the dbValidationStrategy to set
     */
    public void setDbValidationStrategy(
            IEbxmlDatabaseValidationStrategy dbValidationStrategy) {
        this.dbValidationStrategy = dbValidationStrategy;
    }

}
