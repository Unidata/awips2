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

import java.io.File;
import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.apache.commons.beanutils.PropertyUtils;
import org.hibernate.Session;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.impl.SessionFactoryImpl;
import org.hibernate.jdbc.Work;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.ReflectionUtil;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.LifecycleManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.util.RegistrySessionManager;

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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class DbInit extends RegistryDao {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbInit.class);

    /** Query to check which tables exist in the ebxml database */
    private static final String TABLE_CHECK_QUERY = "SELECT tablename FROM pg_tables where schemaname = 'public';";

    /** Constant used for table regeneration */
    private static final String DROP_TABLE = "drop table ";

    /** Constant used for table regeneration */
    private static final String DROP_SEQUENCE = "drop sequence ";

    /** Constant used for table regeneration */
    private static final String IF_EXISTS = "if exists ";

    /** Constant used for table regeneration */
    private static final String CASCADE = " cascade ";

    private LifecycleManagerImpl lcm;

    /**
     * Creates a new instance of DbInit. This constructor should only be called
     * once when loaded by the Spring container.
     * 
     * @throws EbxmlRegistryException
     *             If errors occur while regenerating the database tables
     */
    public DbInit(LifecycleManagerImpl lcm) throws EbxmlRegistryException {
        super(null);
        this.lcm = lcm;
        // try {
        initDb();
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
    private void initDb() throws EbxmlRegistryException {
        /*
         * Create a new configuration object which holds all the classes that
         * this Hibernate SessionFactory is aware of
         */
        AnnotationConfiguration aConfig = new AnnotationConfiguration();
        for (Object obj : this.getSessionFactory().getAllClassMetadata()
                .keySet()) {
            try {
                Class<?> clazz = Class.forName((String) obj);
                aConfig.addAnnotatedClass(clazz);
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

        this.doInTransaction(new RegistryTransactionCallback() {

            @Override
            public Object execute(Session session)
                    throws EbxmlRegistryException {
                session.doWork(new Work() {
                    @Override
                    public void execute(Connection connection)
                            throws SQLException {
                        final String[] dropSqls = aConfig
                                .generateDropSchemaScript(((SessionFactoryImpl) getSessionFactory())
                                        .getDialect());
                        Statement stmt = connection.createStatement();
                        for (String sql : dropSqls) {
                            if (sql.startsWith(DROP_TABLE)) {
                                // Modify the drop string to add the 'if exists'
                                // and
                                // 'cascade' clauses to avoid any errors if the
                                // tables
                                // do not exist already
                                sql = sql.replace(DROP_TABLE, DROP_TABLE
                                        + IF_EXISTS);
                                sql += CASCADE;
                                stmt.execute(sql);
                                connection.commit();
                            } else if (sql.startsWith(DROP_SEQUENCE)) {
                                // Modify the drop string to add the 'if exists'
                                // and
                                // 'cascade' clauses to avoid any errors if the
                                // tables
                                // do not exist already
                                sql = sql.replace(DROP_SEQUENCE, DROP_SEQUENCE
                                        + IF_EXISTS);
                                sql += CASCADE;
                                stmt.execute(sql);
                                connection.commit();
                            }

                        }
                    }
                });
                return null;
            }
        });

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

        this.doInTransaction(new RegistryTransactionCallback() {

            @Override
            public Object execute(Session session)
                    throws EbxmlRegistryException {
                final String[] createSqls = aConfig
                        .generateSchemaCreationScript(((SessionFactoryImpl) getSessionFactory())
                                .getDialect());
                session.doWork(new Work() {
                    @Override
                    public void execute(Connection connection)
                            throws SQLException {
                        Statement stmt = connection.createStatement();
                        for (String sql : createSqls) {
                            stmt.execute(sql);
                            connection.commit();
                        }

                    }
                });
                return null;
            }
        });

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
        statusHandler.info("Verifying RegRep database...");
        final List<String> existingTables = new ArrayList<String>();
        List<String> definedTables = new ArrayList<String>();
        this.doInTransaction(new RegistryTransactionCallback() {
            @Override
            public Object execute(Session session)
                    throws EbxmlRegistryException {
                session.doWork(new Work() {
                    @Override
                    public void execute(Connection connection)
                            throws SQLException {
                        Statement stmt = connection.createStatement();
                        ResultSet results = stmt
                                .executeQuery(TABLE_CHECK_QUERY);
                        while (results.next()) {
                            existingTables.add(results.getString(1));
                        }
                    }
                });
                return null;
            }
        });

        final String[] dropSqls = aConfig
                .generateDropSchemaScript(((SessionFactoryImpl) getSessionFactory())
                        .getDialect());
        for (String sql : dropSqls) {
            if (sql.startsWith(DROP_TABLE)) {
                // Drop the table names to all lower case since this is the form
                // the database expects
                definedTables.add(sql.replace(DROP_TABLE, "").toLowerCase());
            }
        }

        // Check if the table set defined by Hibernate matches the table set
        // defined in the database already
        if (existingTables.size() <= definedTables.size()
                && !existingTables.containsAll(definedTables)) {
            return false;
        }
        return true;
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

            SubmitObjectsRequest obj = null;
            obj = (SubmitObjectsRequest) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(fileList[i]);

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

            // try {
            RegistrySessionManager.openSession();
            try {
                lcm.submitObjectsInternal(obj);
            } finally {
                RegistrySessionManager.closeSession();
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

}
