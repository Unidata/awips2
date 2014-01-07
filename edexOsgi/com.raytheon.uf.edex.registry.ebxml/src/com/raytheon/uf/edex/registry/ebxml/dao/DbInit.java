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
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.xml.bind.JAXBException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.apache.commons.beanutils.PropertyUtils;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.jdbc.Work;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlJaxbManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.ReflectionUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;

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
 * 4/9/2013     1802        bphillip    Changed submitObjects method call from submitObjectsInternal
 * Apr 15, 2013 1693        djohnson    Use a strategy to verify the database is up to date.
 * Apr 30, 2013 1960        djohnson    Extend the generalized DbInit.
 * 5/21/2013    2022        bphillip    Using TransactionTemplate for database initialization
 * May 29, 2013 1650        djohnson    Reference LifecycleManager as interface type.
 * Jun 24, 2013 2106        djohnson    Invoke registry initialized listeners in their own transaction so 
 *                                      they can't fail the ebxml schema creation/population.
 * Nov 01, 2013 2361        njensen     Use EbxmlJaxbManager instead of SerializationUtil
 * Nov 14, 2013 2552        bkowal      EbxmlJaxbManager is now accessed via getInstance
 * Dec 20, 2013 2636        mpduff      Set initialized to true before postInitialized is called.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class DbInit extends com.raytheon.uf.edex.database.init.DbInit implements
        ApplicationListener<ContextRefreshedEvent>, ApplicationContextAware {

    @VisibleForTesting
    static volatile boolean INITIALIZED = false;

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbInit.class);

    /** Query to check which tables exist in the ebxml database */
    private static final String TABLE_CHECK_QUERY = "SELECT tablename FROM pg_tables where schemaname = 'ebxml';";

    /** The lifecycle manager instance */
    private LifecycleManager lcm;

    /** Hibernate session factory */
    private SessionFactory sessionFactory;

    private ApplicationContext applicationContext;

    /**
     * Creates a new instance of DbInit. This constructor should only be called
     * once when loaded by the Spring container.
     */
    public DbInit() {
        super("ebxml registry");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void executeAdditionalSql() throws Exception {
        super.executeAdditionalSql();

        executeRegistrySql();

        populateDB();
    }

    public static boolean isDbInitialized() {
        return INITIALIZED;
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
    protected void populateDB() throws SerializationException,
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
            try {
                obj = EbxmlJaxbManager
                        .getInstance()
                        .getJaxbManager()
                        .unmarshalFromXmlFile(SubmitObjectsRequest.class,
                                fileList[i]);
            } catch (JAXBException e) {
                throw new SerializationException(
                        "Error unmarshalling from file: "
                                + fileList[i].getPath(), e);
            }

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

                    executeWork(new Work() {
                        @Override
                        public void execute(Connection connection)
                                throws SQLException {
                            Statement stmt = connection.createStatement();
                            stmt.execute(buffer.toString());
                            connection.commit();
                        }
                    });
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

    /**
     * {@inheritDoc}
     */
    @Override
    protected AnnotationConfiguration getAnnotationConfiguration() {
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
        return aConfig;
    }

    @Override
    @Transactional
    public void onApplicationEvent(ContextRefreshedEvent event) {
        if (!INITIALIZED) {
            // Must reference this bean through the proxy to get proper
            // transactional semantics, which requires going through the
            // application context
            final DbInit myself = applicationContext.getBean(DbInit.class);
            try {
                myself.initDb();
            } catch (Exception e) {
                statusHandler.fatal("Error initializing EBXML database!", e);
            }

            INITIALIZED = true;

            myself.postInitDb();
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void postInitDb() {
        statusHandler.info("Executing post initialization actions");

        try {
            Map<String, RegistryInitializedListener> beans = EDEXUtil
                    .getSpringContext().getBeansOfType(
                            RegistryInitializedListener.class);
            for (RegistryInitializedListener listener : beans.values()) {
                listener.executeAfterRegistryInit();
            }
        } catch (Throwable t) {
            throw new RuntimeException("Error initializing EBXML database!", t);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String getTableCheckQuery() {
        return TABLE_CHECK_QUERY;
    }

    /**
     * @param lcm
     *            the lcm to set
     */
    public void setLcm(LifecycleManager lcm) {
        this.lcm = lcm;
    }

    /**
     * @param sessionFactory
     *            the sessionFactory to set
     */
    public void setSessionFactory(SessionFactory sessionFactory) {
        this.sessionFactory = sessionFactory;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setApplicationContext(ApplicationContext applicationContext)
            throws BeansException {
        this.applicationContext = applicationContext;
    }
}
