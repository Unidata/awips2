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
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.persistence.metamodel.EntityType;

import org.apache.commons.beanutils.PropertyUtils;
import org.hibernate.SessionFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlJaxbManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.ReflectionUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.registry.acp.xacml.XACMLPolicyAdministrator;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.init.RegistryInitializedListener;
import com.raytheon.uf.edex.registry.ebxml.web.RegistryWebServer;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * The DbInit class is responsible for ensuring that the appropriate tables are
 * present in the database registry implementation.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Feb 09, 2012  184      bphillip     Initial Coding
 * Mar 18, 2013  1082     bphillip     Changed to use transactional boundaries
 *                                     and spring injection
 * Apr 09, 2013  1802     bphillip     Changed submitObjects method call from
 *                                     submitObjectsInternal
 * Apr 15, 2013  1693     djohnson     Use a strategy to verify the database is
 *                                     up to date.
 * Apr 30, 2013  1960     djohnson     Extend the generalized DbInit.
 * May 21, 2013  2022     bphillip     Using TransactionTemplate for database
 *                                     initialization
 * May 29, 2013  1650     djohnson     Reference LifecycleManager as interface
 *                                     type.
 * Jun 24, 2013  2106     djohnson     Invoke registry initialized listeners in
 *                                     their own transaction so they can't fail
 *                                     the ebxml schema creation/population.
 * Nov 01, 2013  2361     njensen      Use EbxmlJaxbManager instead of
 *                                     SerializationUtil
 * Nov 14, 2013  2552     bkowal       EbxmlJaxbManager is now accessed via
 *                                     getInstance
 * Dec 20, 2013  2636     mpduff       Set initialized to true before
 *                                     postInitialized is called.
 * Dec 04, 2013  2584     dhladky      Version based EbxmlJaxbManager
 * Jul 10, 2014  1717     bphillip     Removed xacml policy admin object
 * Jul 10, 2014  2914     garmendariz  Remove EnvProperties
 * Jul 28, 2014  3474     dhladky      Fixed bad ownership settings.
 * Oct 16, 2014  3454     bphillip     Upgrading to Hibernate 4
 * Feb 16, 2017  5899     rjpeter      Updated TABLE_CHECK_QUERY and throw error
 *                                     on failure to initialize.
 * Oct 24, 2017  6166     nabowle      Force RegistryWebServer to initialize first in postDbInit.
 * Sep 24, 2018  7238     skabasele    Store the initial RegistryObjectType Ids in a list
 * Mar 21, 2019  6140     tgurney      Add getDbClasses (Hibernate 5 upgrade)
 * May 14, 2019  6140     tgurney      Replace SessionFactory.getAllClassMetadata() (Hibernate 5)
 *
 * </pre>
 *
 * @author bphillip
 */
public class DbInit extends com.raytheon.uf.edex.database.init.DbInit implements
        ApplicationListener<ContextRefreshedEvent>, ApplicationContextAware {

    @VisibleForTesting
    protected static volatile boolean INITIALIZED = false;

    /** Query to check which tables exist in the ebxml database */
    private static final String TABLE_CHECK_QUERY = "SELECT schemaname || '.' || tablename FROM pg_tables where schemaname = 'ebxml';";

    /** The lifecycle manager instance */
    private LifecycleManager lcm;

    /** Hibernate session factory */
    private SessionFactory sessionFactory;

    private ApplicationContext applicationContext;

    private XACMLPolicyAdministrator xacmlPolicyAdmin;

    /**
     * Set contains the initial object Ids that are automatically created during
     * the initialization of the ebxml schema tables. It will used in the
     * registry synchronization process to determine the the synchronization
     * type to proceed with.
     */
    private static Set<String> initialDbOjectIdsSet = new HashSet<>();

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
        xacmlPolicyAdmin.loadAccessControlPolicies();
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

        logger.info("Populating RegRep database from " + files.length
                + " files...");

        for (ILocalizationFile file : files) {
            logger.info(
                    "Populating RegRep database from file: " + file.getPath());

            SubmitObjectsRequest obj = null;
            try (InputStream is = file.openInputStream()) {
                obj = EbxmlJaxbManager.getInstance().getJaxbManager()
                        .unmarshalFromInputStream(SubmitObjectsRequest.class,
                                is);
            } catch (Exception e) {
                throw new SerializationException(
                        "Error unmarshalling from file: " + file.getPath(), e);
            }

            // Ensure an owner is assigned
            for (RegistryObjectType regObject : obj.getRegistryObjectList()
                    .getRegistryObject()) {
                try {
                    checkOwner(regObject);
                    initialDbOjectIdsSet.add(regObject.getId());
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
        String file = EDEXUtil.getEdexPlugins() + File.separator
                + "com.raytheon.uf.edex.registry.ebxml.jar";
        try (JarFile jar = new JarFile(file)) {
            Enumeration<JarEntry> entries = jar.entries();
            while (entries.hasMoreElements()) {
                JarEntry entry = entries.nextElement();
                String name = entry.getName();
                if (name.startsWith("res/scripts") && name.endsWith(".sql")) {
                    try (InputStream stream = jar.getInputStream(entry);
                            BufferedReader reader = new BufferedReader(
                                    new InputStreamReader(stream))) {
                        String line = null;
                        final StringBuilder buffer = new StringBuilder();
                        while ((line = reader.readLine()) != null) {
                            buffer.append(line);
                        }

                        executeWork(connection -> {
                            try (Statement stmt = connection
                                    .createStatement()) {
                                stmt.execute(buffer.toString());
                                connection.commit();
                            }
                        });
                    } catch (Exception e) {
                        throw new EbxmlRegistryException(
                                "Unable to execute SQL Scripts from [" + name
                                        + "] for registry",
                                e);
                    }
                }
            }
        } catch (IOException e) {
            throw new EbxmlRegistryException(
                    "Error loading registry sql from jar [" + file + "]", e);
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
                regObj.setOwner(RegistryUtil.defaultUser);
            }

            /*
             * Iterate over all the fields and check each member of any
             * collection members that contain registry objects
             */
            List<Field> fields = ReflectionUtil.getAllFields(regObj.getClass());
            boolean isCollection = false;
            for (Field field : fields) {
                isCollection = Collection.class
                        .isAssignableFrom(field.getType());
                if (isCollection) {
                    Collection<Object> coll = Collection.class.cast(
                            PropertyUtils.getProperty(regObj, field.getName()));
                    for (Object object : coll) {
                        checkOwner(object);
                    }
                }
            }
        }
    }

    @Override
    protected Collection<Class<?>> getDbClasses() {
        List<Class<?>> classes = new ArrayList<>();
        for (EntityType<?> entityType : sessionFactory.getMetamodel()
                .getEntities()) {
            Class<?> clazz = entityType.getJavaType();
            if (clazz.getName().startsWith("oasis")) {
                classes.add(clazz);
            }
        }
        return classes;
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
                throw new RuntimeException("Error initializing EBXML database",
                        e);
            }

            INITIALIZED = true;

            myself.postInitDb();
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void postInitDb() {
        logger.info("Executing post initialization actions");

        try {
            Map<String, RegistryInitializedListener> beans = EDEXUtil
                    .getSpringContext()
                    .getBeansOfType(RegistryInitializedListener.class);
            List<RegistryInitializedListener> beanList = new ArrayList<>(
                    beans.values());
            Iterator<RegistryInitializedListener> iter = beanList.iterator();
            /*
             * Execute RegistryWebServer before other
             * RegistryInitializedListeners to ensure the server is up before
             * any local REST calls may be made to it.
             */
            while (iter.hasNext()) {
                RegistryInitializedListener listener = iter.next();
                if (listener instanceof RegistryWebServer) {
                    listener.executeAfterRegistryInit();
                    iter.remove();
                }
            }

            for (RegistryInitializedListener listener : beanList) {
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

    /**
     * @param xacmlPolicyAdmin
     *            the xacmlPolicyAdmin to set
     */
    public void setXacmlPolicyAdmin(XACMLPolicyAdministrator xacmlPolicyAdmin) {
        this.xacmlPolicyAdmin = xacmlPolicyAdmin;
    }

    /**
     * Getter used to retrieve the initialObjectIds created in the database from
     * the localization files when the minimum object are created.
     *
     * @return
     */
    public static Set<String> getInitialDbOjectIdsSet() {
        return initialDbOjectIdsSet;
    }

    /**
     * Add an Id to the set.
     *
     * @param Id
     */
    public static void addToInitialDbOjectIdsSet(String Id) {
        initialDbOjectIdsSet.add(Id);
    }

    /**
     * remove an Id to the set.
     *
     * @param Id
     */
    public static void removeFromInitialDbOjectIdsSet(String Id) {
        initialDbOjectIdsSet.remove(Id);
    }

}
