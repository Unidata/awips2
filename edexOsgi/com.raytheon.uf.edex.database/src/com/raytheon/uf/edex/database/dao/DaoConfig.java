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

package com.raytheon.uf.edex.database.dao;

/**
 * Configuration settings for a data access object.<br>
 * This object contains the required information to correctly instantiate a
 * valid data access object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 12/11/07     600         bphillip    Initial Check in    
 *  
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class DaoConfig {

    /** The default database name */
    public static final String DEFAULT_DB_NAME = "metadata";

    /** The session factory suffix */
    private static final String SESSION_FACTORY = "SessionFactory";

    /** The transaction manager suffix */
    private static final String TX_MANAGER = "TxManager";

    /**
     * The default data access object configuration. This configuration
     * specifies the metadata database
     */
    public static final DaoConfig DEFAULT = DaoConfig
            .forDatabase(DEFAULT_DB_NAME);

    /** The database to connect to */
    private String dbName;

    /** The class for which the desired data access object is to be used for */
    private Class<?> daoClassName;

    /** The name of the Hibernate session factory to use */
    private String sessionFactoryName;

    /** The name of the Hibernate transaction manager to use */
    private String txManagerName;

    /**
     * Default constructor.
     */
    private DaoConfig() {
        this.dbName = DEFAULT_DB_NAME;
        this.sessionFactoryName = DEFAULT_DB_NAME + SESSION_FACTORY;
        this.txManagerName = DEFAULT_DB_NAME + TX_MANAGER;
    }

    /**
     * Constructs a DaoConfig object using the specified class name, default
     * session factory, and the default transaction manager
     * 
     * @param className
     *            The class object
     */
    private DaoConfig(Class<?> className) {
        this.daoClassName = className;
        this.dbName = DEFAULT_DB_NAME;
        this.sessionFactoryName = DEFAULT_DB_NAME + SESSION_FACTORY;
        this.txManagerName = DEFAULT_DB_NAME + TX_MANAGER;
    }

    /**
     * Constructs a DaoConfig object for the specified database using the
     * specified class name. The appropriate session factory and transaction
     * manager will be determined from the database name.
     * 
     * @param dbName
     *            The database name
     * @param className
     *            The class object
     */
    private DaoConfig(String dbName, Class<?> className) {
        this.daoClassName = className;
        this.dbName = dbName;
        this.sessionFactoryName = dbName + SESSION_FACTORY;
        this.txManagerName = dbName + TX_MANAGER;
    }

    /**
     * Constructs a DaoConfig object for the specified database.
     * 
     * @param dbName
     *            The database name
     */
    private DaoConfig(String dbName) {
        this.dbName = dbName;
        this.sessionFactoryName = dbName + SESSION_FACTORY;
        this.txManagerName = dbName + TX_MANAGER;
    }

    /**
     * Gets a DaoConfig object for the specified class using the default session
     * factory and default transaction manager.
     * 
     * @param className
     *            The class for which to create the DaoConfig object
     * @return A DaoConfig instance using the specified class, default session
     *         factory and default transaction manager.
     */
    public static DaoConfig forClass(Class<?> className) {
        return new DaoConfig(className);
    }

    /**
     * Gets a DaoConfig object for the specified class using the default session
     * factory and default transaction manager.
     * 
     * @param className
     *            The class for which to create the DaoConfig object
     * @return A DaoConfig instance using the specified class, default session
     *         factory and default transaction manager.
     * @throws ClassNotFoundException
     *             If the given class name does not exist on the class path
     */
    public static DaoConfig forClass(String className)
            throws ClassNotFoundException {
        return new DaoConfig(DaoConfig.class.getClassLoader().loadClass(
                ((String) className).trim()));
    }

    /**
     * Gets a DaoConfig object for the specified class and database
     * 
     * @param dbName
     *            The database name
     * @param className
     *            The class object
     * @return A DaoConfig instance with the specified database name and class
     *         name
     */
    public static DaoConfig forClass(String dbName, Class<?> className) {
        return new DaoConfig(dbName, className);
    }

    /**
     * Gets a DaoConfig object for the specified class and database
     * 
     * @param dbName
     *            The database name
     * @param className
     *            The class name
     * @return A DaoConfig instance with the specified database name and class
     *         name
     * @throws ClassNotFoundException
     *             If the given class name does not exist on the class path  
     */
    public static DaoConfig forClass(String dbName, String className)
            throws ClassNotFoundException {
        return new DaoConfig(dbName, DaoConfig.class.getClassLoader()
                .loadClass(((String) className).trim()));
    }

    /**
     * Gets a DaoConfig object for the specified database
     * @param dbName The database name
     * @return
     */
    public static DaoConfig forDatabase(String dbName) {
        return new DaoConfig(dbName);
    }

    /**
     * Gets the database name
     * @return The database name
     */
    public String getDbName() {
        return dbName;
    }

    /**
     * Gets the dao class name
     * @return The dao class name
     */
    public Class<?> getDaoClassName() {
        return daoClassName;
    }

    /**
     * Gets the session factory name
     * @return The session factory name
     */
    public String getSessionFactoryName() {
        return sessionFactoryName;
    }

    /**
     * Gets the transaction manager name
     * @return The transaction manager name
     */
    public String getTxManagerName() {
        return txManagerName;
    }

}
