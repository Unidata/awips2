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

package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.jdbc.Work;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;

/**
 * The DbInit class is responsible for ensuring that the appropriate tables are
 * present in the bandwidth manager database implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Jul 31, 2012 726         jspinks     Copied and refactored from ebxml registry DbInit
 * Oct 26, 2012 1286        djohnson    Renamed to Hibernate specific.
 * </pre>
 * 
 * @author jspinks
 * @version 1
 */
public class HibernateBandwidthDbInit implements IBandwidthDbInit {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(HibernateBandwidthDbInit.class);

    /** Query to check which tables exist in the database */
    private static final String TABLE_CHECK_QUERY = "select tablename from pg_tables where tablename like 'bandwidth_%';";

    /** Constant used for table regeneration */
    private static final String DROP_TABLE = "drop table ";

    /** Constant used for table regeneration */
    private static final String DROP_SEQUENCE = "drop sequence ";

    /** Constant used for table regeneration */
    private static final String IF_EXISTS = "if exists ";

    /** Constant used for table regeneration */
    private static final String CASCADE = " cascade ";

    private final HibernateBandwidthDao bandwidthDao;

    /**
     * Creates a new instance of DbInit. This constructor should only be called
     * once when loaded by the Spring container.
     * 
     * @param bandwidthDao
     *            the dao to use
     * 
     */
    public HibernateBandwidthDbInit(HibernateBandwidthDao bandwidthDao) {
        this.bandwidthDao = bandwidthDao;
    }

    /**
     * Initializes the ebxml database
     */
    @Override
    public void init() {

        statusHandler.info("Starting bandwidth database init...");

        /*
         * Create a new configuration object which holds all the classes that
         * this Hibernate SessionFactory is aware of
         */
        AnnotationConfiguration aConfig = new AnnotationConfiguration();

        // TODO: add package scanning or some other more elegant solution, but
        // for now
        // just to it ugly.
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.DataSetMetaDataDao.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDao.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval.class);
        aConfig.addAnnotatedClass(com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation.class);

        /*
         * Check to see if the database is valid.
         */
        boolean dbIsValid = true;
        try {
            dbIsValid = isDbValid(aConfig);
        } catch (SQLException e) {
            throw new IllegalStateException("Error checking if db is valid!", e);
        }

        if (dbIsValid) {
            // Database is valid.
            statusHandler.info("Bandwidth database is up to date!");
        } else {
            // Database is not valid. Drop and regenerate the tables defined by
            // Hibernate
            statusHandler
                    .info("Bandwidth database is out of sync with defined java classes.  Regenerating default database tables...");
            statusHandler.info("Dropping existing tables...");
            try {
                dropTables(aConfig);
            } catch (SQLException e) {
                throw new IllegalStateException(
                        "An unexpected database error occurred while dropping existing Bandwidth database tables.",
                        e);
            }
            statusHandler.info("Recreating tables...");
            try {
                createTables(aConfig);
            } catch (SQLException e) {
                throw new IllegalStateException(
                        "An unexpected database error occurred while creating Bandwidth database tables.",
                        e);
            }

            statusHandler
                    .info("Bandwidth database tables have been successfully regenerated!");
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
    @Override
    public void dropTables(final AnnotationConfiguration aConfig)
            throws SQLException {

        bandwidthDao.doWork(new Work() {
            @Override
            public void execute(Connection connection) throws SQLException {
                final String[] dropSqls = aConfig
                        .generateDropSchemaScript(bandwidthDao
                                .getDialect());
                Statement stmt = connection.createStatement();
                for (String sql : dropSqls) {
                    if (sql.startsWith(DROP_TABLE)) {
                        // Modify the drop string to add the 'if exists'
                        // and
                        // 'cascade' clauses to avoid any errors if the
                        // tables
                        // do not exist already
                        sql = sql.replace(DROP_TABLE, DROP_TABLE + IF_EXISTS);
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

                } // end for
            } // end execute()
        } // end Work()
                );
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
    @Override
    public void createTables(final AnnotationConfiguration aConfig)
            throws SQLException {

        final String[] createSqls = aConfig
                .generateSchemaCreationScript(bandwidthDao.getDialect());
        bandwidthDao.doWork(new Work() {
            @Override
            public void execute(Connection connection) throws SQLException {
                Statement stmt = connection.createStatement();
                for (String sql : createSqls) {
                    stmt.execute(sql);
                    connection.commit();
                }
            }
        });
    }

    /**
     * Checks to see if the database is valid. The Bandwidth database is
     * considered to be valid if the set of tables defined by Hibernate contains
     * the set of tables already in existance in the database
     * 
     * @param aConfig
     *            The Hibernate annotation configuration holding the metadata
     *            for all Hibernate-aware classes
     * @return True if the database is valid, else false
     * @throws SQLException
     *             If the drop sql strings cannot be executed
     * @throws EbxmlRegistryException
     */
    public boolean isDbValid(AnnotationConfiguration aConfig)
            throws SQLException {
        statusHandler.info("Verifying bandwidth database...");
        final List<String> existingTables = new ArrayList<String>();
        List<String> definedTables = new ArrayList<String>();

        bandwidthDao.doWork(new Work() {
            @Override
            public void execute(Connection connection) throws SQLException {
                Statement stmt = connection.createStatement();
                ResultSet results = stmt.executeQuery(TABLE_CHECK_QUERY);
                while (results.next()) {
                    existingTables.add(results.getString(1));
                }
            }
        });

        final String[] dropSqls = aConfig
.generateDropSchemaScript(bandwidthDao
                .getDialect());
        for (String sql : dropSqls) {
            if (sql.startsWith(DROP_TABLE)) {
                // Drop the table names to all lower case since this is the form
                // the database expects
                definedTables.add(sql.replace(DROP_TABLE, "").toLowerCase());
            }
        }

        for (String t : existingTables) {
            statusHandler.info("Existing table [" + t + "]");
        }
        for (String t : definedTables) {
            statusHandler.info("Defined table [" + t + "]");
        }

        // Check if the table set defined by Hibernate matches the table set
        // defined in the database already
        if (existingTables.size() <= definedTables.size()
                && !existingTables.containsAll(definedTables)) {
            return false;
        }
        return true;
    }
}
