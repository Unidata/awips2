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

package com.raytheon.uf.edex.database.init;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.dialect.Dialect;
import org.hibernate.jdbc.Work;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;

/**
 * The DbInit class is responsible for ensuring that the appropriate tables are
 * present in the database implementation for the session factory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Apr 30, 2013 1960        djohnson    Extracted and generalized from the registry DbInit.
 * </pre>
 * 
 * @author djohnson
 */
public abstract class DbInit {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbInit.class);

    /** Constant used for table regeneration */
    private static final Pattern DROP_TABLE_PATTERN = Pattern
            .compile("^drop\\stable\\s");

    /** Constant used for table regeneration */
    private static final Pattern DROP_SEQUENCE_PATTERN = Pattern
            .compile("^drop\\ssequence\\s");

    /** Constant used for table regeneration */
    private static final Pattern CASCADE_PATTERN = Pattern
            .compile("\\scascade$");

    /** Constant used for table regeneration */
    private static final String DROP_TABLE = "drop table ";

    /** Constant used for table regeneration */
    private static final String DROP_SEQUENCE = "drop sequence ";

    /** Constant used for table regeneration */
    private static final String IF_EXISTS = "if exists ";

    /** Constant used for table regeneration */
    private static final String DROP_TABLE_IF_EXISTS = DROP_TABLE + IF_EXISTS;

    /** Constant used for table regeneration */
    private static final String DROP_SEQUENCE_IF_EXISTS = DROP_SEQUENCE
            + IF_EXISTS;

    /** Constant used for table regeneration */
    private static final String CASCADE = " cascade";

    /** The logging application db name **/
    private final String application;

    /** The dao for executing database commands **/
    private SessionManagedDao<?, ?> dao;

    /**
     * Constructor.
     * 
     * @param application
     *            the application component the database is used in support of
     */
    protected DbInit(String application) {
        this.application = application;
    }

    /**
     * Initializes the database. This method compares the existing tables in the
     * database to verify that they match the tables that Hibernate is aware of.
     * If the existing tables in the database do not match the tables Hibernate
     * is expecting, the tables are regenerated. During the regeneration
     * process, the minimum database objects are reloaded into the database.
     * 
     * @throws Exception
     *             on error initializing the database
     */
    public final void initDb() throws Exception {
        /*
         * Create a new configuration object which holds all the classes that
         * this Hibernate SessionFactory is aware of
         */
        AnnotationConfiguration aConfig = getAnnotationConfiguration();

        /*
         * Check to see if the database is valid.
         */
        boolean dbIsValid = isDbValid(aConfig);

        if (dbIsValid) {
            // Database is valid.
            statusHandler.info("Database for application [" + application
                    + "] is up to date!");
        } else {
            // Database is not valid. Drop and regenerate the tables defined by
            // Hibernate
            statusHandler
                    .info("Database for application ["
                            + application
                            + "] is out of sync with defined java classes.  Regenerating default database tables...");
            statusHandler.info("Dropping existing tables...");
            dropTables(aConfig);

            statusHandler.info("Recreating tables...");
            createTables(aConfig);

            statusHandler.info("Executing additional SQL...");

            executeAdditionalSql();

            statusHandler.info("Database tables for application ["
                    + application + "] have been successfully regenerated!");
        }
    }

    /**
     * Hook method to execute any additional setup required.
     * 
     * @throws Exception
     *             any exceptions may be thrown
     */
    protected void executeAdditionalSql() throws Exception {
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
            throws SQLException {
        final String[] createSqls = aConfig
                .generateSchemaCreationScript(getDialect());
        final Work work = new Work() {
            @Override
            public void execute(Connection connection) throws SQLException {
                Statement stmt = connection.createStatement();
                for (String sql : createSqls) {
                    stmt.execute(sql);
                    connection.commit();
                }

            }
        };

        executeWork(work);
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
            throws SQLException {
        statusHandler.info("Verifying the database for application ["
                + application + "] against entity classes...");

        final List<String> definedTables = new ArrayList<String>();
        final String[] dropSqls = aConfig
                .generateDropSchemaScript(getDialect());
        for (String sql : dropSqls) {
            Matcher matcher = DROP_TABLE_PATTERN.matcher(sql);
            if (matcher.find()) {
                // Drop the table names to all lower case since this is the form
                // the database expects
                sql = matcher.replaceFirst("").toLowerCase();

                // Replace any trailing cascades
                Matcher cascadeMatcher = CASCADE_PATTERN.matcher(sql);
                if (cascadeMatcher.find()) {
                    sql = cascadeMatcher.replaceFirst("");
                }

                definedTables.add(sql);
            }
        }

        final String schemaPrefix = generateSchemaPrefix(definedTables);
        final List<String> existingTables = new ArrayList<String>();
        final Work work = new Work() {
            @Override
            public void execute(Connection connection) throws SQLException {
                Statement stmt = connection.createStatement();
                ResultSet results = stmt.executeQuery(getTableCheckQuery());
                while (results.next()) {
                    existingTables.add(schemaPrefix + results.getString(1));
                }
            }
        };
        executeWork(work);

        // Check if the table set defined by Hibernate matches the table set
        // defined in the database already
        if (existingTables.size() != definedTables.size()
                || !existingTables.containsAll(definedTables)) {
            return false;
        }
        return true;
    }

    /**
     * Generates a schema prefix if required.
     * 
     * @param definedTables
     *            the defined tables
     * @return
     */
    private String generateSchemaPrefix(List<String> definedTables) {
        if (!definedTables.isEmpty()) {
            final String table = definedTables.iterator().next();
            final int indexOfPeriod = table.indexOf(".");
            if (indexOfPeriod != -1) {
                // Returns the <schema>. if present
                return table.substring(0, indexOfPeriod + 1);
            }
        }
        return "";
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
            throws SQLException {

        final Work work = new Work() {
            @Override
            public void execute(Connection connection) throws SQLException {
                final String[] dropSqls = aConfig
                        .generateDropSchemaScript(getDialect());
                Statement stmt = connection.createStatement();
                for (String sql : dropSqls) {
                    Matcher dropTableMatcher = DROP_TABLE_PATTERN.matcher(sql);
                    if (dropTableMatcher.find()) {
                        executeDropSql(sql, dropTableMatcher,
                                DROP_TABLE_IF_EXISTS, stmt, connection);
                    } else {
                        Matcher dropSequenceMatcher = DROP_SEQUENCE_PATTERN
                                .matcher(sql);
                        if (dropSequenceMatcher.find()) {
                            executeDropSql(sql, dropSequenceMatcher,
                                    DROP_SEQUENCE_IF_EXISTS, stmt, connection);
                        }
                    }
                }
            }
        };

        executeWork(work);
    }

    /**
     * Convenience method to execute drop sql with parameters.
     * 
     * @param sql
     * @param dropTextMatcher
     * @param replacementText
     * @param stmt
     * @param connection
     * @throws SQLException
     */
    private void executeDropSql(String sql, Matcher dropTextMatcher,
            String replacementText, Statement stmt, Connection connection)
            throws SQLException {
        // Modify the drop string to add the 'if exists'
        // and 'cascade' clauses to avoid any errors if
        // the tables do not exist already
        sql = dropTextMatcher.replaceFirst(replacementText);
        if (!sql.endsWith(CASCADE)) {
            sql += CASCADE;
        }
        stmt.execute(sql);
        connection.commit();
    }

    /**
     * Execute the work.
     * 
     * @param work
     *            the work
     */
    protected void executeWork(final Work work) {
        dao.executeWork(work);
    }

    /**
     * Get the dialect.
     * 
     * @return
     */
    protected Dialect getDialect() {
        return dao.getDialect();
    }

    public void setDao(SessionManagedDao<?, ?> dao) {
        this.dao = dao;
    }

    /**
     * Get the query that will return the list of current table names used for
     * this db init.
     * 
     * @return the query
     */
    protected abstract String getTableCheckQuery();

    /**
     * Get the {@link AnnotationConfiguration} to use.
     * 
     * @return
     */
    protected abstract AnnotationConfiguration getAnnotationConfiguration();
}
