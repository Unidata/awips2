package ohd.hseb.db;

import java.sql.*;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2000             R. Erb     Original Creation
 * Aug 06, 2003             C. Gobs    Class Overhaul
 * May 13, 2011  8079       jnjanga    added method newStmtForScrollableRset()
 * 
 * </pre>
 * 
 * @author R. Erb
 */

public class Database {
    // *****************************************************************************
    // Private data
    // *****************************************************************************

    // the jdbc url is passed in through the connectTo method
    private String _urlConnectionString = null;

    // define data types to be used to connect to the database
    // and store the SQL statement
    private Connection _conn = null;

    private Statement _stmt = null;

    private String _defaultDriverClassName = "org.postgresql.Driver";

    private String _driverClassName = _defaultDriverClassName;

    // private String[] _driverNameArray = {"org.postgresql.Driver",
    // "com.informix.jdbc.IfxDriver" };
    private String[] _driverNameArray = { "org.postgresql.Driver" };

    private DbType _dbType = DbType.PostgreSQL;

    // private DbType _dbType = DbType.Informix;

    // *****************************************************************************
    // IhfsDatabase() - No argument constructor to initialize data
    // *****************************************************************************
    public Database() {

    }

    // *****************************************************************************

    public Database(String connectionString) {
        connect(connectionString);
    }

    // *****************************************************************************

    public void setDriverClassName(String driverClassName) {
        _driverClassName = driverClassName;

        if (_driverClassName.indexOf("informix") > 0) {
            setDbType(DbType.Informix);
        } else if (_driverClassName.indexOf("postgresql") > 0) {
            setDbType(DbType.PostgreSQL);
        }
        return;
    }

    // *****************************************************************************

    public String getDriverClassName() {
        return _driverClassName;
    }

    // *****************************************************************************

    public void setDbType(DbType dbType) {
        _dbType = dbType;
    }

    // *****************************************************************************

    public DbType getDbType() {
        return _dbType;
    }

    // ---------------------------------------------------------------------------------

    private void loadDbDriver(String driverClassName) {
        // Load the JDBC driver
        try {
            Class.forName(driverClassName);
        } catch (Exception e) {
            e.printStackTrace(System.err);
            System.err.println("ERROR: failed to load JDBC driver named "
                    + driverClassName + "\n Shutting down.");
            System.exit(1);
        }

    }

    // ---------------------------------------------------------------------------------

    // ---------------------------------------------------------------------------------

    public void connect(String connectionString) {
        _urlConnectionString = connectionString;

        loadDbDriver(getDriverClassName());

        // Establish a connection to the database and create a statement
        try {
            _conn = DriverManager.getConnection(_urlConnectionString);
            _stmt = _conn.createStatement();
        } catch (SQLException e) {
            e.printStackTrace();
            System.err.println("ERROR: failed to connect!");
            System.err.println("ERROR: " + e.getMessage());
            // chip change
            System.err.println("ERROR: error code = " + e.getErrorCode());
            System.err.println("ERROR: SQL STATE = " + e.getSQLState());

            System.exit(1);
        }

    } // end of method connectTo

    // *****************************************************************************

    public void connectWithDriverSearch(String connectionString) {
        _urlConnectionString = connectionString;

        try {
            connectWithDriverAttempts(0);
        } catch (SQLException e) {
            e.printStackTrace();
            System.err.println("ERROR: failed to connect!");
            System.err.println("ERROR: " + e.getMessage());
            System.err.println("ERROR: error code = " + e.getErrorCode());
            System.err.println("ERROR: SQL STATE = " + e.getSQLState());

            System.exit(1);
        }

        return;

    }

    // *****************************************************************************

    private void connectWithDriverAttempts(int attemptCount)
            throws SQLException {

        try {
            String driverClassName = _driverNameArray[attemptCount];
            setDriverClassName(driverClassName);
            loadDbDriver(driverClassName);

            // System.out.println("attempting to load driver " +
            // driverClassName);
            attemptCount++; // this must occur after the use of attempCount
                            // above

            _conn = DriverManager.getConnection(_urlConnectionString);
            _stmt = _conn.createStatement();
        } catch (SQLException e) {
            if (attemptCount < _driverNameArray.length) {
                connectWithDriverAttempts(attemptCount);
            } else {
                throw e;
            }
        }

        return;
    }

    // *****************************************************************************
    // getConnection()
    // *****************************************************************************
    public Connection getConnection() {

        return _conn;
    }

    // *****************************************************************************
    // getStatement()
    // *****************************************************************************
    public Statement getStatement() {
        return _stmt;
    }

    public Statement newStmtForScrollableRset() throws SQLException {
        return getConnection().createStatement(
                ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_UPDATABLE);
    }

    // *****************************************************************************
    // getDatabaseName()
    // *****************************************************************************
    public String getDatabaseName() {
        int lastSlashIndex = _urlConnectionString.lastIndexOf("/");

        String dbName = "Unknown";

        if (lastSlashIndex > -1) {
            String slashSubString = _urlConnectionString
                    .substring(lastSlashIndex);
            int colonIndex = slashSubString.indexOf(":");

            if (colonIndex > -1) {
                dbName = slashSubString.substring(1, colonIndex);
            } else {
                int qIndex = slashSubString.indexOf("?");
                if (qIndex > -1) {
                    dbName = slashSubString.substring(1, qIndex);
                }
            }

        }

        return dbName;

    } // end of method getDatabaseName

    // *****************************************************************************
    // disconnectFrom()
    // *****************************************************************************
    public void disconnect() {
        // Close the statement and connection to the database
        try {
            _stmt.close();
            _conn.close();

        } catch (SQLException e) {
            System.err.println("ERROR: failed to close the connection!");
            System.err.println("ERROR: " + e.getMessage());
            System.err.println("ERROR: error code = " + e.getErrorCode());
            e.printStackTrace();
            System.err.println("Shutting Down.");
            System.exit(1);
        } catch (NullPointerException e) {
            System.err.println("ERROR: " + e.getMessage());
            e.printStackTrace();
        }
    } // end of method disconnect

} // end of class Database