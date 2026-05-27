package com.raytheon.uf.edex.upgrade.gridlocation;

import java.io.File;
import java.io.ObjectInputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Point;

/**
 * This is a delta script to update the gfe_gridlocaion table to store
 * coordinates/points as doubles/ints instead of java serialized byte arrays. It
 * does not need to be maintained in future releases.
 */
public class UpdateGridLocation {

    // SSL related constants
    private static final String CERTIFICATE_FILE_EXTENSION = ".crt";

    private static final String ROOT_CERTIFICATE_FILE_NAME = "root"
            + CERTIFICATE_FILE_EXTENSION;

    private static final String PKCS8_KEY_FILE_EXTENSION = ".pk8";

    private static final String SSL_FACTORY_CLASS = "org.postgresql.ssl.jdbc4.LibPQFactory";

    private static final String HOST_ARGUMENT = "-host";

    private static final String DEFAULT_HOST = "localhost";

    private static final String PORT_ARGUMENT = "-port";

    private static final String DEFAULT_PORT = "5432";

    private static final String DB_USER_ARGUMENT = "-user";

    private static final String DEFAULT_DB_USER = "awipsadmin";

    private static final String DATABASE_ARGUMENT = "-database";

    private static final String DEFAULT_DATABASE = "metadata";

    private static final String CERT_DIR_PATH_ARGUMENT = "-certDirPath";

    private static final String DEFAULT_CERT_DIR_PATH = System
            .getProperty("user.home") + File.separator + ".postgresql";

    private static final String SSL_MODE_ARGUMENT = "-sslMode";

    private static final String DEFAULT_SSL_MODE = "verify-ca";

    private static final String JDBC_CONNECTION_FORMAT_STRING = "jdbc:postgresql://%s:%s/%s";

    private static Map<String, Object> argumentMap = new HashMap<>();

    private static final List<String> COORDINATE_COLUMNS = Arrays
            .asList("origin", "extent", "latlonll", "latlonur", "latlonorigin");

    private static final List<String> POINT_COLUMNS = Arrays
            .asList("gridpointll", "gridpointur");

    private static final List<String> ALL_COLUMNS_LIST;
    static {
        ALL_COLUMNS_LIST = new ArrayList<>(COORDINATE_COLUMNS);
        ALL_COLUMNS_LIST.addAll(POINT_COLUMNS);
    }

    /**
     * @return a newly created connection to the database
     * @throws SQLException
     */
    private static Connection openConnection() throws SQLException, Exception {

        String host = getString(HOST_ARGUMENT, DEFAULT_HOST);
        String port = getString(PORT_ARGUMENT, DEFAULT_PORT);
        String database = getString(DATABASE_ARGUMENT, DEFAULT_DATABASE);

        String DBUser = getString(DB_USER_ARGUMENT, DEFAULT_DB_USER);
        String certDir = getString(CERT_DIR_PATH_ARGUMENT,
                DEFAULT_CERT_DIR_PATH);
        String sslMode = getString(SSL_MODE_ARGUMENT, DEFAULT_SSL_MODE);

        DriverManager.registerDriver(new org.postgresql.Driver());
        String connectionURL = String.format(JDBC_CONNECTION_FORMAT_STRING,
                host, port, database);

        StringBuilder filePath = new StringBuilder(certDir);
        filePath.append(File.separator);

        // the following connection properties are needed for DB access with SSL
        // certificates
        Properties props = new Properties();
        props.setProperty("sslmode", sslMode);
        props.setProperty("sslfactory", SSL_FACTORY_CLASS);
        props.setProperty("sslrootcert",
                filePath.toString() + ROOT_CERTIFICATE_FILE_NAME);
        props.setProperty("sslcert", filePath.append(DBUser).toString()
                + CERTIFICATE_FILE_EXTENSION);
        props.setProperty("sslkey",
                filePath.toString() + PKCS8_KEY_FILE_EXTENSION);
        props.setProperty("user", DBUser);

        return DriverManager.getConnection(connectionURL, props);
    }

    /**
     * Parse command line arguments into the argumentMap
     *
     * @param args
     */
    private static void parseArguments(String[] args) {
        for (int i = 0; i < args.length; ++i) {
            String arg = args[i];
            if (arg.startsWith("-")) {
                // we have a key
                if (args.length > (i + 1) && !args[i + 1].startsWith("-")) {
                    argumentMap.put(arg, args[i + 1]);
                    ++i;
                } else {
                    argumentMap.put(arg, true);
                }
            }
        }
    }

    /**
     * Get command line argument value
     *
     * @param key
     * @param defaultValue
     * @return
     */
    private static String getString(String key, String defaultValue) {
        Object val = argumentMap.get(key);
        if (val != null) {
            return val.toString();
        }
        return defaultValue;
    }

    private static Coordinate getCoordinate(ResultSet result, String column)
            throws Exception {
        Object obj = result.getObject(column);
        if (obj instanceof byte[]) {
            obj = new ObjectInputStream(result.getBinaryStream(column))
                    .readObject();
        } else if (obj instanceof Point) {
            // nothing to do
            return null;
        } else {
            throw new IllegalAccessException(
                    String.format("Unexpected type %s found in column %s",
                            obj.getClass().getName(), column));
        }

        if (obj instanceof Coordinate) {
            return (Coordinate) obj;
        } else if (obj instanceof com.vividsolutions.jts.geom.Coordinate) {
            com.vividsolutions.jts.geom.Coordinate oldCoord = (com.vividsolutions.jts.geom.Coordinate) obj;
            return new Coordinate(oldCoord.x, oldCoord.y);
        } else {
            throw new IllegalAccessException(
                    String.format("Unexpected type %s found in column %s",
                            obj.getClass().getName(), column));
        }
    }

    private static void saveCoordinate(Connection conn, int id, String column,
            Coordinate coord) throws Exception {
        StringBuilder updateSql = new StringBuilder();
        updateSql.append("UPDATE gfe_gridlocation SET (").append(column)
                .append("_x, ").append(column).append("_y)").append(" = (")
                .append(coord.x).append(", ").append(coord.y)
                .append(") WHERE id = ").append(id).append(";\n");

        try (Statement update = conn.createStatement()) {
            update.executeUpdate(updateSql.toString());
        }
    }

    private static java.awt.Point getPoint(ResultSet result, String column)
            throws Exception {
        Object obj = result.getObject(column);
        if (obj instanceof byte[]) {
            obj = new ObjectInputStream(result.getBinaryStream(column))
                    .readObject();
        } else {
            throw new IllegalAccessException(
                    String.format("Unexpected type %s found in column %s",
                            obj.getClass().getName(), column));
        }

        if (obj instanceof java.awt.Point) {
            return (java.awt.Point) obj;
        } else {
            throw new IllegalAccessException(
                    String.format("Unexpected type %s found in column %s",
                            obj.getClass().getName(), column));
        }
    }

    private static void savePoint(Connection conn, int id, String column,
            java.awt.Point point) throws Exception {
        StringBuilder updateSql = new StringBuilder();
        updateSql.append("UPDATE gfe_gridlocation SET (").append(column)
                .append("_x, ").append(column).append("_y)").append(" = (")
                .append(point.x).append(", ").append(point.y)
                .append(") WHERE id = ").append(id).append(";\n");

        try (Statement update = conn.createStatement()) {
            update.executeUpdate(updateSql.toString());
        }
    }

    public static void main(String[] args) throws Exception {
        System.out.println(
                "Running delta script for RODO DR 7596 to update the schema of the gfe_gridlocation table...");
        parseArguments(args);
        Connection conn = openConnection();

        try {
            conn.setAutoCommit(false);

            // add the new columns
            StringBuilder addSql = new StringBuilder();
            for (String column : COORDINATE_COLUMNS) {
                addSql.append("ALTER TABLE gfe_gridlocation ADD COLUMN ")
                        .append(column).append("_x double precision;\n")
                        .append("ALTER TABLE gfe_gridlocation ADD COLUMN ")
                        .append(column).append("_y double precision;\n");
            }

            for (String column : POINT_COLUMNS) {
                addSql.append("ALTER TABLE gfe_gridlocation ADD COLUMN ")
                        .append(column).append("_x integer;\n")
                        .append("ALTER TABLE gfe_gridlocation ADD COLUMN ")
                        .append(column).append("_y integer;\n");
            }
            try (Statement alter = conn.createStatement()) {
                alter.executeUpdate(addSql.toString());
            }

            // query the current values of the existing columns
            String query = "SELECT id, " + String.join(", ", ALL_COLUMNS_LIST)
                    + " FROM gfe_gridlocation;";

            try (ResultSet result = conn.createStatement()
                    .executeQuery(query)) {
                // set the values of the new columns
                while (result.next()) {
                    int id = result.getInt("id");
                    for (String column : COORDINATE_COLUMNS) {
                        Coordinate coord = getCoordinate(result, column);
                        saveCoordinate(conn, id, column, coord);
                    }

                    for (String column : POINT_COLUMNS) {
                        java.awt.Point point = getPoint(result, column);
                        savePoint(conn, id, column, point);
                    }
                }
            }

            // delete the old columns and make the new ones non-nullable
            StringBuilder deleteSql = new StringBuilder();
            for (String column : ALL_COLUMNS_LIST) {
                deleteSql.append("ALTER TABLE gfe_gridlocation DROP COLUMN ")
                        .append(column).append(";\n")
                        .append("ALTER TABLE gfe_gridlocation ALTER COLUMN ")
                        .append(column).append("_x SET NOT NULL;\n")
                        .append("ALTER TABLE gfe_gridlocation ALTER COLUMN ")
                        .append(column).append("_y SET NOT NULL;\n");
            }

            try (Statement delete = conn.createStatement()) {
                delete.executeUpdate(deleteSql.toString());
            }

            conn.commit();
            System.out.println("delta script for RODO DR 7596 complete");
        } catch (Exception e) {
            e.printStackTrace();
            conn.rollback();
            System.out.println("\ndelta script for RODO DR 7596 failed");
        } finally {
            conn.close();
        }

    }

}
