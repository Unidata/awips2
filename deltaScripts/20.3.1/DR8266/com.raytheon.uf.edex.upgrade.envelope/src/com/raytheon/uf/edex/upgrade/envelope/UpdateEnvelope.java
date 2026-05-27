package com.raytheon.uf.edex.upgrade.envelope;

import java.io.File;
import java.io.ObjectInputStream;
import java.io.StreamCorruptedException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * This is a delta script to update the viirs_spatial and modis_spatial tables
 * to store geometries as type "geometry" instead of java serialized byte
 * arrays. It does not need to be maintained in future releases.
 */
public class UpdateEnvelope {

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

    private static String getGeomWkt(ResultSet result, String column)
            throws Exception {
        Object obj = result.getObject(column);
        try {
            if (obj instanceof byte[]) {
                try {
                    obj = new ObjectInputStream(result.getBinaryStream(column))
                            .readObject();
                } catch (StreamCorruptedException e) {
                    // Could be a WKB
                    obj = (new org.locationtech.jts.io.WKBReader())
                            .read((byte[]) obj);
                }
            }
            if (obj instanceof org.locationtech.jts.geom.Geometry) {
                return ((org.locationtech.jts.geom.Geometry) obj).toText();
            } else if (obj instanceof com.vividsolutions.jts.geom.Geometry) {
                com.vividsolutions.jts.geom.Geometry oldGeom = (com.vividsolutions.jts.geom.Geometry) obj;
                return oldGeom.toText();
            }
        } catch (Exception e) {
            return null;
        }
        System.out.println("WARN: Unexpected type " + obj.getClass().getName()
                + " found in column " + column);
        return null;
    }

    private static boolean shouldUpdate(Connection conn, String tableName)
            throws SQLException {
        String checkQuery = "select data_type, udt_name"
                + " from information_schema.columns"
                + " where table_catalog = 'metadata'"
                + " and table_schema = 'awips'" + " and table_name = '"
                + tableName + "' and column_name = 'envelope';";
        try (ResultSet result = conn.createStatement()
                .executeQuery(checkQuery)) {
            if (!result.next()) {
                System.out.println("WARN: column " + tableName
                        + ".envelope does " + "not exist, skipping this one");
            }
            String dataType = result.getString("data_type");
            String udtName = result.getString("udt_name");
            if ("USER-DEFINED".equals(dataType) && "geometry".equals(udtName)) {
                System.out.println("INFO: " + tableName
                        + " has already been updated, skipping this one");
                return false;
            } else if (!"bytea".equals(dataType)) {
                System.out.println("WARN: " + tableName
                        + ".envelope has data type " + dataType
                        + ", expected bytea. Skipping this one");
                return false;
            }
        }
        return true;
    }

    public static void main(String[] args) throws Exception {
        System.out
                .println("Running delta script for RODO DR 8266 to update the "
                        + "viirs_spatial and modis_spatial tables...");
        parseArguments(args);
        Connection conn = openConnection();

        try {
            conn.setAutoCommit(false);
            for (String tableName : new String[] { "viirs_spatial",
                    "modis_spatial" }) {

                if (!shouldUpdate(conn, tableName)) {
                    continue;
                } else {
                    System.out.println(
                            "INFO: " + tableName + " is being updated");
                }
                try (Statement alter = conn.createStatement()) {
                    alter.executeUpdate("alter table " + tableName
                            + " add column _envelope geometry;");
                }
                String query = "SELECT gid, envelope FROM " + tableName;

                try (ResultSet result = conn.createStatement()
                        .executeQuery(query)) {
                    // set the values of the new columns
                    while (result.next()) {
                        int gid = result.getInt("gid");
                        String geomWkt = getGeomWkt(result, "envelope");
                        String updateSql = "";
                        if (geomWkt == null) {
                            System.out.println("WARN: Record in table "
                                    + tableName + " with gid = " + gid
                                    + " is corrupt, it will be deleted");
                            updateSql = "delete from " + tableName
                                    + " where gid = " + gid;
                        } else {
                            updateSql = "update " + tableName
                                    + " set _envelope " + " = ST_GeomFromText('"
                                    + geomWkt + "') where gid = " + gid;
                        }

                        try (Statement update = conn.createStatement()) {
                            update.executeUpdate(updateSql);
                        }
                    }
                }

                // replace new column with old
                StringBuilder deleteSql = new StringBuilder();
                deleteSql.append("ALTER TABLE ").append(tableName)
                        .append(" DROP COLUMN envelope;\n");
                deleteSql.append("ALTER TABLE ").append(tableName)
                        .append(" RENAME COLUMN _envelope to envelope;\n");

                try (Statement delete = conn.createStatement()) {
                    delete.executeUpdate(deleteSql.toString());
                }
            }
            conn.commit();
            System.out.println("delta script for RODO DR 7596 complete");
        } catch (Exception e) {
            e.printStackTrace();
            conn.rollback();
            System.out.println("\ndelta script for RODO DR 7596 failed, "
                    + "changes have been rolled back.");
        } finally {
            conn.close();
        }

    }
}
