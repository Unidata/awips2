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
package com.raytheon.uf.edex.upgrade.obslocation;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * Reformats the dataURI to match the new precision scheme for obs locations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2014 3410       bclement     Initial creation
 * Oct  4, 2016 DCS18655   K.Steinfeld  Implement secure DB access using SSL keys/certificates;
 *                                      remove DB password implementation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DataUriDoubleToFloat {
	
	// SSL related constants
    private static final String CERTIFICATE_FILE_EXTENSION = ".crt";

	private static final String ROOT_CERTIFICATE_FILE_NAME = "root" + CERTIFICATE_FILE_EXTENSION;
	
	private static final String PKCS8_KEY_FILE_EXTENSION = ".pk8";
	
    private static final String SSL_FACTORY_CLASS = "org.postgresql.ssl.jdbc4.LibPQFactory";
    // 
	
    private static final String HOST_ARGUMENT = "-host";

    private static final String DEFAULT_HOST = "localhost";

    private static final String PORT_ARGUMENT = "-port";

    private static final String DEFAULT_PORT = "5432";
    
    private static final String DB_USER_ARGUMENT = "-user";
    
    private static final String DEFAULT_DB_USER = "awips";

    private static final String DATABASE_ARGUMENT = "-database";

    private static final String DEFAULT_DATABASE = "metadata";
    
    private static final String CERT_DIR_PATH_ARGUMENT = "-certDirPath";
    
    private static final String DEFAULT_CERT_DIR_PATH = System.getProperty("user.home") + File.separator + ".postgresql";
    
    private static final String SSL_MODE_ARGUMENT = "-sslMode";
    
    private static final String DEFAULT_SSL_MODE = "verify-full";

    private static final String JDBC_CONNECTION_FORMAT_STRING = "jdbc:postgresql://%s:%s/%s";

    private static Map<String, Object> argumentMap = new HashMap<String, Object>();

    /* map of table names to latitude 0-based index in data uri */
    private static final Map<String, Integer> latitudeIndexMap = new HashMap<String, Integer>();

    static {
        latitudeIndexMap.put("tcg", 5);
        latitudeIndexMap.put("acars", 4);
        latitudeIndexMap.put("acarssounding", 3);
        latitudeIndexMap.put("ldad_manual", 5);
        latitudeIndexMap.put("obs", 5);
        latitudeIndexMap.put("airep", 5);
        latitudeIndexMap.put("bufrncwf", 3);
        latitudeIndexMap.put("svrwx", 4);
        latitudeIndexMap.put("ldadprofiler", 4);
        latitudeIndexMap.put("bufrquikscat", 4);
        latitudeIndexMap.put("sfcobs", 5);
        latitudeIndexMap.put("bufrua", 6);
        latitudeIndexMap.put("modelsounding", 4);
        latitudeIndexMap.put("fssobs", 5);
        latitudeIndexMap.put("lsr", 4);
        latitudeIndexMap.put("ldadhydro", 5);
        latitudeIndexMap.put("pirep", 5);
        latitudeIndexMap.put("profiler", 4);
        latitudeIndexMap.put("tcs", 4);
        latitudeIndexMap.put("ncpafm", 5);
        latitudeIndexMap.put("ncscd", 4);
        latitudeIndexMap.put("ncuair", 4);
        latitudeIndexMap.put("nctaf", 4);
    }

    private static final String LOC_DEF_COL = "locationdefined";

    private static final String DATAURI_COL = "datauri";

    private static final String LAT_COL = "latitude";

    private static final String LON_COL = "longitude";

    /**
     * @return a newly created connection to the database
     * @throws SQLException
     */
    private static Connection openConnection() throws SQLException, Exception {
    	
        String host = getString(HOST_ARGUMENT, DEFAULT_HOST);
        String port = getString(PORT_ARGUMENT, DEFAULT_PORT);
        String database = getString(DATABASE_ARGUMENT, DEFAULT_DATABASE);
        
        String DBUser = getString(DB_USER_ARGUMENT, DEFAULT_DB_USER);
        String certDir = getString(CERT_DIR_PATH_ARGUMENT, DEFAULT_CERT_DIR_PATH);
        String sslMode = getString(SSL_MODE_ARGUMENT, DEFAULT_SSL_MODE);

        DriverManager.registerDriver(new org.postgresql.Driver());
        String connectionURL = String.format(JDBC_CONNECTION_FORMAT_STRING, host, port, database);
        
        StringBuilder filePath = new StringBuilder(certDir);
        filePath.append(File.separator);
        
        // the following connection properties are needed for DB access with SSL certificates
        Properties props = new Properties();
        props.setProperty("sslmode", sslMode);
        props.setProperty("sslfactory", SSL_FACTORY_CLASS);
        props.setProperty("sslroot", filePath.toString() + ROOT_CERTIFICATE_FILE_NAME);
        props.setProperty("sslcert", filePath.append(DBUser).toString() + CERTIFICATE_FILE_EXTENSION);
        props.setProperty("sslkey", filePath.toString() + PKCS8_KEY_FILE_EXTENSION);

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
                if (args.length > (i + 1)
                        && args[i + 1].startsWith("-") == false) {
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
    
    /**
     * Get all tables in the schema with the provided column name
     * 
     * @param conn
     * @param column
     * @return
     * @throws Exception
     */
    private static Set<String> getTablesWithColumn(Connection conn,
            String column) throws Exception {
        Statement query = conn.createStatement();
        ResultSet result = query
                .executeQuery("select table_name from information_schema.columns where column_name = '" + column +"'");
        Set<String> rval = new HashSet<>();
        while (result.next()) {
            rval.add(result.getString("table_name"));
        }
        return rval;
    }

    /**
     * Create an updatable result set with id, latitude, longitude and datauri
     * columns
     * 
     * @param conn
     * @param table
     * @return
     * @throws Exception
     */
    private static ResultSet getLocationAndDataUri(Connection conn, String table)
            throws Exception {
        Statement query = conn.createStatement(ResultSet.TYPE_SCROLL_SENSITIVE,
                ResultSet.CONCUR_UPDATABLE);
        String sql = String.format("select id, %s, %s, %s from %s", LAT_COL,
                LON_COL, DATAURI_COL, table);
        return query.executeQuery(sql);
    }

    /**
     * Reformat each dataURI in table
     * 
     * @param conn
     * @param table
     * @throws Exception
     */
    private static void updateTable(Connection conn, String table)
            throws Exception {
        Integer latIndex = latitudeIndexMap.get(table);
        if (latIndex == null) {
            throw new Exception(
                    "Unable to determine index of latitude/longitude in dataURI");
        }
        /* plus 1 here to account for how String.split() handles leading slash */
        latIndex += 1;
        int lonIndex = latIndex + 1;
        ResultSet res = getLocationAndDataUri(conn, table);
        while (res.next()) {
            String uri = res.getString(DATAURI_COL);
            Float lat = res.getFloat(LAT_COL);
            Float lon = res.getFloat(LON_COL);
            if (uri == null) {
                int id = res.getInt("id");
                throw new Exception("Empty dataURI on row with id " + id);
            }
            String[] parts = uri.split("/");
            if (parts.length < lonIndex + 1) {
                throw new Exception("Expected dataURI with at least "
                        + (lonIndex + 1) + " parts, got " + uri);
            }
            String latStr = parts[latIndex];
            String lonStr = parts[lonIndex];
            String newLatStr = String.valueOf(lat);
            String newLonStr = String.valueOf(lon);
            if (!latStr.equals(newLatStr) || !lonStr.equals(newLonStr)) {
                parts[latIndex] = newLatStr;
                parts[lonIndex] = newLonStr;
                StringBuilder sb = new StringBuilder();
                /*
                 * skip first element due to String.split() with leading slash
                 */
                for (int i = 1; i < parts.length; ++i) {
                    sb.append("/").append(parts[i]);
                }
                res.updateString(DATAURI_COL, sb.toString());
                try {
                    res.updateRow();
                } catch (SQLException e) {
                    if (e.getMessage().contains("duplicate key")) {
                        /*
                         * this can happen if data has been ingested twice with
                         * both the float locations and the double locations.
                         */
                        res.deleteRow();
                        System.out.println("Encountered duplicate row after"
                                + " reformatting, deleted row with dataURI "
                                + uri + " to resolve conflict.");
                    } else {
                        throw e;
                    }
                }
            }
        }
        System.out.println("Updated table: " + table);
    }

    /**
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        parseArguments(args);
        Connection conn = openConnection();
        Set<String> locationTables = getTablesWithColumn(conn, LOC_DEF_COL);
        Set<String> dataUriTables = getTablesWithColumn(conn, DATAURI_COL);
        /* only look at tables that both use obs location and have data uris */
        locationTables.retainAll(dataUriTables);
        for (String table : locationTables) {
            try {
                updateTable(conn, table);
            } catch (Exception e) {
                String msg = e.getLocalizedMessage();
                System.err.println("ERROR: Unable to update table " + table
                        + ": " + e.getLocalizedMessage());
                if (msg == null || msg.isEmpty()) {
                    e.printStackTrace();
                }

            }
        }
    }

}
