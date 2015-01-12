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
package com.raytheon.uf.edex.upgrade.satellitespatial;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.EnvelopeIntersection;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKBReader;

/**
 * Java application to update the satellite spatial table. Converts old spatial
 * format into new format using crs space
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2013       2333 mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class UpdateSatSpatial {

    private static final String SATELLITE_SPATIAL_TABLE = "satellite_spatial";

    private static final String SATELLITE_SPATIAL_GID = "gid";

    private static final String SATELLITE_SPATIAL_CRSWKT = "crswkt";

    private static final String SATELLITE_SPATIAL_GEOM = "the_geom";

    private static final String SATELLITE_SPATIAL_NX = "nx";

    private static final String SATELLITE_SPATIAL_NY = "ny";

    private static final String SATELLITE_SPATIAL_DX = "dx";

    private static final String SATELLITE_SPATIAL_DY = "dy";

    private static final String SATELLITE_SPATIAL_MINX = "minx";

    private static final String SATELLITE_SPATIAL_MINY = "miny";

    private static final String SATELLITE_SPATIAL_MINIMUMS = "minimums";

    private static final String HOST_ARGUMENT = "-host";

    private static final String DEFAULT_HOST = "localhost";

    private static final String PORT_ARGUMENT = "-port";

    private static final String DEFAULT_PORT = "5432";

    private static final String USER_ARGUMENT = "-user";

    private static final String DEFAULT_USER = "awips";

    private static final String PASSWORD_ARGUMENT = "-password";

    private static final String DEFAULT_PASSWORD = "awips";

    private static final String DATABASE_ARGUMENT = "-database";

    private static final String DEFAULT_DATABASE = "metadata";

    private static final String JDBC_CONNECTION_FORMAT_STRING = "jdbc:postgresql://%s:%s/%s";

    private static final String USER_PROPERTY = "user";

    private static final String PASSWORD_PROPERTY = "password";

    private static Map<String, Object> argumentMap = new HashMap<String, Object>();

    private static class SpatialObject implements ISpatialObject {

        private static final long serialVersionUID = 1L;

        private final int nx;

        private final int ny;

        private final Geometry geometry;

        private final CoordinateReferenceSystem crs;

        public SpatialObject(int nx, int ny, Geometry geometry,
                CoordinateReferenceSystem crs) {
            this.nx = nx;
            this.ny = ny;
            this.geometry = geometry;
            this.crs = crs;
        }

        @Override
        public Geometry getGeometry() {
            return geometry;
        }

        @Override
        public CoordinateReferenceSystem getCrs() {
            return crs;
        }

        @Override
        public Integer getNx() {
            return nx;
        }

        @Override
        public Integer getNy() {
            return ny;
        }

    }

    public static void main(String[] args) throws Exception {
        // Parse arguments
        parseArguments(args);
        Connection conn = openConnection();

        Statement query = conn.createStatement();

        ResultSet results = query.executeQuery("SELECT ("
                + SATELLITE_SPATIAL_MINX + " || '_' || "
                + SATELLITE_SPATIAL_MINY + ") as " + SATELLITE_SPATIAL_MINIMUMS
                + ", " + SATELLITE_SPATIAL_GID + ", "
                + SATELLITE_SPATIAL_CRSWKT + ", " + SATELLITE_SPATIAL_NX + ", "
                + SATELLITE_SPATIAL_NY + ", " + SATELLITE_SPATIAL_DX + ", "
                + SATELLITE_SPATIAL_DY + ", AsBinary(" + SATELLITE_SPATIAL_GEOM
                + ") as " + SATELLITE_SPATIAL_GEOM + " FROM "
                + SATELLITE_SPATIAL_TABLE);

        String updateStatement = "UPDATE " + SATELLITE_SPATIAL_TABLE + " SET ("
                + SATELLITE_SPATIAL_MINX + ", " + SATELLITE_SPATIAL_MINY + ", "
                + SATELLITE_SPATIAL_DX + ", " + SATELLITE_SPATIAL_DY + ", "
                + SATELLITE_SPATIAL_GEOM
                + ") = (?, ?, ?, ?, GeomFromText(? , -1)) WHERE "
                + SATELLITE_SPATIAL_GID + " = ?";

        while (results.next()) {
            int gid = results.getInt(SATELLITE_SPATIAL_GID);
            String mins = results.getString(SATELLITE_SPATIAL_MINIMUMS);
            if (mins == null || mins.isEmpty()) {
                System.out
                        .println("Upgrading satellite_spatial record: " + gid);
                // No minimum values set, continue with upgrade
                Geometry geometry = new WKBReader().read(results
                        .getBytes(SATELLITE_SPATIAL_GEOM));
                CoordinateReferenceSystem crs = CRS.parseWKT(results
                        .getString(SATELLITE_SPATIAL_CRSWKT));
                int nx = results.getInt(SATELLITE_SPATIAL_NX);
                int ny = results.getInt(SATELLITE_SPATIAL_NY);
                double dx = results.getDouble(SATELLITE_SPATIAL_DX);
                double dy = results.getDouble(SATELLITE_SPATIAL_DY);

                ISpatialObject object = new SpatialObject(nx, ny, geometry, crs);
                GridGeometry2D resultGeom = MapUtil.getGridGeometry(object);

                Envelope2D env = resultGeom.getEnvelope2D();
                GridEnvelope2D grid = resultGeom.getGridRange2D();
                double minX = env.getMinX();
                double minY = env.getMinY();

                if (dx == 0.0) {
                    dx = env.getWidth() / grid.width;
                }
                if (dy == 0.0) {
                    dy = env.getHeight() / grid.height;
                }

                Geometry newGeom = EnvelopeIntersection
                        .createEnvelopeIntersection(
                                resultGeom.getEnvelope(),
                                new Envelope2D(DefaultGeographicCRS.WGS84,
                                        -180, -90, 360, 180), 1.0, 10, 10)
                        .getEnvelope();

                PreparedStatement update = conn
                        .prepareStatement(updateStatement);
                int index = 1;
                update.setDouble(index++, minX);
                update.setDouble(index++, minY);
                update.setDouble(index++, dx);
                update.setDouble(index++, dy);
                update.setString(index++, newGeom.toText());
                update.setInt(index++, gid);

                update.execute();
            } else {
                System.err
                        .println("Skipping update of satellite_spatial record: "
                                + gid);
            }
        }

        conn.close();
    }

    private static Connection openConnection() throws SQLException {
        String host = getString(HOST_ARGUMENT, DEFAULT_HOST);
        String port = getString(PORT_ARGUMENT, DEFAULT_PORT);
        String database = getString(DATABASE_ARGUMENT, DEFAULT_DATABASE);
        String user = getString(USER_ARGUMENT, DEFAULT_USER);
        String password = getString(PASSWORD_ARGUMENT, DEFAULT_PASSWORD);

        DriverManager.registerDriver(new org.postgresql.Driver());
        String connectionURL = String.format(JDBC_CONNECTION_FORMAT_STRING,
                host, port, database);
        Properties props = new Properties();
        props.setProperty(USER_PROPERTY, user);
        props.setProperty(PASSWORD_PROPERTY, password);

        return DriverManager.getConnection(connectionURL, props);
    }

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

    private static String getString(String key, String defaultValue) {
        Object val = argumentMap.get(key);
        if (val != null) {
            return val.toString();
        }
        return defaultValue;
    }

}
