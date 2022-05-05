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
package com.raytheon.uf.edex.ohd.areal;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.geotools.data.DataStore;
import org.geotools.data.DefaultTransaction;
import org.geotools.data.Transaction;
import org.geotools.data.collection.ListFeatureCollection;
import org.geotools.data.postgis.PostGISDialect;
import org.geotools.data.simple.SimpleFeatureCollection;
import org.geotools.data.simple.SimpleFeatureStore;
import org.geotools.feature.simple.SimpleFeatureBuilder;
import org.geotools.feature.simple.SimpleFeatureTypeBuilder;
import org.geotools.jdbc.JDBCDataStore;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.hibernate.internal.SessionFactoryImpl;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;
import org.opengis.feature.type.Name;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.areal.ArealTypeSelection;
import com.raytheon.uf.common.hydro.areal.GeoAreaData;
import com.raytheon.uf.common.hydro.util.DbUtils;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.jdbc.SSLEnabledPostgisNGDataStoreFactory;
import org.locationtech.jts.densify.Densifier;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Polygon;

/**
 * This class handles the database access for Hydro's importing of Areal data
 * and SSHP basins.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2018 6979       mpduff      Initial creation
 * Mar 11, 2020 19533  mgamazaychikov  Minor log handling cleanup
 * Apr 24, 2019 6140       tgurney     Hibernate 5 fixes
 *
 * </pre>
 *
 * @author mpduff
 */

public class ArealDataAccessor {

    private static final String NEWLINE = System.getProperty("line.separator");

    private final IUFStatusHandler statusHandler = UFStatus.getHandler(ArealDataAccessor.class);

    /** pghost token */
    private static final String PGHOST = "pghost";

    /** pgport token */
    private static final String PGPORT = "pgport";

    private static final String SSHP_BASINS_TABLE = "sshpbasins";

    private StringBuilder logger = new StringBuilder();

    private CoreDao hydroDao = new CoreDao(
            DaoConfig.forDatabase(CommonHydroConstants.IHFS));

    public ArealDataAccessor() {

    }

    /**
     * Update the Areal data tables for Hydro. Tables include mapdata.sshpbasins
     * and IHFS tables are linesegs and geodata.
     *
     * @param geoDataList
     *            List of GeoAreaData objects
     * @param selectedType
     *            Type of area selected
     * @return String to add to the log
     * @throws Exception
     */
    public String updateArealData(List<GeoAreaData> geoDataList,
            ArealTypeSelection selectedType) throws Exception {

        /*
         * Delete the information from the GeoArea and LineSegs table for the
         * boundary type being processed.
         */
        if (selectedType != ArealTypeSelection.RESERVOIRS) {

            try {
                int numSegDel = deleteLineSegs(selectedType.getDataName());
                log("Deleting LineSegs using query: DELETE FROM LineSegs WHERE"
                        + " area_id IN (SELECT area_id FROM GeoArea WHERE boundary_type='"
                        + selectedType.getDataName() + "');");

                log("DELETE " + numSegDel);
            } catch (Exception e) {
                statusHandler
                        .error("Error deleting rows from LineSegs table for "
                                + selectedType.toString(), e);
                log("Could not delete rows corresponding to GeoArea type");
                log(selectedType.toString()
                        + " from the LineSegs database table.");
                return logger.toString();
            }
        }

        try {
            int numGeoDel = deleteGeoArea(selectedType.getDataName());
            log("Deleting GeoArea info using query: DELETE FROM GeoArea WHERE"
                    + " boundary_type='" + selectedType.getDataName() + "';");

            log("DELETE " + numGeoDel);
        } catch (Exception e) {
            statusHandler.error("Could not delete rows from GeoArea for "
                    + selectedType.toString(), e);
            log("Could not delete rows from the GeoArea table");
            log("corresponding to boundary_type " + selectedType.toString());
            return logger.toString();
        }

        String areaId = null;
        try {
            for (GeoAreaData data : geoDataList) {
                /* load the data into the database if good data */
                if (data.isSaveDataBlock()) {
                    areaId = data.getAreaId();
                    putGeoArea(data);
                } else {
                    log("ERROR: Discarding block of data for id "
                            + data.getAreaId() + " due to errors");
                }
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Error writing to geoarea table for " + areaId,
                    e);
            log("ERROR: Write to geoarea failed for " + areaId);
            return logger.toString();
        }

        // Load the linesegs table
        HydroGeoProcessor proc;
        try {
            proc = new HydroGeoProcessor();
        } catch (Exception e) {
            statusHandler.error("Error initializing the Hydro GeoProcessor", e);
            log("Error initializing the Hydro GeoProcessor");
            return logger.toString();
        }

        if (selectedType != ArealTypeSelection.RESERVOIRS) {
            for (GeoAreaData data : geoDataList) {

                /* do the main processing */
                HrapBinList binList;
                try {
                    binList = proc.getHrapBinList(data);
                } catch (Exception e) {
                    statusHandler
                            .error("Error creating HRAP bins for input data "
                                    + data.getAreaId(), e);
                    log("Error creating HRAP bins for input data "
                            + data.getAreaId());
                    return logger.toString();
                }

                log("Processing area " + data.getAreaId() + ":" + "  Writing "
                        + binList.getNumRows() + " rows");
                putLineSegs(data.getAreaId(), binList);
            }
        }

        // Populate the mapdata.sshpBasins table
        putSshpBasins(geoDataList);
        log("Wrote " + geoDataList.size()
                + " SSHP Basins to sshpbasins table.");
        return logger.toString();
    }

    private int deleteLineSegs(String dataType) {
        String query = "DELETE FROM LineSegs WHERE area_id IN ("
                + "SELECT area_id FROM GeoArea WHERE boundary_type='" + dataType
                + "')";

        return hydroDao.executeSQLUpdate(query);
    }

    private int deleteGeoArea(String dataName) {
        String query = "DELETE FROM GeoArea WHERE boundary_type = '" + dataName
                + "'";
        return hydroDao.executeSQLUpdate(query);
    }

    private int putGeoArea(GeoAreaData data) {
        double[] lat;
        double[] lon;
        double intLat;
        double intLon;
        double latTotal = 0;
        double lonTotal = 0;
        double posWeight = 0;
        double weightTotal = 0;

        if (data == null) {
            return -1;
        }

        /*
         * if the interior lat, lon were provided from the input file, then use
         * them. otherwise compute them.
         */
        lat = data.getLat();
        lon = data.getLon();

        if (data.getInteriorLat() != -1) {
            intLat = data.getInteriorLat();
            intLon = data.getInteriorLon();
        } else {
            /*
             * compute the interior lat-lon using weighted method; subtract
             * numbers to reduce magnitude out of paranoia
             */
            for (int i = 0; i < data.getNumberPoints(); i++) {
                if (i > 0) {
                    posWeight = Math.sqrt(Math.pow(lat[i] - lat[i - 1], 2)
                            + Math.pow(lon[i] - lon[i - 1], 2));
                }

                weightTotal += posWeight;
                latTotal += posWeight * lat[i];
                lonTotal += posWeight * lon[i];
            }

            // In Hydro Longitude must be > 0
            intLon = Math.abs(lonTotal / weightTotal);
            intLat = latTotal / weightTotal;

            data.setInteriorLat(intLat);
            data.setInteriorLon(intLon);
        }

        String dataName = DbUtils.escapeSpecialCharforStr(data.getName());

        StringBuilder query = new StringBuilder();
        query.append("insert into geoarea (area_id,");
        query.append(" name, boundary_type, interior_lat, interior_lon)");
        query.append(" values ('").append(data.getAreaId()).append("', '");
        query.append(dataName).append("', '");
        query.append(data.getBoundaryType()).append("', ");
        query.append(data.getInteriorLat()).append(", ");
        query.append(data.getInteriorLon()).append(")");

        return hydroDao.executeSQLUpdate(query.toString());
    }

    private void putLineSegs(String areaId, HrapBinList binList) {
        int status = 0;
        long count = 0;
        double area = binList.getArea();
        StringBuilder query = new StringBuilder();
        StringBuilder where = new StringBuilder();

        for (int i = 0; i < binList.getNumRows(); i++) {
            long hrapRow = binList.getRows().get(i);
            long hrapBegCol = binList.getBeginCols().get(i);
            long hrapEndCol = binList.getEndCols().get(i);

            where.setLength(0);
            query.setLength(0);

            /* check if the record exists in the database */
            where.append(" where area_id = '").append(areaId).append("' and ");
            where.append(" hrap_row = ").append(hrapRow);
            where.append(" and hrap_beg_col = ").append(hrapBegCol);

            query.append("select count(*) from linesegs ");
            query.append(where.toString());
            Object[] rs = hydroDao.executeSQLQuery(query.toString());
            count = ((Number) rs[0]).longValue();
            if (count == 0) {
                // insert the record
                query.setLength(0);
                query.append("insert into linesegs (area_id, hrap_row, ");
                query.append("hrap_beg_col, hrap_end_col, area) values ");
                query.append("('").append(areaId).append("', ").append(hrapRow)
                        .append(", ");
                query.append(hrapBegCol).append(", ").append(hrapEndCol)
                        .append(", ");
                query.append(area).append(")");

                try {
                    status = hydroDao.executeSQLUpdate(query.toString());
                    if (status != 1) {
                        throw new Exception(
                                "Invalid status for insert into linesegs.");
                    }
                } catch (Exception e) {
                    status = -1;
                    statusHandler
                            .error("Error puttin dta into LineSegs for area_id "
                                    + areaId, e);
                    log("Error putting data into LineSegs for area_id:  "
                            + areaId);
                }
            } else {
                /* delete the record and insert the new record */
                String delete = "delete from linesegs " + where.toString();

                try {
                    status = hydroDao.executeSQLUpdate(delete);
                    if (status != 1) {
                        throw new Exception(
                                "Invalid status for delete from linesegs.");
                    }
                } catch (Exception e) {
                    statusHandler.error(
                            "Error updating LineSegs for area_id: " + areaId,
                            e);
                    log("Error updating LineSegs for area_id:  " + areaId);
                }
            }
        }
    }

    private void putSshpBasins(List<GeoAreaData> geoDataList) {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String host = appsDefaults.getToken(PGHOST);
        String port = appsDefaults.getToken(PGPORT);

        Object obj = EDEXUtil.getESBComponent("admin_mapsSessionFactory");
        SessionFactoryImpl factory = (SessionFactoryImpl) obj;

        // Get the connection properties
        Map<String, Object> props = factory.getProperties();
        String userName = props.get("connection.username").toString();
        String sslMode = props.get("connection.sslmode").toString();
        String sslCert = props.get("connection.sslcert").toString();
        String sslKey = props.get("connection.sslkey").toString();
        String sslRootCert = props.get("connection.sslrootcert").toString();

        Map<String, Object> params = new HashMap<>();
        params.put("dbtype", "postgis");
        params.put("host", host);
        params.put("port", port);
        params.put("schema", "mapdata");
        params.put("database", "maps");
        params.put("user", userName);
        params.put("sslmode", sslMode);
        params.put("sslcert", sslCert);
        params.put("sslkey", sslKey);
        params.put("sslrootcert", sslRootCert);
        params.put("validate connections", true);

        /*
         * Had trouble connecting to localhost, which was returned from the
         * spring admin_mapsSessionFactory. Change host to 127.0.0.1 and the
         * connection was successful. Leaving this here for future developers to
         * help with troubleshooting locally.
         */
        // params.put("host", "127.0.0.1");

        DataStore dataStore = null;
        try {
            SSLEnabledPostgisNGDataStoreFactory fac = new SSLEnabledPostgisNGDataStoreFactory();
            dataStore = fac.createDataStore(params);
            if (dataStore == null) {
                log("ERROR:  Could not connect to maps database over SSL on host "
                        + host);
                return;
            }
            ((JDBCDataStore) dataStore)
                    .setSQLDialect(new AwipsDialect((JDBCDataStore) dataStore));
            List<Name> names = dataStore.getNames();
            boolean exists = false;
            for (Name name : names) {
                if (name.getLocalPart().equals(SSHP_BASINS_TABLE)) {
                    exists = true;
                    break;
                }
            }

            // Drop the table if it exists
            if (exists) {
                dataStore.removeSchema(SSHP_BASINS_TABLE);
            }

            // Builds the FeatureType (table)
            SimpleFeatureTypeBuilder builder = new SimpleFeatureTypeBuilder();
            builder.setName(SSHP_BASINS_TABLE);
            builder.setCRS(DefaultGeographicCRS.WGS84);

            // Add attributes in table column order
            builder.length(8).add("areaId", String.class);
            builder.length(45).add("areaname", String.class);
            builder.add("featurerank", Integer.class);
            builder.add("lat", Double.class);
            builder.add("lon", Double.class);
            builder.srid(4326).add("the_geom", Polygon.class);
            builder.srid(4326).add("the_geom_0", Polygon.class);

            // Build the type (table)
            final SimpleFeatureType SSHP_BASINS = builder.buildFeatureType();

            // Create the table
            dataStore.createSchema(SSHP_BASINS);

            SimpleFeatureStore store = (SimpleFeatureStore) dataStore
                    .getFeatureSource(SSHP_BASINS_TABLE);

            SimpleFeatureType featureType = store.getSchema();

            /*
             * A list to collect features as we create them.
             */
            List<SimpleFeature> features = new ArrayList<>();
            for (GeoAreaData data : geoDataList) {
                Coordinate[] coords = data.getAreaCoordinates();
                Polygon poly = MapUtil.getPolygon(coords);

                Object[] values = new Object[7];
                values[0] = data.getAreaId();
                values[1] = data.getName();

                // feature rank not parsed in A1
                values[2] = -1;
                values[3] = data.getInteriorLat();
                values[4] = data.getInteriorLon();
                values[5] = poly;
                values[6] = Densifier.densify(poly, 0.1);

                features.add(
                        SimpleFeatureBuilder.build(featureType, values, "gid"));
            }

            SimpleFeatureCollection collection = new ListFeatureCollection(
                    featureType, features);
            Transaction transaction = null;
            try {
                transaction = new DefaultTransaction("Store SSHP Basins");
                store.setTransaction(transaction);
                store.addFeatures(collection);
                transaction.commit();
            } catch (Exception e) {
                statusHandler.error(
                        "Error storing data to the sshpbasins table.", e);
                log("Error storing data to the sshpbasins table.");
                if (transaction != null) {
                    transaction.rollback();
                }
            } finally {
                if (transaction != null) {
                    transaction.close();
                }
            }
        } catch (IOException e) {
            statusHandler.error("Error storing data to the sshpbasins table.",
                    e);
            log("Error storing data to the sshpbasins table.");
        } finally {
            if (dataStore != null) {
                dataStore.dispose();
            }
        }
    }

    /**
     * Send a message to the log file
     *
     * @param stmt
     *            A message to be logged
     */
    private void log(String stmt) {
        logger.append(stmt).append(NEWLINE);
    }

    /**
     * This class is used to change the primary key column from the default fid
     * name to the AWIPS gid name.
     */
    private class AwipsDialect extends PostGISDialect {

        public AwipsDialect(JDBCDataStore dataStore) {
            super(dataStore);
        }

        @Override
        public void encodePrimaryKey(String column, StringBuffer sql) {
            encodeColumnName(null, "gid", sql);
            sql.append(" SERIAL PRIMARY KEY");
        }
    }
}
