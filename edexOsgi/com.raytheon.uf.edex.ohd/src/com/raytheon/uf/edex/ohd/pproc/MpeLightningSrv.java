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

package com.raytheon.uf.edex.ohd.pproc;

import java.io.File;
import java.sql.Timestamp;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.HibernateException;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Service implementation for gathering the lightning datasets from files in
 * HDF5 format and inserting them into the ifhs lightning table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date              Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 06, 2011       5951      jnjanga     Initial creation
 * Jan 18, 2013       1469      bkowal      Removed the hdf5 data directory.
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */

public class MpeLightningSrv {

    private static class BinLightningPathProvider extends DefaultPathProvider {
        private static BinLightningPathProvider instance = new BinLightningPathProvider();

        public static BinLightningPathProvider getInstance() {
            return instance;
        }

        protected BinLightningPathProvider() {

        }

        public synchronized String getHDFPath(IPersistable persistable) {
            return File.separator + "binlightning";
        }
    }

    private Log logger = LogFactory.getLog(getClass());

    /**
     * Check the metadata Database for new lightning entries.
     * 
     * @return rows returned from the query
     */
    private QueryResultRow[] getMostRecentStrikes() throws EdexException {
        QueryResult rs = null;
        CoreDao coreDao = new CoreDao(DaoConfig.DEFAULT);
        final String lgtSQL = "select datauri from binlightning "
                + "where reftime > (now()- interval \'30 minutes \')";
        try {
            rs = (QueryResult) coreDao.executeNativeSql(lgtSQL, true);
        } catch (Exception e) {
            throw new EdexException("Couldn't get BinLightning records from"
                    + " metadata database. " + e);
        }
        return rs.getRows();
    }

    /**
     * Inserts a single record into ihfs's lightning table.
     * 
     * @param dataURI
     * @throws EdexException
     */
    private void ifhsInsertLightRecord(String dataURI) throws EdexException {
        int results = 0;
        try {
            // set up a lightning record
            BinLightningRecord ltngRec = new BinLightningRecord(dataURI);

            // create custom path provider for binlightning repository
            BinLightningPathProvider pathProvider = BinLightningPathProvider
                    .getInstance();

            // obtain the hdf5 filename
            String persistDir = pathProvider.getHDFPath(ltngRec)
                    + File.separator;
            String archive = pathProvider.getHDFFileName(
                    ltngRec.getPluginName(), ltngRec);
            File persistFile = new File(persistDir, archive);

            // connect to the data store
            IDataStore ds = DataStoreFactory.getDataStore(persistFile);

            // retrieve the datasets
            ltngRec.retrieveFromDataStore(ds);
            logger.info("retrieved datasets for lightning file " + persistDir
                    + archive);

            // get refs to datasets
            float[] latitudes = ltngRec.getLatitudes();
            float[] longitudes = ltngRec.getLongitudes();
            long[] obstimes = ltngRec.getObsTimes();
            byte[] strikes = ltngRec.getStrikeCounts();

            // convert latitude and longitude to grid coordinate
            HRAP hrap = HRAP.getInstance();
            PixelOrientation po = PixelOrientation.CENTER;
            short[] x_hgrids = new short[latitudes.length];
            short[] y_hgrids = new short[longitudes.length];
            Coordinate gridCell = new Coordinate();
            for (int i = 0; i < latitudes.length; i++) {
                float lat = latitudes[i];
                float lon = longitudes[i];
                Coordinate latLon = new Coordinate(lat, lon);
                gridCell = hrap.latLonToGridCoordinate(latLon, po);
                x_hgrids[i] = (short) gridCell.x;
                y_hgrids[i] = (short) gridCell.y;
            }

            // set up query first
            StringBuilder sql = new StringBuilder("INSERT INTO lightning "
                    + " (x_hgrid, y_hgrid, obstime, no_of_strike) VALUES");
            // form tuples
            for (int j = 0; j < x_hgrids.length; j++) {
                // need to convert obstime from seconds
                // to timestamp type for table insertion
                Timestamp ts = new Timestamp(obstimes[j]);
                String tuple = "(" + x_hgrids[j] + "," + y_hgrids[j]
                        + ", TIMESTAMP '" + ts.toString() + "' ," + strikes[j]
                        + ")";
                if (j != x_hgrids.length - 1) {
                    tuple = tuple + ",";
                } else {
                    tuple = tuple + ";";
                }
                sql.append(tuple);
            }

            // insert all the tuples into the 'lightning' table in ihfs
            // database.
            CoreDao coreDao = new CoreDao(DaoConfig.forDatabase("ihfs"));
            results = coreDao.executeSQLUpdate(sql.toString());
            logger.info("inserted " + results + " items in ihfs.Lightning db ");

        } catch (StorageException se) {
            throw new EdexException(
                    "Could not retrive datasets from datastore : " + se);
        } catch (HibernateException he) {
            throw new EdexException(
                    "Could not insert into ifhs ligthning table : " + he);
        } catch (Exception e) {
            throw new EdexException("Could not convert to grid coordinate : "
                    + e);
        }
    }

    /**
     * Populates ifhs' lightning table with the resultset obtained from querying
     * metadata's binlighting table.
     * 
     * @param rows
     * @throws EdexException
     */
    private void ifhsInsertMostRecentStrikes(QueryResultRow[] rows)
            throws EdexException {
        if (rows.length == 0) {
            logger.info("No new lightning records to insert in ifhs. ");
        }
        for (QueryResultRow row : rows) {
            String dataURI = (String) row.getColumn(0);
            ifhsInsertLightRecord(dataURI);
        }
    }

    /**
     * run at scheduled timer.
     * 
     * @throws EdexException
     */
    public void runOnSchedule() throws EdexException {
        QueryResultRow[] rows = getMostRecentStrikes();
        ifhsInsertMostRecentStrikes(rows);
    }
}
