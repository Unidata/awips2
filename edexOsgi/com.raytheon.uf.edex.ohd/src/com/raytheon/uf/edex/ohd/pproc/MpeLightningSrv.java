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
import java.util.HashMap;
import java.util.Map;

import org.hibernate.HibernateException;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 06, 2011 5951       jnjanga     Initial creation
 * Jan 10, 2013 1448       bgonzale    Added app context check in runOnSchedule().
 * Jan 18, 2013 1469       bkowal      Removed the hdf5 data directory.
 * Mar 28, 2014 2952       mpduff      Changed to use UFStatus for logging.
 * Jun 05, 2014 3226       bclement    BinLightning refactor
 * Aug 20, 2014 3549       njensen     Fixed spelling in exceptions
 * Sep 17, 2014 3015       bclement    improved exception handling
 * Dec 04, 2014 3015       njensen     Corrected usage of Coordinate(x, y)
 * Feb 25, 2015 3992       nabowle     Limit getMostRecentStrikes to NLDN.
 *                                     Deduplicate lightning data in a
 *                                     single BinLightningRecord.
 * Jul 09, 2015 4500       rjpeter     Fix SQL Injection concern.
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

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(MpeLightningSrv.class);

    /**
     * Check the metadata Database for new lightning entries.
     * 
     * @return rows returned from the query
     */
    private Object[] getMostRecentStrikes() throws EdexException {
        CoreDao coreDao = new CoreDao(DaoConfig.DEFAULT);
        /*
         * TODO: This can miss data, should use insertTime and track last pull
         * time
         */
        final String lgtSQL = "select datauri from binlightning "
                + "where reftime > (now()- interval \'30 minutes \')"
                + "and source = :source";
        try {
            return coreDao.executeSQLQuery(lgtSQL, "source",
                    LightningConstants.DEFAULT_SOURCE);
        } catch (Exception e) {
            throw new EdexException("Couldn't get BinLightning records from"
                    + " metadata database. Failed SQL: " + lgtSQL, e);
        }
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
            byte[] strikes = ltngRec.getPulseCounts();
            int[] intensities = ltngRec.getIntensities();

            Map<LightningData, Integer> lightningData = new HashMap<>();

            // convert latitude and longitude to grid coordinate
            HRAP hrap = HRAP.getInstance();
            PixelOrientation po = PixelOrientation.CENTER;
            Coordinate gridCell = new Coordinate();
            LightningData data;
            Integer oldIntensity;
            for (int i = 0; i < latitudes.length; i++) {
                float lat = latitudes[i];
                float lon = longitudes[i];
                Coordinate c = new Coordinate(lon, lat);
                gridCell = hrap.latLonToGridCoordinate(c, po);
                data = new LightningData((short) gridCell.x,
                        (short) gridCell.y, obstimes[i], strikes[i]);

                // deduplicate lightning data that will create the same
                // primary keys.
                oldIntensity = lightningData.get(data);
                if (oldIntensity == null) {
                    lightningData.put(data, intensities[i]);
                } else {
                    logger.debug("dataURI " + dataURI
                            + " has multiple lightning data for "
                            + "ihfs.Lightning pk (" + data.getX() + ", "
                            + data.getY() + ", " + data.getObstime() + ")");
                    if (intensities[i] > oldIntensity.intValue()) {
                        /*
                         * highest intensity data is retained. #put() does not
                         * replace keys, so because only some of the fields are
                         * used for hashcode and equals, we must remove the old
                         * key before putting the new key.
                         */
                        lightningData.remove(data);
                        lightningData.put(data, intensities[i]);
                    }
                }
            }

            // set up query first
            StringBuilder sql = new StringBuilder("INSERT INTO lightning "
                    + " (x_hgrid, y_hgrid, obstime, no_of_strike) VALUES");
            // form tuples
            for (LightningData lightning : lightningData.keySet()) {
                // need to convert obstime from seconds
                // to timestamp type for table insertion
                Timestamp ts = new Timestamp(lightning.getObstime());
                String tuple = "(" + lightning.getX() + "," + lightning.getY()
                        + ", TIMESTAMP '" + ts.toString() + "' ,"
                        + lightning.getStrikes() + ")";
                tuple = tuple + ",";
                sql.append(tuple);
            }
            sql.replace(sql.length() - 1, sql.length(), ";");

            // insert all the tuples into the 'lightning' table in ihfs
            // database.
            CoreDao coreDao = new CoreDao(DaoConfig.forDatabase("ihfs"));
            results = coreDao.executeSQLUpdate(sql.toString());
            logger.info("inserted " + results + " items in ihfs.Lightning db ");

        } catch (StorageException se) {
            throw new EdexException(
                    "Could not retrieve datasets from datastore for dataURI: "
                            + dataURI, se);
        } catch (HibernateException he) {
            throw new EdexException(
                    "Could not insert into ifhs lightning table for dataURI: "
                            + dataURI, he);
        } catch (Exception e) {
            throw new EdexException(
                    "Could not convert to grid coordinate for dataURI: "
                            + dataURI, e);
        }
    }

    /**
     * Populates ifhs' lightning table with the resultset obtained from querying
     * metadata's binlighting table.
     * 
     * @param rows
     * @throws EdexException
     */
    private void ifhsInsertMostRecentStrikes(Object[] rows)
            throws EdexException {
        if (rows.length == 0) {
            logger.info("No new lightning records to insert in ifhs. ");
        }
        for (Object col : rows) {
            String dataURI = (String) col;
            ifhsInsertLightRecord(dataURI);
        }
    }

    /**
     * run at scheduled timer.
     * 
     * @throws EdexException
     */
    public void runOnSchedule() throws EdexException {
        if (!AppsDefaults.getInstance().setAppContext(this)) {
            return;
        }
        Object[] rows = getMostRecentStrikes();
        ifhsInsertMostRecentStrikes(rows);
    }

    /**
     * Class to simplify deduplicating lightning data in a
     * {@link BinLightningRecord} that generate the same ihfs lightning primary
     * key.
     */
    private static class LightningData {
        private final short x;

        private final short y;

        private final long obstime;

        private final byte strikes;

        public LightningData(short x, short y, long time, byte strikes) {
            super();
            this.x = x;
            this.y = y;
            this.obstime = time;
            this.strikes = strikes;
        }

        /**
         * @return the x
         */
        public short getX() {
            return x;
        }

        /**
         * @return the y
         */
        public short getY() {
            return y;
        }

        /**
         * @return the obstime
         */
        public long getObstime() {
            return obstime;
        }

        /**
         * @return the strikes
         */
        public byte getStrikes() {
            return strikes;
        }

        /**
         * Generate a hashcode using the ihfs primary key fields: x, y, and
         * time.
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = (prime * result) + (int) (obstime ^ (obstime >>> 32));
            result = (prime * result) + x;
            result = (prime * result) + y;
            return result;
        }

        /**
         * Determine equality using the ihfs primary key fields: x, y, and time.
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            LightningData other = (LightningData) obj;
            if (obstime != other.obstime) {
                return false;
            }
            if (x != other.x) {
                return false;
            }
            if (y != other.y) {
                return false;
            }
            return true;
        }
    }
}
