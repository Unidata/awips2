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
package com.raytheon.uf.common.dataplugin.pointset;

import java.io.File;
import java.io.FileNotFoundException;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStore.StoreOp;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * 
 * Container for the location information for a {@link PointSetRecord}. The
 * location information is specified as a longitude and latitude value for each
 * data point. The longitude and latitude values are stored as two separate
 * {@link FloatBuffer}s. The longitude, latitude and data values can be
 * correlated by index; a single data point can be represented by the values
 * from all the Buffers at a single index.
 * 
 * It is valid to reuse a PointSetLocation for multiple PointSetRecords. If
 * these records are using the same {@link File} for the {@link IDataStore} then
 * the location only needs to be stored once. A Location id is unique across
 * multiple times so if two PointSetRecords have the same location id then the
 * location only needs to be loaded once even if the records are stored in
 * different Files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 11, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class PointSetLocation {

    private static final String LATITUDE = "latitude";

    private static final String LONGITUDE = "longitude";

    private final FloatBuffer longitudes;

    private final FloatBuffer latitudes;

    private final String id;

    public PointSetLocation(float[] longitudes, float[] latitudes) {
        this(FloatBuffer.wrap(longitudes), FloatBuffer.wrap(latitudes));
    }

    public PointSetLocation(FloatBuffer longitudes, FloatBuffer latitudes) {
        if (longitudes.capacity() != latitudes.capacity()) {
            throw new IllegalArgumentException(
                    "Longitudes and Latitudes must be the same size");
        }
        this.longitudes = longitudes;
        this.latitudes = latitudes;
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-1");
            byte[] btmp = new byte[4];
            FloatBuffer ftmp = ByteBuffer.wrap(btmp).asFloatBuffer();
            while (longitudes.hasRemaining()) {
                ftmp.put(0, longitudes.get());
                md.digest(btmp);
            }
            longitudes.rewind();
            while (latitudes.hasRemaining()) {
                ftmp.put(0, latitudes.get());
                md.update(btmp);
            }
            latitudes.rewind();
            byte[] sha1hash = md.digest();
            StringBuilder sb = new StringBuilder(40);
            for (byte b : sha1hash) {
                sb.append(String.format("%02X", b));
            }
            id = sb.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("Missing SHA1 Algorithm", e);
        }
    }

    protected PointSetLocation(float[] longitudes, float[] latitudes, String id) {
        this.longitudes = FloatBuffer.wrap(longitudes);
        this.latitudes = FloatBuffer.wrap(latitudes);
        this.id = id;
    }

    /**
     * Unique identified for the data. Currently this is a SHA-1 so that if a
     * location is reused for unrelated records the location arrays are only
     * stored once.
     * 
     * @return
     */
    public String getId() {
        return id;
    }

    public FloatBuffer getLongitudes() {
        return longitudes;
    }

    public FloatBuffer getLatitudes() {
        return latitudes;
    }

    /**
     * Save to an {@link IDataStore}. The provided file will be used to get the
     * IDataStore and is not an actual file on the local system.
     */
    public void save(File file) throws StorageException {
        IDataStore store = DataStoreFactory.getDataStore(file);
        /*
         * In the future if there are locations being reused often then this
         * should check for the id in the store before persisting the data. For
         * now there are no known cases of duplicates that aren't decoded
         * together so this would be a waste of time.
         */
        store.addDataRecord(new FloatDataRecord(LONGITUDE, getGroup(id),
                longitudes.array()));
        store.addDataRecord(new FloatDataRecord(LATITUDE, getGroup(id),
                latitudes.array()));
        store.store(StoreOp.REPLACE);
    }

    /**
     * Load data from an {@link IDataStore}. The provided file will be used to
     * get the IDataStore and is not an actual file on the local system.
     */
    public static PointSetLocation load(File file, String id)
            throws FileNotFoundException, StorageException {
        IDataStore store = DataStoreFactory.getDataStore(file);
        IDataRecord[] datasets = store.retrieve(getGroup(id));
        float[] latitudes = null;
        float[] longitudes = null;

        for(IDataRecord record : datasets){
            if (record.getName().equals(LATITUDE)) {
                latitudes = (float[]) record.getDataObject();
            } else if (record.getName().equals(LONGITUDE)) {
                longitudes = (float[]) record.getDataObject();
            }
        }
        if (longitudes == null || latitudes == null) {
            throw new IllegalArgumentException(
                    "Unable to determine location information for " + id
                            + " from " + file.getName());
        }
        return new PointSetLocation(longitudes, latitudes, id);
    }

    protected static String getGroup(String id) {
        return "spatial-" + id;
    }

}
