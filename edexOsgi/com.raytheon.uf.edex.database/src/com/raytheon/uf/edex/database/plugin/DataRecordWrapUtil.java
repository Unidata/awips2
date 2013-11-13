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
package com.raytheon.uf.edex.database.plugin;

import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.interpolation.data.AbstractDataWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.ByteArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.DataWrapper1D;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.IntArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.ShortArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.UnsignedByteArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.UnsignedShortArrayWrapper;

/**
 * Utility for wrapping data records to be a data source/destination
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2013  2393      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class DataRecordWrapUtil {

    /**
     * Create an {@link AbstractDataWrapper} source from the supplied
     * {@link IDataRecord} with given dimensions.
     * 
     * @param rec
     *            The record containing data to be wrapped.
     * @param nx
     *            Number of items on the x axis.
     * @param ny
     *            Number of items on the y axis.
     * @return The wrapped data.
     * @throws StorageException
     */
    public static DataWrapper1D wrap(IDataRecord rec, int nx, int ny)
            throws StorageException {
        // default to signed data
        return wrap(rec, nx, ny, false);
    }

    /**
     * Create an {@link AbstractDataWrapper} source from the supplied
     * {@link IDataRecord} with given dimensions.
     * 
     * @param rec
     *            The record containing data to be wrapped.
     * @param nx
     *            Number of items on the x axis.
     * @param ny
     *            Number of items on the y axis.
     * @param unsigned
     *            attempt to treat the data as unsigned if possible
     * @return The wrapped data.
     * @throws StorageException
     */
    public static DataWrapper1D wrap(IDataRecord rec, int nx, int ny,
            boolean unsigned) throws StorageException {
        DataWrapper1D source = null;

        if (rec instanceof ByteDataRecord) {
            byte[] b = ((ByteDataRecord) rec).getByteData();
            if (unsigned) {
                source = new UnsignedByteArrayWrapper(b, nx, ny);
            } else {
                source = new ByteArrayWrapper(b, nx, ny);
            }
        } else if (rec instanceof ShortDataRecord) {
            short[] s = ((ShortDataRecord) rec).getShortData();
            if (unsigned) {
                source = new UnsignedShortArrayWrapper(s, nx, ny);
            } else {
                source = new ShortArrayWrapper(s, nx, ny);
            }
        } else if (rec instanceof IntegerDataRecord) {
            int[] i = ((IntegerDataRecord) rec).getIntData();
            source = new IntArrayWrapper(i, nx, ny);
        } else if (rec instanceof FloatDataRecord) {
            float[] f = ((FloatDataRecord) rec).getFloatData();
            source = new FloatArrayWrapper(f, nx, ny);
        } else {
            throw new StorageException("Unsupported data record type: "
                    + rec.getClass(), rec);
        }
        return source;
    }

}
