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
package com.raytheon.uf.common.dataaccess.util;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.interpolation.data.ByteArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.IntArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.ShortArrayWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.DataWrapper1D;

/**
 * This methods in this utility may eventually be added to an abstract class.
 * The abstract class may be extended by any factories that utilize the metadata
 * table or most factories that need to access IDataRecord. Can be determined as
 * more factories are implemented for data types that are accessed from
 * metadata.
 * 
 * This utility contains a method to convert an IDataRecord into the associated
 * DataWrapper1D.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 03, 2012            bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public final class DataWrapperUtil {
    /**
	 * 
	 */
    private DataWrapperUtil() {
    }

    /**
     * Constructs an array wrapper to wrap the raw information stored in the
     * supplied IDataRecord; the array wrapper that is constructed is dependent
     * on the type of IDataRecord that is supplied.
     * 
     * @param dataRecord the IDataRecord with raw data
     * @return the array-wrapped data
     */
    public static DataWrapper1D constructArrayWrapper(IDataRecord dataRecord) {
        DataWrapper1D arrayWrapper = null;

        long[] dimensions = dataRecord.getSizes();
        final int nx = (int) dimensions[0];
        final int ny = (int) dimensions[1];

        /*
         * determine the type of the data record and construct the appropriate
         * array wrapper.
         */
        if (dataRecord instanceof ByteDataRecord) {
            ByteDataRecord byteDataRecord = (ByteDataRecord) dataRecord;

            arrayWrapper = new ByteArrayWrapper(byteDataRecord.getByteData(),
                    nx, ny);
        } else if (dataRecord instanceof FloatDataRecord) {
            FloatDataRecord floatDataRecord = (FloatDataRecord) dataRecord;

            arrayWrapper = new FloatArrayWrapper(
                    floatDataRecord.getFloatData(), nx, ny);
        } else if (dataRecord instanceof IntegerDataRecord) {
            IntegerDataRecord integerDataRecord = (IntegerDataRecord) dataRecord;

            arrayWrapper = new IntArrayWrapper(integerDataRecord.getIntData(),
                    nx, ny);
        } else if (dataRecord instanceof ShortDataRecord) {
            ShortDataRecord shortDataRecord = (ShortDataRecord) dataRecord;

            arrayWrapper = new ShortArrayWrapper(
                    shortDataRecord.getShortData(), nx, ny);
        }

        arrayWrapper.setFillValue(dataRecord.getFillValue().doubleValue());

        return arrayWrapper;
    }
}