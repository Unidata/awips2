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
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.numeric.filter.FillValueFilter;
import com.raytheon.uf.common.numeric.filter.UnsignedFilter;
import com.raytheon.uf.common.numeric.source.DataSource;

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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 03, 2012           bkowal      Initial creation
 * Mar 07, 2014  2791     bsteffen    Move Data Source/Destination to numeric
 *                                    plugin.
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
    public static DataSource constructArrayWrapper(IDataRecord dataRecord) {
        return constructArrayWrapper(dataRecord, true);
    }

    public static DataSource constructArrayWrapper(IDataRecord dataRecord,
            boolean signed) {
        long[] dimensions = dataRecord.getSizes();
        final int nx = (int) dimensions[0];
        final int ny = (int) dimensions[1];

        DataSource source = BufferWrapper.wrapArray(
                dataRecord.getDataObject(), nx, ny);
        if (!signed) {
            if (source instanceof ByteBufferWrapper) {
                source = UnsignedFilter.apply((ByteBufferWrapper) source);
            } else if (source instanceof ShortBufferWrapper) {
                source = UnsignedFilter.apply((ShortBufferWrapper) source);
            }
        }

        Number fillValue = dataRecord.getFillValue();
        if (fillValue != null) {
            source = FillValueFilter.apply(source, fillValue.doubleValue());
        }

        return source;
    }
}