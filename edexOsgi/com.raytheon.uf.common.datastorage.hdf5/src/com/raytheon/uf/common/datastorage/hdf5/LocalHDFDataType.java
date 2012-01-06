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
package com.raytheon.uf.common.datastorage.hdf5;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import ncsa.hdf.hdf5lib.HDF5Constants;

import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;

/**
 * Enum to manage HDF5 data types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 12, 2007             chammack    Initial Creation. (HDF5DataStore)
 * 20070914            379  jkorman     Refactored from HDFDataStore.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public enum LocalHDFDataType {

    BYTE(HDF5Constants.H5T_NATIVE_INT8, "BYTE"), SHORT(
            HDF5Constants.H5T_NATIVE_SHORT, "SHORT"), INT(
            HDF5Constants.H5T_NATIVE_INT, "INT"), LONG(
            HDF5Constants.H5T_NATIVE_INT64, "LONG"), FLOAT(
            HDF5Constants.H5T_NATIVE_FLOAT, "FLOAT"), DOUBLE(
            HDF5Constants.H5T_NATIVE_DOUBLE, "DOUBLE"), STRING(
            HDF5Constants.H5T_STR_NULLTERM, "STRING");

    private final int hdfNativeDataType;

    private final String localName;

    private static final Map<Integer, LocalHDFDataType> typeMap = new HashMap<Integer, LocalHDFDataType>();
    static {
        typeMap.put(BYTE.getHDFNativeType(), BYTE);
        typeMap.put(SHORT.getHDFNativeType(), SHORT);
        typeMap.put(INT.getHDFNativeType(), INT);
        typeMap.put(LONG.getHDFNativeType(), LONG);
        typeMap.put(FLOAT.getHDFNativeType(), FLOAT);
        typeMap.put(DOUBLE.getHDFNativeType(), DOUBLE);
        typeMap.put(STRING.getHDFNativeType(), STRING);
    }

    private static final Map<Class<? extends IDataRecord>, LocalHDFDataType> dataRecordMap = new HashMap<Class<? extends IDataRecord>, LocalHDFDataType>();
    static {
        dataRecordMap.put(FloatDataRecord.class, FLOAT);
        dataRecordMap.put(IntegerDataRecord.class, INT);
        dataRecordMap.put(ShortDataRecord.class, SHORT);
        dataRecordMap.put(LongDataRecord.class, LONG);
        dataRecordMap.put(StringDataRecord.class, STRING);
        dataRecordMap.put(ByteDataRecord.class, BYTE);
    }

    /**
     * Construct an instance of this class.
     * 
     * @param hdfDataType
     *            The HDF5 data type.
     * @param name
     *            The name of this instance.
     */
    private LocalHDFDataType(int hdfDataType, String name) {
        this.hdfNativeDataType = hdfDataType;
        localName = name;
    }

    /**
     * Get the HDF5 data type associated with this instance.
     * 
     * @return The HDF5 data type.
     */
    public int getHDFNativeType() {
        return hdfNativeDataType;
    }

    /**
     * Get the string representation of this instance.
     * 
     * @return The string representation.
     */
    @Override
    public String toString() {
        return localName;
    }

    /**
     * Get the LocalHDFDataType enum corresponding to a specified HDF5 data
     * type. If the data type is not defined, a null is returned.
     * 
     * @param hdfDataType
     *            HDF5 data type to find.
     * @return The LocalHDFDataType if defined, null otherwise.
     */
    public static LocalHDFDataType getCorrespondingEnum(int hdfDataType) {
        return typeMap.get(hdfDataType);
    }

    /**
     * Get the LocalHDFDataType enum corresponding to a specified data type
     * name. If the data type is not defined, a null is returned.
     * 
     * @param typeName
     *            hdfDataType HDF5 data type name to find.
     * @return The LocalHDFDataType if defined, null otherwise.
     */
    public static LocalHDFDataType getCorrespondingEnum(String typeName) {
        LocalHDFDataType dataType = null;
        Collection<LocalHDFDataType> dataTypes = typeMap.values();
        for (LocalHDFDataType type : dataTypes) {
            if (type.localName.equals(typeName)) {
                dataType = type;
                break;
            }
        }
        return dataType;
    }

    public static LocalHDFDataType getCorrespondingEnum(IDataRecord rec) {
        if (rec == null) {
            return null;
        }

        return dataRecordMap.get(rec.getClass());
    }
}
