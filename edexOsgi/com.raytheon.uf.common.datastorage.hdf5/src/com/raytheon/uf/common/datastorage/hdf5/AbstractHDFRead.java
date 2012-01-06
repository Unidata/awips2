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

import java.awt.Point;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import ncsa.hdf.hdf5lib.H5;
import ncsa.hdf.hdf5lib.HDF5Constants;
import ncsa.hdf.hdf5lib.HDFNativeData;
import ncsa.hdf.hdf5lib.exceptions.HDF5Exception;
import ncsa.hdf.hdf5lib.exceptions.HDF5LibraryException;

import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.Request.Type;
import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * Defines the base implementation of an HDF5 Read
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public abstract class AbstractHDFRead {

    private static Method select;

    static {
        try {
            Method[] methods = H5.class.getDeclaredMethods();
            for (Method method : methods) {
                if (method.getName().equals("H5Sselect_elements")) {
                    Class<?>[] clazz = method.getParameterTypes();
                    if (clazz.length != 4) {
                        continue;
                    }

                    if (clazz[0] == Integer.TYPE && clazz[1] == Integer.TYPE
                            && clazz[2] == Integer.TYPE
                            && clazz[3] == byte[].class) {
                        select = method;
                        select.setAccessible(true);
                        break;
                    }
                }

            }
        } catch (Exception e) {
            System.out
                    .println("Method signature for H5Sselect_elements not found.");
            e.printStackTrace();
        }
    }

    public IDataRecord read(Request request, int file_id, String group,
            String dataset, float scaleFactor) throws StorageException {
        int dataset_id = -1;
        int typeid = -1;
        int memspace = -1;
        int dataspace = -1;
        int plist = -1;
        Number fillValueResult = null;

        try {
            // Open an existing dataset.

            dataset_id = H5.H5Dopen(file_id, group + "/" + dataset);

            plist = H5.H5Dget_create_plist(dataset_id);

            typeid = H5.H5Dget_type(dataset_id);

            int classid = H5.H5Tget_class(typeid);

            int size = H5.H5Tget_size(typeid);
            boolean isVL = H5.H5Tis_variable_str(typeid);
            
            dataspace = H5.H5Dget_space(dataset_id);

            int sz = H5.H5Sget_simple_extent_ndims(dataspace);

            long[] dims = new long[sz];
            long[] originalDims = new long[sz];

            int totalSize = 1;
            H5.H5Sget_simple_extent_dims(dataspace, dims, (long[]) null);
            // perform a deep copy
            for (int i = 0; i < originalDims.length; i++) {
                originalDims[i] = dims[i];
            }

            // optimization:
            // if a line query was used, but it's only 1d, this is really
            // a point query. We _could_ use hyperslabs to do this, but
            // it would be a lot slower than a regular point query, so
            // just rewrite the request as a point query.
            AbstractHDFRead readObj = this;
            if (dims.length == 1
                    && (request.getType() == Type.XLINE || request.getType() == Type.YLINE)) {
                int[] idx = request.getIndices();
                int[] pts = new int[idx.length];
                Set<Point> p = new LinkedHashSet<Point>(idx.length);
                int k = 0;
                for (int i = 0; i < idx.length; i++) {
                    if (request.getType() == Type.XLINE) {
                        if (idx[i] >= originalDims[1])
                            continue;
                    } else if (request.getType() == Type.YLINE) {
                        if (idx[i] >= originalDims[0])
                            continue;
                    }
                    p.add(new Point(idx[i], 0));
                    pts[k] = idx[i];
                    k++;
                }

                if (k != idx.length) {
                    // Prune the caller's copy (optimization)
                    idx = new int[k];
                    System.arraycopy(pts, 0, idx, 0, k);
                    request.setIndices(idx);
                }

                request = (Request.buildPointRequest(p.toArray(new Point[p
                        .size()])));
                readObj = HDF5OpManager.readTypes.get(request.getType());
            }

            totalSize = readObj.calculateSize(dataspace, sz, request, dims,
                    originalDims, totalSize);

            LocalHDFDataType dataType = null;
            Object data = null;
            Object fillValue = null;
            int readType = 0;
            if (classid == HDF5Constants.H5T_FLOAT) {
                dataType = LocalHDFDataType.FLOAT;
                data = new float[totalSize];
                fillValue = new float[1];
                readType = HDF5Constants.H5T_NATIVE_FLOAT;
            } else if (classid == HDF5Constants.H5T_INTEGER) {
                if (size == 1) {
                    dataType = LocalHDFDataType.BYTE;
                    data = new byte[totalSize];
                    fillValue = new byte[1];
                    readType = HDF5Constants.H5T_NATIVE_INT8;
                } else if (size == 2) {
                    dataType = LocalHDFDataType.SHORT;
                    data = new short[totalSize];
                    fillValue = new short[1];
                    readType = HDF5Constants.H5T_NATIVE_SHORT;
                } else if (size == 4) {
                    dataType = LocalHDFDataType.INT;
                    data = new int[totalSize];
                    fillValue = new int[1];
                    readType = HDF5Constants.H5T_NATIVE_INT;
                } else if (size == 8) {
                    dataType = LocalHDFDataType.LONG;
                    data = new long[totalSize];
                    fillValue = new long[1];
                    readType = HDF5Constants.H5T_NATIVE_INT64;
                }
            } else if (classid == HDF5Constants.H5T_STRING) {
                data = new String[totalSize];
                dataType = LocalHDFDataType.STRING;
                readType = typeid;
            }

            memspace = H5.H5Screate_simple(sz, dims, null);

            readObj.selectSet(memspace, dataspace, request, dims, originalDims);

            if (dataType == LocalHDFDataType.STRING && isVL) {
                H5.H5DreadVL(dataset_id, readType, memspace, dataspace,
                        HDF5Constants.H5P_DEFAULT, (Object[]) data);
            } else {
                H5.H5Dread(dataset_id, readType, memspace, dataspace,
                        HDF5Constants.H5P_DATASET_XFER_DEFAULT, data);
            }

            if (fillValue != null) {
                H5.H5Pget_fill_value(plist, readType, fillValue);
                if (fillValue instanceof double[]) {
                    fillValueResult = new Double(((double[]) fillValue)[0]);
                } else if (fillValue instanceof int[]) {
                    fillValueResult = new Integer(((int[]) fillValue)[0]);
                } else if (fillValue instanceof byte[]) {
                    fillValueResult = new Byte(((byte[]) fillValue)[0]);
                } else if (fillValue instanceof long[]) {
                    fillValueResult = new Long(((long[]) fillValue)[0]);
                } else if (fillValue instanceof short[]) {
                    fillValueResult = new Short(((short[]) fillValue)[0]);
                } else if (fillValue instanceof float[]) {
                    fillValueResult = new Float(((float[]) fillValue)[0]);
                }

            }

            // Swizzle and scale the dims to match java nomenclature
            long[] dims2 = new long[dims.length];
            int k = 0;
            for (int i = dims2.length - 1; i >= 0; i--) {
                dims2[k] = (int) (dims[i] * scaleFactor);
                k++;
            }

            AbstractStorageRecord rec = DataStoreFactory.createStorageRecord(
                    dataset, group, data, sz, dims2);
            if (fillValueResult != null) {
                rec.setFillValue(fillValueResult);
            }
            readProperties(rec, dataset_id);

            return rec;
        } catch (Exception e) {
            try {
                H5.H5Eclear();
            } catch (HDF5LibraryException e1) {
                // ignore
            }
            throw new StorageException("Error occurred during retrieve ", null,
                    e);
        } finally {

            try {
                if (memspace >= 0) {
                    H5.H5Sclose(memspace);
                }
            } catch (HDF5LibraryException e) {
                // ignore
            }
            try {
                if (dataspace >= 0) {
                    H5.H5Sclose(dataspace);
                }
            } catch (HDF5LibraryException e) {
                // ignore
            }

            try {
                if (plist >= 0) {
                    H5.H5Pclose(plist);
                }
            } catch (HDF5LibraryException e) {
                // ignore
            }

            try {
                if (dataset_id >= 0) {
                    H5.H5Dclose(dataset_id);
                }
            } catch (HDF5LibraryException e) {
                // ignore
            }

            try {
                if (typeid >= 0) {
                    H5.H5Tclose(typeid);
                }
            } catch (HDF5LibraryException e) {
                // ignore
            }
        }
    }

    private void readProperties(IDataRecord dr, int dataSet) {
        try {

            Map<String, Object> attribMap = new LinkedHashMap<String, Object>();
            int attribs = H5.H5Aget_num_attrs(dataSet);
            if (attribs > 0) {
                dr.setDataAttributes(attribMap);
            }

            for (int i = 0; i < attribs; i++) {

                int attribId = 0;
                int type = 0;
                int nativeType = 0;
                try {
                    attribId = H5.H5Aopen_idx(dataSet, i);
                    String[] str = new String[1];
                    H5.H5Aget_name(attribId, 256, str);
                    type = H5.H5Aget_type(attribId);
                    int spc = H5.H5Tget_size(type);
                    nativeType = H5.H5Tget_native_type(type);
                    int cls = H5.H5Tget_class(type);

                    if (cls == HDF5Constants.H5T_INTEGER) {
                        Object d = null;
                        switch (spc) {
                        case 1:
                            // byte
                            d = new byte[1];
                            H5.H5Aread(attribId, HDF5Constants.H5T_NATIVE_INT,
                                    d);
                            attribMap.put(str[0], ((byte[]) d)[0]);
                            break;
                        case 2:
                            // short
                            d = new short[1];
                            H5.H5Aread(attribId, HDF5Constants.H5T_NATIVE_INT,
                                    d);
                            attribMap.put(str[0], ((short[]) d)[0]);
                            break;
                        case 4:
                            // regular int
                            d = new int[1];
                            H5.H5Aread(attribId, HDF5Constants.H5T_NATIVE_INT,
                                    d);
                            attribMap.put(str[0], ((int[]) d)[0]);
                            break;
                        case 8:
                            // long
                            d = new long[1];
                            H5.H5Aread(attribId, HDF5Constants.H5T_NATIVE_INT,
                                    d);
                            attribMap.put(str[0], ((long[]) d)[0]);
                            break;
                        }

                    } else if (cls == HDF5Constants.H5T_FLOAT) {
                        if (spc == 4) {
                            float[] d = new float[1];
                            H5.H5Aread(attribId,
                                    HDF5Constants.H5T_NATIVE_FLOAT, d);
                            attribMap.put(str[0], d[0]);
                        } else if (spc == 8) {
                            double[] d = new double[1];
                            H5.H5Aread(attribId,
                                    HDF5Constants.H5T_NATIVE_DOUBLE, d);
                            attribMap.put(str[0], d[0]);
                        }
                    } else if (cls == HDF5Constants.H5T_STRING) {
                        byte[] b = new byte[spc];
                        H5.H5Aread(attribId, nativeType, b);
                        String outStr = new String(b, 0, spc - 1);
                        attribMap.put(str[0], outStr);
                    } else {
                        throw new IllegalArgumentException(
                                "Unable to handle type" + cls);
                    }
                } catch (HDF5Exception e) {
                    H5.H5Eclear();
                    e.printStackTrace();
                } finally {
                    if (type > 0) {
                        try {
                            H5.H5Tclose(type);
                        } catch (Exception e) {
                        }
                    }

                    if (attribId > 0) {
                        try {
                            H5.H5Aclose(attribId);
                        } catch (Exception e) {
                        }
                    }

                }
            }
        } catch (HDF5LibraryException e) {
            try {
                H5.H5Eclear();
            } catch (HDF5LibraryException e1) {
            }
            e.printStackTrace();
            return;
        }

    }

    protected abstract void selectSet(int memspace, int dataspace,
            Request request, long[] dims, long[] originalDatasetDims)
            throws HDF5Exception, HDF5LibraryException, StorageException;

    protected abstract int calculateSize(int dataspace, int sz,
            Request request, long[] dims, long[] originalDims, int totalSize)
            throws HDF5LibraryException;

    // For some reason, the hdf5 folks implemented this using reflection which
    // makes it very slow. This implementation is at least 5x faster
    protected static final byte[] longToBytes(long[][] indices) {
        long[] inn = new long[indices.length * indices[0].length];
        for (int j = 0; j < indices.length; j++) {
            System.arraycopy(indices[j], 0, inn, j * indices[0].length,
                    indices[0].length);
        }

        return HDFNativeData.longToByte(0, inn.length, inn);
    }

    protected static int invokeH5Sselect_elements(int space, int op,
            int num_elements, byte[] coords) throws StorageException {
        // synchronization not needed since caller methods are synchronized
        if (select == null) {
            throw new StorageException(
                    "Method did not initialize properly?  Incompatible version of hdf5?",
                    null);
        }

        try {
            Integer i = (Integer) select.invoke(null, space, op, num_elements,
                    coords);
            return i;
        } catch (Exception e) {
            throw new StorageException("Error selecting elements", null, e);
        }
    }

}
