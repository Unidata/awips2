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

package com.raytheon.uf.common.datastorage;

import java.io.File;

import com.raytheon.uf.common.datastorage.records.AbstractStorageRecord;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.datastorage.records.StringDataRecord;

/**
 * This class is used for constructing IDataStore objects. It is a single that
 * has an IDataStoreFactory that it uses to actually do the construction of the
 * IDataStore objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 12, 2007             chammack    Initial Creation.
 * 20070914            379  jkorman     Added createStorageRecord factory methods.
 *                                      Refactored from HDFDataStore.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class DataStoreFactory {

    private static final DataStoreFactory instance = new DataStoreFactory();

    private IDataStoreFactory underlyingFactory;

    public static DataStoreFactory getInstance() {
        return instance;
    }

    /**
     * Get the underlying IDataStoreFactory in the DataStoreFactory
     * 
     * @return the underlying factory used for creating IDataStore objects
     */
    public IDataStoreFactory getUnderlyingFactory() {
        return underlyingFactory;
    }

    /**
     * Set the underlying factory to be used by the DataStoreFactory
     * 
     * @param underlyingFactory
     *            the underlying factory to use
     */
    public void setUnderlyingFactory(IDataStoreFactory factory) {
        this.underlyingFactory = factory;
    }

    public static IDataStore getDataStore(File file) {
        return getDataStore(file, true);
    }

    protected static IDataStore getDataStore(File file, boolean useLocking) {
        return instance.underlyingFactory.getDataStore(file, useLocking);
    }

    /**
     * Create an AbstractStorageRecord from given parameters.
     * 
     * @param name
     *            The name of the data item.
     * @param group
     *            The group to store the record in
     * @param data
     *            An array of primitive type e.g. byte []
     * @param dimension
     *            Number of dimensions in the data.
     * @param sizes
     *            Size of each dimension.
     * @return Return null if the type of the input data is not recognized.
     */
    public static AbstractStorageRecord createStorageRecord(String name,
            String group, Object data, int dimension, long[] sizes) {
        AbstractStorageRecord record = null;

        if ((byte[].class) == data.getClass()) {
            record = new ByteDataRecord(name, group, (byte[]) data, dimension,
                    sizes);
        } else if ((short[].class) == data.getClass()) {
            record = new ShortDataRecord(name, group, (short[]) data,
                    dimension, sizes);
        } else if ((int[].class) == data.getClass()) {
            record = new IntegerDataRecord(name, group, (int[]) data,
                    dimension, sizes);
        } else if ((long[].class) == data.getClass()) {
            record = new LongDataRecord(name, group, (long[]) data, dimension,
                    sizes);
        } else if ((float[].class) == data.getClass()) {
            record = new FloatDataRecord(name, group, (float[]) data,
                    dimension, sizes);
        } else if (String[].class == data.getClass()) {
            record = new StringDataRecord(name, group, (String[]) data,
                    dimension, sizes);
        } else {
            throw new IllegalArgumentException(
                    "Don't know how to reconstruct for datatype: " + data);
        }
        return record;
    }

    /**
     * Create an AbstractStorageRecord from given parameters for singly
     * dimensioned data.
     * 
     * @param name
     *            The name of the data item.
     * @param group
     *            The group to store in
     * @param data
     *            An array of primitive type e.g. byte []
     * @return Return null if the type of the input data is not recognized.
     */
    public static AbstractStorageRecord createStorageRecord(String name,
            String group, Object data) {
        AbstractStorageRecord record = null;

        if ((byte[].class) == data.getClass()) {
            record = new ByteDataRecord(name, group, (byte[]) data);
        } else if ((short[].class) == data.getClass()) {
            record = new ShortDataRecord(name, group, (short[]) data);
        } else if ((int[].class) == data.getClass()) {
            record = new IntegerDataRecord(name, group, (int[]) data);
        } else if ((long[].class) == data.getClass()) {
            record = new LongDataRecord(name, group, (long[]) data);
        } else if ((float[].class) == data.getClass()) {
            record = new FloatDataRecord(name, group, (float[]) data);
        }

        return record;
    }

}
