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
 * - AWIPS2 Baseline Repository --------
 * Jul 18, 2012        798 jkorman      Extracted methods {@link #createDataSetName}, {@link #createGroupName}, and
 * {@link #isInterpolated} from various classes.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class DataStoreFactory {

    /**
     * Default data set name; {@value #DEF_DATASET_NAME}.
     */
    public static final String DEF_DATASET_NAME = "Data";

    /**
     * Default interpolation suffix ({@value #DEF_INTERPOLATED_GROUP}) for
     * interpolated groups.
     */
    public static final String DEF_INTERPOLATED_GROUP = "-interpolated";

    /**
     * Default group element separator; {@value #DEF_SEPARATOR}.
     */
    public static final String DEF_SEPARATOR = "/";

    /**
     * Base interpolation level. Any interpolation level greater than this value
     * ({@value #BASE_LEVEL}) is considered to be a valid interpolation level.
     */
    public static final int BASE_LEVEL = 0;

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

    /**
     * Create a storage dataset name using the group and data set name and an
     * interpolation level. Any interpolation level less than or equal to zero
     * generates the base dataset name with no interpolation. Any interpolation
     * levels greater than zero are considered to be decimated levels of the
     * original data.
     * <table>
     * <tr>
     * <th>base group</th>
     * <th>dataset</th>
     * <th>interpolation</th>
     * <th>result</th>
     * </tr>
     * <tr>
     * <td>null</td>
     * <td>null</td>
     * <td>-1</td>
     * <td>/Data</td>
     * </tr>
     * </tr>
     * <tr>
     * <td>null</td>
     * <td>null</td>
     * <td>0</td>
     * <td>/Data</td>
     * </tr>
     * <tr>
     * <td>null</td>
     * <td>null</td>
     * <td>4</td>
     * <td>/Data-interpolated/4</td>
     * </tr>
     * <tr>
     * <td>/data/group</td>
     * <td>null</td>
     * <td>0</td>
     * <td>/data/group/Data</td>
     * </tr>
     * <tr>
     * <td>/data/group</td>
     * <td>null</td>
     * <td>3</td>
     * <td>/data/group/Data-interpolated/3</td>
     * </tr>
     * <tr>
     * <td>/data/group</td>
     * <td>dsname</td>
     * <td>-1</td>
     * <td>/data/group/dsname</td>
     * </tr>
     * <tr>
     * <td>/data/group</td>
     * <td>dsname</td>
     * <td>2</td>
     * <td>/data/group/dsname-interpolated/2</td>
     * </tr>
     * </table>
     * 
     * @param groupName
     *            The group name this data set belongs to. If null, an empty
     *            group name is generated.
     * @param baseDataSet
     *            Data set name The dataset name. This name and the
     *            {@link DEF_INTERPOLATED_GROUP} are used to create the
     *            interpolated suffix for the group name. If null and
     *            interpolation is requested, a default value
     *            {@link DEF_DATASET_NAME} will be used.
     * @param interpolatedLevel
     *            The interpolation level data set numeric identifier.
     * @return The generated fully qualified dataset name.
     */
    public static String createDataSetName(String groupName,
            String baseDataSet, int interpolatedLevel) {
        boolean interpolated = isInterpolated(interpolatedLevel);
        StringBuilder interpolatedGroup = new StringBuilder(createGroupName(
                groupName, baseDataSet, interpolated));

        interpolatedGroup.append(DEF_SEPARATOR);
        if (interpolated) {
            interpolatedGroup.append(String.valueOf(interpolatedLevel));
        } else {
            if (baseDataSet != null) {
                interpolatedGroup.append(baseDataSet);
            } else {
                interpolatedGroup.append(DEF_DATASET_NAME);
            }
        }
        return interpolatedGroup.toString();
    }

    /**
     * Create a hierarchical group name, given a base group name, a dataset name
     * and if interpolated levels are being created. If interpolation is not
     * requested then the base group name is returned unchanged or if null an
     * empty string is returned. When interpolation is requested, the dataset
     * name is appended with an interpolation identifer to create an
     * interpolation level group name.
     * <table>
     * <tr>
     * <th>base group</th>
     * <th>dataset</th>
     * <th>interpolation</th>
     * <th>result</th>
     * </tr>
     * <tr>
     * <td>null</td>
     * <td>null</td>
     * <td>false</td>
     * <td>zero length string</td>
     * </tr>
     * <tr>
     * <td>null</td>
     * <td>null</td>
     * <td>true</td>
     * <td>/Data-interpolated</td>
     * </tr>
     * <tr>
     * <td>/data/group</td>
     * <td>null</td>
     * <td>false</td>
     * <td>/data/group</td>
     * </tr>
     * <tr>
     * <td>/data/group</td>
     * <td>null</td>
     * <td>true</td>
     * <td>/data/group/Data-interpolated</td>
     * </tr>
     * <tr>
     * <td>/data/group</td>
     * <td>dsname</td>
     * <td>false</td>
     * <td>/data/group</td>
     * </tr>
     * <tr>
     * <td>/data/group</td>
     * <td>dsname</td>
     * <td>true</td>
     * <td>/data/group/dsname-interpolated</td>
     * </tr>
     * </table>
     * 
     * @param groupName
     *            The base group name.
     * @param baseDataSet
     *            Data set name The dataset name. This name and the
     *            {@link #DEF_INTERPOLATED_GROUP} are used to create the
     *            interpolated suffix for the group name. If null and
     *            interpolation is requested, a default value
     *            {@link #DEF_DATASET_NAME} will be used.
     * @param interpolatedLevel
     *            Create an interpolated group name.
     * @return The generated group name.
     */
    public static String createGroupName(String groupName, String baseDataSet,
            boolean interpolated) {
        StringBuilder interpolatedGroup = new StringBuilder(256);
        if (groupName != null) {
            interpolatedGroup.append(groupName);
        }
        if (interpolated) {
            interpolatedGroup.append(DEF_SEPARATOR);
            if (baseDataSet != null) {
                if (baseDataSet.length() > BASE_LEVEL) {
                    interpolatedGroup.append(baseDataSet);
                } else {
                    interpolatedGroup.append(DEF_DATASET_NAME);
                }
            } else {
                interpolatedGroup.append(DEF_DATASET_NAME);
            }
            interpolatedGroup.append(DEF_INTERPOLATED_GROUP);
        }
        return interpolatedGroup.toString();
    }

    /**
     * Is the specified interpolation greater than the {@link BASE_LEVEL}?
     * 
     * @param interpolatedLevel
     *            An interpolation level.
     * @return
     */
    public static boolean isInterpolated(int interpolatedLevel) {
        return (interpolatedLevel > BASE_LEVEL);
    }
}
