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

package com.raytheon.uf.common.datastorage.records;

import java.util.Arrays;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 8, 2007              chammack    Initial Creation.
 * 24 Nov 2007        555   garmendariz Added method to check dataset dimensions and override toString
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@DynamicSerialize
public class FloatDataRecord extends AbstractStorageRecord {

    @DynamicSerializeElement
    protected float[] floatData;

    public FloatDataRecord() {

    }

    /**
     * Constructor
     * 
     * @param name
     *            the name of the data
     * @param group
     *            the group inside the file
     * @param floatData
     *            the float data as 1d array
     * @param dimension
     *            the dimension of the data
     * @param sizes
     *            the length of each dimension
     */
    public FloatDataRecord(String name, String group, float[] floatData,
            int dimension, long[] sizes) {
        this.floatData = floatData;
        this.group = group;
        this.dimension = dimension;
        this.sizes = sizes;
        this.name = name;
    }

    /**
     * Convenience constructor for single dimension float data
     * 
     * @param name
     *            name of the data
     * @param group
     *            the group inside the file
     * @param floatData
     *            the one dimensional float data
     */
    public FloatDataRecord(String name, String group, float[] floatData) {
        this(name, group, floatData, 1, new long[] { floatData.length });
    }

    /**
     * @return the floatData
     */
    public float[] getFloatData() {
        return floatData;
    }

    /**
     * @param floatData
     *            the floatData to set
     */
    public void setFloatData(float[] floatData) {
        this.floatData = floatData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.AbstractDataRecord#getDataObject()
     */
    @Override
    public Object getDataObject() {
        return this.floatData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#validateDataSet()
     */
    public boolean validateDataSet() {

        long size = 1;

        for (int i = 0; i < this.dimension; i++) {
            size *= this.sizes[i];
        }

        if (size == this.floatData.length) {
            return true;
        } else {
            return false;
        }

    }

    /**
     * Override toString method to print dimensions
     */
    @Override
    public String toString() {
        return "[dims,data size]=[" + Arrays.toString(sizes) + ","
                + this.floatData.length + "]";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#reduce(int[])
     */
    @Override
    public void reduce(int[] indices) {
        float[] reducedData = new float[indices.length];
        for (int i = 0; i < reducedData.length; i++) {
            if (indices[i] >= 0) {
                reducedData[i] = floatData[indices[i]];
            } else {
                reducedData[i] = -9999;
            }
        }
        this.floatData = reducedData;
        setDimension(1);
        setSizes(new long[] { indices.length });
    }

    @Override
    protected AbstractStorageRecord cloneInternal() {
        FloatDataRecord record = new FloatDataRecord();
        if (floatData != null) {
            record.floatData = Arrays.copyOf(floatData, floatData.length);
        }
        return record;
    }

    @Override
    public int getSizeInBytes() {
        return floatData == null ? 0 : floatData.length * 4;
    }
}
