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
 * Provides an interface to datasets of type integer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 8, 2007              chammack    Initial Creation.
 * 24 Nov 2007        555   garmendariz Added method to check dataset dimensions and override toString
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@DynamicSerialize
public class IntegerDataRecord extends AbstractStorageRecord {

    @DynamicSerializeElement
    protected int[] intData;

    public IntegerDataRecord() {

    }

    /**
     * 
     * @param name
     * @param group
     * @param intData
     * @param dimension
     * @param sizes
     */
    public IntegerDataRecord(String name, String group, int[] intData,
            int dimension, long[] sizes) {
        this.intData = intData;
        this.group = group;
        this.dimension = dimension;
        this.sizes = sizes;
        this.name = name;
    }

    /**
     * Convenience constructor for single dimension int data
     * 
     * @param name
     * @param group
     * @param intData
     */
    public IntegerDataRecord(String name, String group, int[] intData) {
        this(name, group, intData, 1, new long[] { intData.length });
    }

    /**
     * @return the intData
     */
    public int[] getIntData() {
        return intData;
    }

    /**
     * @param intData
     *            the intData to set
     */
    public void setIntData(int[] intData) {
        this.intData = intData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.AbstractDataRecord#getDataObject()
     */
    @Override
    public Object getDataObject() {
        return this.intData;
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

        if (size == this.intData.length) {
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
                + this.intData.length + "]";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#reduce(int[])
     */
    @Override
    public void reduce(int[] indices) {
        int[] reducedData = new int[indices.length];
        for (int i = 0; i < reducedData.length; i++) {
            if (indices[i] >= 0) {
                reducedData[i] = intData[indices[i]];
            } else {
                reducedData[i] = -9999;
            }
        }
        this.intData = reducedData;
        setDimension(1);
        setSizes(new long[] { indices.length });
    }

    @Override
    protected AbstractStorageRecord cloneInternal() {
        IntegerDataRecord record = new IntegerDataRecord();
        if (intData != null) {
            record.intData = Arrays.copyOf(intData, intData.length);
        }
        return record;
    }

    @Override
    public int getSizeInBytes() {
        return intData == null ? 0 : intData.length * 4;
    }

}
