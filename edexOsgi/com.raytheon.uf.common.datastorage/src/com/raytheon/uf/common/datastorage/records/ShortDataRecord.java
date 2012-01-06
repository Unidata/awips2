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
 * Provides an interface to datasets of type short
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
public class ShortDataRecord extends AbstractStorageRecord {

    @DynamicSerializeElement
    protected short[] shortData;

    public ShortDataRecord() {

    }

    /**
     * 
     * @param name
     * @param group
     * @param intData
     * @param dimension
     * @param sizes
     */
    public ShortDataRecord(String name, String group, short[] shortData,
            int dimension, long[] sizes) {
        this.shortData = shortData;
        this.group = group;
        this.dimension = dimension;
        this.sizes = sizes;
        this.name = name;
    }

    /**
     * Convenience constructor for single dimension short data
     * 
     * @param name
     * @param group
     * @param intData
     */
    public ShortDataRecord(String name, String group, short[] shortData) {
        this(name, group, shortData, 1, new long[] { shortData.length });
    }

    /**
     * @return the intData
     */
    public short[] getShortData() {
        return shortData;
    }

    /**
     * @param intData
     *            the intData to set
     */
    public void setShortData(short[] shortData) {
        this.shortData = shortData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.AbstractDataRecord#getDataObject()
     */
    @Override
    public Object getDataObject() {
        return this.shortData;
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

        if (size == this.shortData.length) {
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
                + this.shortData.length + "]";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#reduce(int[])
     */
    @Override
    public void reduce(int[] indices) {
        short[] reducedData = new short[indices.length];
        for (int i = 0; i < reducedData.length; i++) {
            if (indices[i] >= 0) {
                reducedData[i] = shortData[indices[i]];
            } else {
                reducedData[i] = -9999;
            }
        }
        this.shortData = reducedData;
        setDimension(1);
        setSizes(new long[] { indices.length });
    }

    @Override
    protected AbstractStorageRecord cloneInternal() {
        ShortDataRecord record = new ShortDataRecord();
        if (shortData != null) {
            record.shortData = Arrays.copyOf(shortData, shortData.length);
        }
        return record;
    }

    @Override
    public int getSizeInBytes() {
        return shortData == null ? 0 : shortData.length * 2;
    }

}
