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
 * 
 * Provides an interface to datasets of type ascii String
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
public class StringDataRecord extends AbstractStorageRecord {

    @DynamicSerializeElement
    protected String[] stringData;

    @DynamicSerializeElement
    protected int maxLength;

    public StringDataRecord() {

    }

    /**
     * 
     * @param name
     * @param group
     * @param stringData
     * @param dimension
     * @param sizes
     */
    public StringDataRecord(String name, String group, String[] stringData,
            int dimension, long[] sizes) {
        this.stringData = stringData;
        this.group = group;
        this.dimension = dimension;
        this.sizes = sizes;
        this.name = name;
    }

    /**
     * Convenience constructor for single dimension String data
     * 
     * @param name
     * @param group
     * @param stringData
     */
    public StringDataRecord(String name, String group, String[] stringData) {
        this(name, group, stringData, 1, new long[] { stringData.length });
    }

    /**
     * @return the stringData
     */
    public String[] getStringData() {
        return this.stringData;
    }

    /**
     * @param stringData
     *            the stringData to set
     */
    public void setStringData(String[] stringData) {
        this.stringData = stringData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.AbstractDataRecord#getDataObject()
     */
    @Override
    public Object getDataObject() {
        return this.stringData;
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

        if (size == this.stringData.length) {
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
                + this.stringData.length + "]";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#reduce(int[])
     */
    @Override
    public void reduce(int[] indices) {
        String[] reducedData = new String[indices.length];
        for (int i = 0; i < reducedData.length; i++) {
            if (indices[i] >= 0) {
                reducedData[i] = stringData[indices[i]];
            } else {
                reducedData[i] = null;
            }
        }
        this.stringData = reducedData;
        setDimension(1);
        setSizes(new long[] { indices.length });
    }

    /**
     * @return the maxLength
     */
    public int getMaxLength() {
        return maxLength;
    }

    /**
     * @param maxLength
     *            the maxLength to set
     */
    public void setMaxLength(int maxLength) {
        this.maxLength = maxLength;
    }

    public Object getStorageObject() {
        Object mydata = getStringData();
        int sz = getMaxLength();
        if (sz > 0) {
            String[] data = getStringData();
            mydata = new byte[sz * data.length];
            for (int i = 0; i < data.length; i++) {
                byte[] d = data[i].getBytes();
                byte[] padded = new byte[d.length + 1];
                System.arraycopy(d, 0, padded, 0, d.length);
                padded[padded.length - 1] = '\0';

                System.arraycopy(padded, 0, mydata, i * sz, Math.min(sz,
                        padded.length));

            }
        }

        return mydata;
    }

    @Override
    public int getSizeInBytes() {
        int size = 0;
        if (stringData != null) {
            for (String s : stringData) {
                if (s != null) {
                    size += s.length();
                }
            }
        }
        // for the maxLength field
        size += 4;
        return size;
    }

    @Override
    protected AbstractStorageRecord cloneInternal() {
        StringDataRecord record = new StringDataRecord();
        if (stringData != null) {
            record.stringData = Arrays.copyOf(stringData, stringData.length);
        }
        record.maxLength = maxLength;
        return record;
    }

}
