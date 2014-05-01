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
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.datastorage.StorageProperties;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Defines an abstract dataset
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 8, 2007              chammack    Initial Creation.
 * Dec 31, 2008             chammack    Added correlation object
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@DynamicSerialize
public abstract class AbstractStorageRecord implements IDataRecord {

    @DynamicSerializeElement
    protected String name;

    @DynamicSerializeElement
    protected int dimension;

    @DynamicSerializeElement
    protected long[] sizes;

    @DynamicSerializeElement
    protected long[] maxSizes;

    @DynamicSerializeElement
    protected StorageProperties props;

    @DynamicSerializeElement
    protected long[] minIndex;

    @DynamicSerializeElement
    protected String group;

    @DynamicSerializeElement
    protected Map<String, Object> dataAttributes;

    @DynamicSerializeElement
    protected Number fillValue;

    @DynamicSerializeElement
    protected int maxChunkSize;

    /**
     * An arbitrary object that goes along for the ride that can be used for
     * correlation if errors occur
     */
    protected Object correlationObject;

    public void setIntSizes(int[] sizes) {
        long[] longSizes = new long[sizes.length];
        for (int i = 0; i < sizes.length; i++) {
            longSizes[i] = sizes[i];
        }
        this.setSizes(longSizes);
    }

    public StorageProperties getProperties() {
        return this.props;
    }

    public void setMinIndex(long[] minIndex) {
        this.minIndex = minIndex;
    }

    public long[] getMinIndex() {
        return minIndex;
    }

    public void setProperties(StorageProperties props) {
        this.props = props;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#getDimension()
     */
    public int getDimension() {
        return dimension;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#setDimension(int)
     */
    public void setDimension(int dimensions) {
        this.dimension = dimensions;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#getName()
     */
    public String getName() {
        return name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.storage.records.IDataRecord#setName(java.lang.String)
     */
    public void setName(String name) {
        this.name = name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#getSizes()
     */
    public long[] getSizes() {
        return sizes;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#setSizes(int[])
     */
    public void setSizes(long[] sizes) {
        this.sizes = sizes;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.storage.records.IDataRecord#getDataObject()
     */
    public abstract Object getDataObject();

    /**
     * @return the group
     */
    public String getGroup() {
        return group;
    }

    /**
     * @param group
     *            the group to set
     */
    public void setGroup(String group) {
        this.group = group;
    }

    /**
     * @return the correlationObject
     */
    public Object getCorrelationObject() {
        return correlationObject;
    }

    /**
     * @param correlationObject
     *            the correlationObject to set
     */
    public void setCorrelationObject(Object correlationObject) {
        this.correlationObject = correlationObject;
    }

    /**
     * @return the dataAttributes
     */
    public Map<String, Object> getDataAttributes() {
        return dataAttributes;
    }

    /**
     * @param dataAttributes
     *            the dataAttributes to set
     */
    public void setDataAttributes(Map<String, Object> dataAttributes) {
        this.dataAttributes = dataAttributes;
    }

    /**
     * @return the fillValue
     */
    public Number getFillValue() {
        return fillValue;
    }

    /**
     * @param fillValue
     *            the fillValue to set
     */
    public void setFillValue(Number fillValue) {
        this.fillValue = fillValue;
    }

    /**
     * @return the maxSizes
     */
    public long[] getMaxSizes() {
        return maxSizes;
    }

    /**
     * @param maxSizes
     *            the maxSizes to set
     */
    public void setMaxSizes(long[] maxSizes) {
        this.maxSizes = maxSizes;
    }

    /**
     * @return the maxChunkSize
     */
    public int getMaxChunkSize() {
        return maxChunkSize;
    }

    /**
     * @param maxChunkSize
     *            the maxChunkSize to set
     */
    public void setMaxChunkSize(int maxChunkSize) {
        this.maxChunkSize = maxChunkSize;
    }

    @Override
    public IDataRecord clone() {
        AbstractStorageRecord record = cloneInternal();
        record.name = name;
        record.dimension = dimension;
        if (sizes != null) {
            record.sizes = Arrays.copyOf(sizes, sizes.length);
        }
        if (maxSizes != null) {
            record.maxSizes = Arrays.copyOf(maxSizes, maxSizes.length);
        }
        if (props != null) {
            record.props = props.clone();
        }
        if (minIndex != null) {
            record.minIndex = Arrays.copyOf(minIndex, minIndex.length);
        }
        record.group = group;
        if (dataAttributes != null) {
            record.dataAttributes = new HashMap<String, Object>(dataAttributes);
        }
        record.fillValue = fillValue;
        record.maxChunkSize = maxChunkSize;
        record.correlationObject = correlationObject;
        return record;
    }

    /**
     * Create a new Record Object and clone/copy all members of the object where
     * possibly, do not just set references
     * 
     * @return cloned record
     */
    protected abstract AbstractStorageRecord cloneInternal();

    public StorageProperties getProps() {
        return props;
    }

    public void setProps(StorageProperties props) {
        this.props = props;
    }

}
