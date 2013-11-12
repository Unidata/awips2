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
package com.raytheon.uf.common.pointdata.elements;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A long data container
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class LongPointDataObject extends AbstractPointDataObject<long[]> {
    @DynamicSerializeElement
    @XmlElement
    protected long[] longData;

    public LongPointDataObject() {
        super(null, (ParameterDescription) null, 1);
    }

    public LongPointDataObject(PointDataContainer container,
            ParameterDescription description, int dimensions) {
        super(container, description, dimensions);
        this.longData = new long[0];
    }

    public LongPointDataObject(PointDataContainer container, LongDataRecord rec) {
        super(container, rec);
        this.longData = rec.getLongData();
    }

    /**
     * @return the longData
     */
    public long[] getLongData() {
        return longData;
    }

    /**
     * @param longData
     *            the longData to set
     */
    public void setLongData(long[] longData) {
        this.longData = longData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.pointData.IPointDataObject#getData()
     */
    @Override
    public long[] getData() {
        int curSz = container.getCurrentSz();
        if (curSz < longData.length) {
            long[] newData = new long[curSz];
            System.arraycopy(longData, 0, newData, 0, curSz);
            return newData;
        } else if (curSz == longData.length) {
            return longData;
        } else {
            throw new IllegalStateException(
                    "Data length is greater than array size");
        }
    }

    public void setData(long[] data) {
        this.longData = data;
    }

    @Override
    public void resize(int sz) {
        long[] newData = new long[sz];
        System.arraycopy(longData, 0, newData, 0, longData.length);
        int fill = description.getFillValue().intValue();
        for (int i = this.longData.length; i < newData.length; i++) {
            newData[i] = fill;
        }

        this.longData = newData;
    }

    @Override
    public IDataRecord getRecord() {
        LongDataRecord ldr = null;
        if (this.dimensions == 1) {
            ldr = new LongDataRecord(getParameterName(), "", getData());
        } else {
            long[] d = getData();
            ldr = new LongDataRecord(getParameterName(), "", d, 2, new long[] {
                    this.description.getDimensionAsInt(), d.length });

            long[] maxSizes = new long[] { 0,
                    this.description.getDimensionAsInt() };
            ldr.setMaxSizes(maxSizes);
            ldr.setMaxChunkSize(STORAGE_CHUNK_SIZE);
        }

        if (this.description.getFillValue() != null) {
            ldr.setFillValue(new Long(this.description.getFillValue()
                    .longValue()));
        }

        setProperties(ldr);
        return ldr;
    }

    @Override
    public Number getNumber(int idx) {
        // Assumes range checking is provided by the view
        return new Long(longData[idx]);
    }

    public long getLong(int idx) {
        // Assumes range checking is provided by the view
        return longData[idx];
    }

    public void setLong(int idx, long val) {
        // Assumes range checking is provided by the view
        longData[idx] = val;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.pointData.elements.AbstractPointDataObject#setNumber
     * (int, java.lang.Number)
     */
    @Override
    public void setNumber(int idx, Number number) {
        // Assumes range checking is provided by the view
        this.longData[idx] = number.longValue();
    }

    @Override
    public void combine(AbstractPointDataObject<?> obj) {
        LongPointDataObject intP = (LongPointDataObject) obj;
        long[] d = new long[intP.longData.length + longData.length];
        System.arraycopy(this.longData, 0, d, 0, this.longData.length);
        System.arraycopy(intP.longData, 0, d, this.longData.length,
                intP.longData.length);
        this.longData = d;
    }

}
