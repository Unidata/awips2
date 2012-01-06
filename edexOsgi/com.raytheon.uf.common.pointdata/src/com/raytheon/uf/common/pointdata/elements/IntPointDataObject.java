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
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * An integer data container
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
public class IntPointDataObject extends AbstractPointDataObject<int[]> {
    @DynamicSerializeElement
    @XmlElement
    protected int[] intData;

    public IntPointDataObject() {
        super(null, (ParameterDescription) null, 1);
    }

    public IntPointDataObject(PointDataContainer container,
            ParameterDescription description, int dimensions) {
        super(container, description, dimensions);
        this.intData = new int[0];
    }

    public IntPointDataObject(PointDataContainer container,
            IntegerDataRecord rec) {
        super(container, rec);
        this.intData = rec.getIntData();
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
     * @see com.raytheon.uf.common.pointData.IPointDataObject#getData()
     */
    @Override
    public int[] getData() {
        int curSz = container.getCurrentSz();
        if (this.dimensions > 1)
            curSz *= this.description.getDimensionAsInt();

        if (curSz < intData.length) {
            int[] newData = new int[curSz];
            System.arraycopy(intData, 0, newData, 0, curSz);
            return newData;
        } else if (curSz == intData.length) {
            return intData;
        } else {
            throw new IllegalStateException(
                    "Data length is greater than array size");
        }
    }

    public void setData(int[] data) {
        this.intData = data;
    }

    @Override
    public void resize(int sz) {
        int[] newData = new int[sz];
        System.arraycopy(intData, 0, newData, 0, intData.length);
        int fill = description.getFillValue().intValue();
        for (int i = this.intData.length; i < newData.length; i++) {
            newData[i] = fill;
        }

        this.intData = newData;
    }

    @Override
    public IDataRecord getRecord() {
        IntegerDataRecord idr = null;
        if (this.dimensions == 1) {
            idr = new IntegerDataRecord(getParameterName(), "", getData());
        } else {
            int[] d = getData();
            idr = new IntegerDataRecord(getParameterName(), "", d, 2,
                    new long[] { this.description.getDimensionAsInt(),
                            d.length / this.description.getDimensionAsInt() });
            long[] maxSizes = new long[] { 0,
                    this.description.getDimensionAsInt() };
            idr.setMaxSizes(maxSizes);
            idr.setMaxChunkSize(STORAGE_CHUNK_SIZE);
        }

        if (this.description.getFillValue() != null) {
            idr.setFillValue(new Integer(this.description.getFillValue()
                    .intValue()));
        }

        setProperties(idr);
        return idr;
    }

    @Override
    public Number getNumber(int idx) {
        // Assumes range checking is provided by the view
        return new Integer(intData[idx]);
    }

    public int getInt(int idx) {
        // Assumes range checking is provided by the view
        return intData[idx];
    }

    public void setInt(int idx, int val) {
        // Assumes range checking is provided by the view
        intData[idx] = val;
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
        this.intData[idx] = number.intValue();
    }

    @Override
    public void combine(AbstractPointDataObject<?> obj) {
        IntPointDataObject intP = (IntPointDataObject) obj;
        int[] d = new int[intP.intData.length + this.intData.length];
        System.arraycopy(this.intData, 0, d, 0, this.intData.length);
        System.arraycopy(intP.intData, 0, d, this.intData.length,
                intP.intData.length);
        this.intData = d;
    }

}
