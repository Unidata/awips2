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

import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A float data container
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
public class FloatPointDataObject extends AbstractPointDataObject<float[]> {

    @DynamicSerializeElement
    @XmlElement
    protected float[] floatData;

    public FloatPointDataObject() {
        super(null, (ParameterDescription) null, 1);
    }

    public FloatPointDataObject(PointDataContainer container,
            ParameterDescription description, int dims) {
        super(container, description, dims);
        this.floatData = new float[0];
    }

    public FloatPointDataObject(PointDataContainer container,
            FloatDataRecord rec) {
        super(container, rec);
        this.floatData = rec.getFloatData();
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
     * @see com.raytheon.uf.common.pointData.IPointDataObject#getData()
     */
    @Override
    public float[] getData() {
        int curSz = container.getCurrentSz();
        if (this.dimensions > 1)
            curSz *= this.description.getDimensionAsInt();

        if (curSz < floatData.length) {
            float[] newData = new float[curSz];
            System.arraycopy(floatData, 0, newData, 0, curSz);
            return newData;
        } else if (curSz == floatData.length) {
            return floatData;
        } else {
            throw new IllegalStateException(
                    "Data length is greater than array size");
        }
    }

    public void setData(float[] data) {
        this.floatData = data;
    }

    @Override
    public void resize(int sz) {
        float[] newData = new float[sz];
        System.arraycopy(floatData, 0, newData, 0, floatData.length);
        int fill = description.getFillValue().intValue();
        for (int i = this.floatData.length; i < newData.length; i++) {
            newData[i] = fill;
        }

        this.floatData = newData;
    }

    @Override
    public IDataRecord getRecord() {
        FloatDataRecord idr = null;
        if (this.dimensions == 1) {
            idr = new FloatDataRecord(getParameterName(), "", getData());
        } else {
            float[] d = getData();
            idr = new FloatDataRecord(getParameterName(), "", d, 2, new long[] {
                    this.description.getDimensionAsInt(),
                    d.length / this.description.getDimensionAsInt() });
            long[] maxSizes = new long[] { 0,
                    this.description.getDimensionAsInt() };
            idr.setMaxSizes(maxSizes);
            idr.setMaxChunkSize(STORAGE_CHUNK_SIZE);
        }

        if (this.description.getFillValue() != null) {
            idr.setFillValue(new Float(this.description.getFillValue()
                    .floatValue()));
        }
        setProperties(idr);
        return idr;
    }

    @Override
    public Number getNumber(int idx) {
        // Assumes range checking is provided by the view
        return new Float(floatData[idx]);
    }

    public float getFloat(int idx) {
        // Assumes range checking is provided by the view
        return floatData[idx];
    }

    public void setFloat(int idx, float val) {
        // Assumes range checking is provided by the view
        floatData[idx] = val;
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
        this.floatData[idx] = number.floatValue();
    }

    @Override
    public void combine(AbstractPointDataObject<?> obj) {
        FloatPointDataObject floatP = (FloatPointDataObject) obj;
        float[] d = new float[floatP.floatData.length + floatData.length];
        System.arraycopy(this.floatData, 0, d, 0, this.floatData.length);
        System.arraycopy(floatP.floatData, 0, d, this.floatData.length,
                floatP.floatData.length);
        this.floatData = d;
    }

}
