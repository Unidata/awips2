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

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.ParameterDescription;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A generic point data object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 08, 2009           chammack    Initial creation
 * Dec 02, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class AbstractPointDataObject<A> {

    static final int STORAGE_CHUNK_SIZE = 1024;

    transient PointDataContainer container;

    @DynamicSerializeElement
    int dimensions;

    @DynamicSerializeElement
    ParameterDescription description;

    AbstractPointDataObject() {
        this.dimensions = 1;
    }

    AbstractPointDataObject(PointDataContainer container,
            ParameterDescription description, int dimensions) {
        this.description = description;
        this.container = container;
        this.dimensions = dimensions;
    }

    AbstractPointDataObject(PointDataContainer container, IDataRecord rec) {
        this.container = container;
        this.description = new ParameterDescription();
        this.description.setParameterName(rec.getName());
        this.dimensions = rec.getDimension();
        if (rec.getDataAttributes() != null) {
            this.description.setUnit((String) rec.getDataAttributes().get(
                    "UNIT"));
        }

        if (rec.getFillValue() != null) {
            this.description.setFillValue(rec.getFillValue().doubleValue());
        }

        if (this.dimensions > 1) {
            long sz = rec.getSizes()[0];
            this.description.setDimensionAsInt((int) sz);
        }
    }

    public abstract A getData();

    public abstract void resize(int sz);

    public abstract IDataRecord getRecord();

    public String getParameterName() {
        return this.description.getParameterName();
    }

    public abstract Number getNumber(int idx);

    public abstract void setNumber(int idx, Number number);

    public abstract void combine(AbstractPointDataObject<?> obj);

    protected void setProperties(IDataRecord rec) {
        if (description.getUnit() != null) {
            Map<String, Object> attribs = new HashMap<String, Object>();
            attribs.put("UNIT", description.getUnit());
            rec.setDataAttributes(attribs);
        }
    }

    /**
     * @return the description
     */
    public ParameterDescription getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(ParameterDescription description) {
        this.description = description;
    }

    /**
     * @return the dimensions
     */
    public int getDimensions() {
        return dimensions;
    }

    /**
     * @param dimensions
     *            the dimensions to set
     */
    public void setDimensions(int dimensions) {
        this.dimensions = dimensions;
    }

    public PointDataContainer getContainer() {
        return container;
    }

    public void setContainer(PointDataContainer container) {
        this.container = container;
    }

}
