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
package com.raytheon.uf.edex.plugin.npp.viirs;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * VIIRS Message Data object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2011             mschenke    Initial creation
 * Feb 21, 2012 #30        mschenke    Changed missing value to float
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class VIIRSMessageData {

    @DynamicSerializeElement
    private Object rawData;

    @DynamicSerializeElement
    private float scale;

    @DynamicSerializeElement
    private float offset;

    @DynamicSerializeElement
    private float[] missingValues;

    @DynamicSerializeElement
    private String unitString;

    /**
     * @return the rawData
     */
    public Object getRawData() {
        return rawData;
    }

    /**
     * @param rawData
     *            the rawData to set
     */
    public void setRawData(Object rawData) {
        this.rawData = rawData;
    }

    /**
     * @return the scale
     */
    public float getScale() {
        return scale;
    }

    /**
     * @param scale
     *            the scale to set
     */
    public void setScale(float scale) {
        this.scale = scale;
    }

    /**
     * @return the offset
     */
    public float getOffset() {
        return offset;
    }

    /**
     * @param offset
     *            the offset to set
     */
    public void setOffset(float offset) {
        this.offset = offset;
    }

    /**
     * @return the missingValue
     */
    public float[] getMissingValues() {
        return missingValues;
    }

    /**
     * @param missingValue
     *            the missingValue to set
     */
    public void setMissingValues(float[] missingValues) {
        this.missingValues = missingValues;
    }

    /**
     * @return the unitString
     */
    public String getUnitString() {
        return unitString;
    }

    /**
     * @param unitString
     *            the unitString to set
     */
    public void setUnitString(String unitString) {
        this.unitString = unitString;
    }

}
