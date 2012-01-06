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
package com.raytheon.uf.edex.decodertools.bufr.packets;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRTableB;
import com.raytheon.uf.edex.decodertools.bufr.io.BUFRBitInputStream;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080214            862 jkorman     BUFRMOS implementation changes.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRNumericPacket implements IBUFRDataPacket {
    private BUFRDescriptor referenceDescriptor = null;

    private final Long value;

    private final String units;

    private final boolean missingData;

    // This array is used when decoding compressed data.
    private Long[] arrayValues = null;

    public BUFRNumericPacket(Long value, String units,
            BUFRDescriptor refDescriptor) {
        this.value = value;
        missingData = (this.value == null);
        this.units = units;
        referenceDescriptor = refDescriptor;
    }

    @Override
    public String getUnits() {
        return units;
    }

    @Override
    public Object getValue() {
        return value;
    }

    /**
     * Is the value for this packet missing.
     * 
     * @return Is the value missing.
     */
    public boolean isMissing() {
        return missingData;
    }

    public String toString() {
        return value + ":" + units;
    }

    public BUFRDescriptor getReferencingDescriptor() {
        return referenceDescriptor;
    }

    /**
     * Set the value array built when decoding compressed data.
     * 
     * @param values
     */
    public void setArrayValues(Long[] values) {
        arrayValues = values;
    }

    /**
     * Get the value array built when decoding compressed data.
     * 
     * @param values
     */
    public Long[] getArrayValues() {
        return arrayValues;
    }

    public void readCompressed(int numSubsets, BUFRBitInputStream bitStrm) {

        long refValue = ((BUFRTableB) referenceDescriptor).getReferenceValue();

        arrayValues = new Long[numSubsets];
        int numBits = (int) bitStrm.read(6);

        if (numBits == 0) {
            for (int i = 0; i < arrayValues.length; i++) {
                arrayValues[i] = value;
            }
        } else {
            long missingMask = 0L;
            for (int i = 0; i < numBits; i++) {
                missingMask <<= 1;
                missingMask |= 1;
            }

            // value holds the "base" value for this element
            for (int i = 0; i < arrayValues.length; i++) {
                long incValue = bitStrm.read(numBits);
                Long v = null;
                if ((incValue & missingMask) == missingMask) {
                    v = new Long(IDecoderConstants.VAL_MISSING);
                } else {
                    v = new Long(refValue + value + incValue);
                }
                arrayValues[i] = v;
            }
        }
    }

    /**
     * Get the count of data subset elements.
     * @return The number of data subset elements.
     */
    public int getSubsetDataCount() {
        int count = -1;
        if (arrayValues != null) {
            count = arrayValues.length;
        }
        return count;
    }
    
    /**
     * 
     * @return
     */
    public IBUFRDataPacket getSubsetData(int index) {
        IBUFRDataPacket packet = null;
        if ((arrayValues != null) && (arrayValues.length > 0)) {
            packet = new BUFRNumericPacket(arrayValues[index], units,
                    referenceDescriptor);
        }
        return packet;
    }

}
