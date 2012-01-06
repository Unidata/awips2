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

import java.util.Arrays;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.io.BUFRBitInputStream;

/**
 *  
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080327            382 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRUnknownPacket implements IBUFRDataPacket {
    private BUFRDescriptor referenceDescriptor = null;

    private final Long value;

    private final String units = "UNKNOWN";

    private boolean missingData = false;
    
    // This array is used when decoding compressed data.
    private Long[] arrayValues = null;

    public BUFRUnknownPacket(Long value, BUFRDescriptor refDescriptor) {
        this.value = value;
        referenceDescriptor = refDescriptor;
    }
   
    /**
     * 
     */
    @Override
    public String getUnits() {
        return units;
    }

    /**
     * 
     */
    @Override
    public Object getValue() {
        return value;
    }

    /**
     * Always return false.
     * 
     * @return Is the value missing.
     */
    public boolean isMissing() {
        return missingData;
    }

    /**
     * Create a string representation of this class.
     * @return The String representation of this class.
     */
    public String toString() {
        return value + ":" + units;
    }

    /**
     * 
     */
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

    /**
     * 
     */
    public void readCompressed(int numSubsets, BUFRBitInputStream bitStrm) {

        arrayValues = new Long[numSubsets];
        Arrays.fill(arrayValues, -1);
        int numBits = (int) bitStrm.read(6);

        // We're not going to actually deal with the unknown data, just read it
        // out of the stream to make things right.
        for(int i = 0;i < numSubsets;i++) {
            bitStrm.read(numBits);
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
     * Get the packet at a specified index.
     * @return The packet, or null if the index was out of range.
     */
    public IBUFRDataPacket getSubsetData(int index) {
        IBUFRDataPacket packet = null;
        if ((arrayValues != null) && (arrayValues.length > 0)) {
            packet = new BUFRUnknownPacket(arrayValues[index],referenceDescriptor);
        }
        return packet;
    }
    
}
