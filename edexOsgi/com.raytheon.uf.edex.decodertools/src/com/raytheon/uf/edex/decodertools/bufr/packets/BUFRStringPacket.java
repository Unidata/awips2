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
public class BUFRStringPacket implements IBUFRDataPacket {
    private BUFRDescriptor referenceDescriptor = null;

    private final String value;

    private final String units;

    private boolean missingData;

    // This array is used when decoding compressed data.
    private String[] arrayValues = null;

    public BUFRStringPacket(String value, String units,
            BUFRDescriptor refDescriptor) {
        this.value = value;
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
    public void setArrayValues(String[] values) {
        arrayValues = values;
    }

    /**
     * Get the value array built when decoding compressed data.
     * 
     * @param values
     */
    public String[] getArrayValues() {
        return arrayValues;
    }

    /**
     * Read compressed character data from the data stream.
     * 
     * @param numSubsets
     *            The total number of subsets for this data.
     * @param bitStrm
     *            The bit input stream to read data from.
     */
    public void readCompressed(int numSubsets, BUFRBitInputStream bitStrm) {

        arrayValues = new String[numSubsets];
        int numOctets = (int) bitStrm.read(6);

        if (numOctets == 0) {
            for (int i = 0; i < arrayValues.length; i++) {
                arrayValues[i] = value;
            }
        } else {
            // value holds the "base" value for this element
            for (int i = 0; i < arrayValues.length; i++) {
                byte[] localData = new byte[numOctets];
                Arrays.fill(localData, (byte) 32);
                for (int j = 0; j < numOctets; j++) {
                    long incValue = bitStrm.read(8);
                    localData[j] = (byte) (incValue & 0xFF);
                }
                arrayValues[i] = new String(localData);
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
            packet = new BUFRStringPacket(arrayValues[index], units,
                    referenceDescriptor);
        }
        return packet;
    }

}
