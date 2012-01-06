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
package com.raytheon.uf.edex.decodertools.bufr;

import java.nio.ByteBuffer;

import com.raytheon.uf.edex.decodertools.bufr.exceptions.BUFRDecoderException;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class BUFRSection {
    // Assume not valid until proven otherwise!
    private boolean sectionValid = false;

    private byte[] sectionData;

    BUFRSection(ByteBuffer dataBuffer, int size) {
        sectionData = new byte[size];
        dataBuffer.get(sectionData);
    }

    BUFRSection(ByteBuffer dataBuffer) {
        // Mark our current position, we will come back to get the entire
        // section data after we've found the section size.
        dataBuffer.mark();

        try {
            // Read the size data
            sectionData = new byte[getInteger(dataBuffer, 3)];
            // now reset back to the start of the section.
            dataBuffer.reset();

            dataBuffer.get(sectionData);
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new BUFRDecoderException("Out of data in BUFRSection1", e);
        }
    }

    void setValidity(boolean validity) {
        sectionValid = validity;
    }

    public boolean isSectionValid() {
        return sectionValid;
    }

    byte getSectionData(int index) {
        return sectionData[index];
    }

    public byte[] getSectionArray() {
        return sectionData;
    }

    public int getSectionSize() {
        return sectionData.length;
    }

    /**
     * Gets data from the section data.
     * 
     * @param numberOfBytes
     * @param position
     * @return
     */
    public int getInteger(int numberOfBytes, int position) {
        int value = 0;
        if (position + numberOfBytes <= sectionData.length) {
            for (int i = 0; i < numberOfBytes; i++) {
                value <<= 8;
                value |= (sectionData[position + i] & 0xFF);
            }
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }

        return value;
    }

    public static int getInteger(ByteBuffer dataBuffer, int numberOfBytes) {
        int value = 0;
        if (numberOfBytes <= dataBuffer.remaining()) {
            for (int i = 0; i < numberOfBytes; i++) {
                value <<= 8;
                value |= (dataBuffer.get() & 0xFF);
            }
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }

        return value;
    }

    public StringBuilder getStringData(StringBuilder buffer) {
        if (buffer == null) {
            buffer = new StringBuilder();
        }
        return buffer;
    }

    public String toString() {
        return getStringData(null).toString();
    }

}
