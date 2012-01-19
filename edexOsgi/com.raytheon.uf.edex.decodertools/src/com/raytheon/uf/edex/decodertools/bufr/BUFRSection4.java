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

import com.raytheon.uf.edex.decodertools.bufr.io.BUFRBitInputStream;
import com.raytheon.uf.edex.decodertools.bufr.io.BUFRByteArrayBitInputStream;

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
public class BUFRSection4 extends BUFRSection {

    BUFRSection4(ByteBuffer dataBuffer) {
        super(dataBuffer);
        setValidity(true);
    }

    public StringBuilder getStringData(StringBuilder buffer) {
        buffer = super.getStringData(buffer);
        buffer.append(String.format("Section 4: Data Length = %6d\n",
                getSectionSize()));
        return buffer;
    }

    public void display() {
        int brk = 0;
        for (int i = 0; i < getSectionSize(); i++) {
            int n = getSectionData(i) & 0xFF;
            int mask = 0x80;

            while (mask != 0) {
                String s = ((mask & n) > 0) ? "1" : "0";
                System.out.print(s);
                mask >>= 1;
            }
            if (brk++ == 10) {
                System.out.println();
                brk = 0;
            }
        }
        System.out.println();
    }

    /**
     * 
     * @return
     */
    public BUFRBitInputStream getStream() {
        return new BUFRByteArrayBitInputStream(getSectionArray());
    }

}
