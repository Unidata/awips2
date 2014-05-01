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
public class BUFRSection5 extends BUFRSection {
    public static final byte[] BUFR_TRAILER = "7777".getBytes();

    BUFRSection5(ByteBuffer dataBuffer) {
        super(dataBuffer, BUFR_TRAILER.length);

        boolean isValid = BUFR_TRAILER.length == getSectionSize();

        for (int i = 0; i < BUFR_TRAILER.length; i++) {
            isValid &= (BUFR_TRAILER[i] == getSectionData(i));
        }
        setValidity(isValid);
    }

    public StringBuffer getStringData(StringBuffer buffer) {
        if (buffer == null) {
            buffer = new StringBuffer();
        }
        buffer.append(String.format("Section 5: %s\n", "7777"));

        return buffer;
    }

}
