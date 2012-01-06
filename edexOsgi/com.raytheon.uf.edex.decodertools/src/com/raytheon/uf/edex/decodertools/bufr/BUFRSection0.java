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
public class BUFRSection0 extends BUFRSection {
    private static final int SECTION_0_LENGTH = 8;

    public static final byte[] BUFR_HDR = "BUFR".getBytes();

    private int documentSize;

    private int documentEdition;

    BUFRSection0(ByteBuffer dataBuffer) {
        super(dataBuffer, 8);
        try {
            documentSize = getInteger(3, 4);

            documentEdition = getInteger(1, 7);
        } catch (ArrayIndexOutOfBoundsException e) {
            throw new BUFRDecoderException("Out of data in BUFRSection0", e);
        }
        boolean isValid = BUFR_HDR.length == SECTION_0_LENGTH;
        isValid = isValid || (documentEdition == 3);
        isValid = isValid || (documentEdition == 4);

        for (int i = 0; i < BUFR_HDR.length; i++) {
            isValid &= (BUFR_HDR[i] == getSectionData(i));
        }
        setValidity(isValid);
    }

    public int getDocumentSize() {
        return documentSize;
    }

    public BUFRSection1 getSection1(ByteBuffer dataBuffer) {
        BUFRSection1 section = null;
        switch (documentEdition) {
        case 1: {
            section = new BUFRSection1ed1(dataBuffer);
            break;
        }
        case 2: {
            section = new BUFRSection1ed2(dataBuffer);
            break;
        }
        case 3: {
            section = new BUFRSection1ed3(dataBuffer);
            break;
        }
        case 4: {
            section = new BUFRSection1ed4(dataBuffer);
            break;
        }
        default: {
            System.out.println("Unknown edition " + documentEdition);
        }
        }
        return section;

    }

    public StringBuffer getStringData(StringBuffer buffer) {
        if (buffer == null) {
            buffer = new StringBuffer();
        }
        buffer.append(String.format(
                "Section 0: Edition = %1d: Document Size = %6d\n",
                documentEdition, documentSize));

        return buffer;
    }

}
