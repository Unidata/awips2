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
package com.raytheon.rcm.message;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Calendar;

import com.raytheon.rcm.ccmreq.CcmRequestDefinition;

/**
 * Represents the contents of an ORPG Command Control Message.
 *
 * <pre>
 *  SOFTWARE HISTORY
 *
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  2016-05-06   DCS18795    jdynina     Initial version
 * </pre>
 *
 * @author jdynina
 * @version 1.0
 */

public class CCM {

    private static final short MASK = (short) (1 << 13);

    public static byte[] encode(CcmRequestDefinition options) {
        Message msg = new Message();
        msg.messageCode = Message.COMMAND_CONTROL_MESSAGE;
        msg.time = Calendar.getInstance();

        try {
            msg.blocks = new byte[][] { encodeCcmBlock(options) };
        } catch (IOException ie) {
            ie.printStackTrace();
        }

        return msg.encode();
    }

    private static byte[] encodeCcmBlock(CcmRequestDefinition options) throws IOException {

        ByteBuffer buf = ByteBuffer.allocate(3 * 2); // 3 shorts
        short vcpFlag;

        if (options.getRestartVcp()) {
            vcpFlag = (short) 1;
        } else {
            vcpFlag = (short) 0;
        }

        Short vcp = (short) options.getVcp();

        if (vcp > Short.MAX_VALUE) {
            throw new IllegalArgumentException("Vcp Overflow occured");
        }

        if (vcpFlag == 1) {
            vcp = (short) ((short) vcp | MASK);
        }

        buf.putShort(vcp);
        buf.putShort((short) options.getAvsetEnabled());
        buf.putShort((short) options.getSailsCount());

        return buf.array();
    }
}

