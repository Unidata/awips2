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

import java.nio.ByteBuffer;

/**
 * Represents the contents of an ORPG Command Parameter Message.
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

public class CPM extends Message {

    public int[] clearAirVcps;
    public int[] precipVcps;

    public int maxSailsCuts;

    public static CPM decode(byte[] msg) {
        return (CPM) MD.decode(msg);
    }

    protected void decodeBlock(int index, ByteBuffer buf) {
        if (index != 1) {
            return;
        }

        int numClearAirVcps = buf.getShort();
        clearAirVcps = new int[numClearAirVcps];

        for (int i = 0; i < numClearAirVcps; ++i) {
            clearAirVcps[i] = buf.getShort();
        }

        int numPrecipVcps = buf.getShort();
        precipVcps = new int[numPrecipVcps];

        for (int i = 0; i < numPrecipVcps; ++i) {
            precipVcps[i] = buf.getShort();
        }

        buf.position(buf.limit() - 2);
        maxSailsCuts = buf.getShort();

    }
}
