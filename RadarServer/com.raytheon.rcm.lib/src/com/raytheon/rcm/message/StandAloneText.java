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

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;

public class StandAloneText {
	public static byte[] decodeTextBytes(byte[] msg) {
		ByteArrayOutputStream result = new ByteArrayOutputStream(msg.length);
		ByteBuffer buf = ByteBuffer.wrap(msg);
		
		/* The format of the blocks differ from other products blocks
		 * that have a block ID and 32-bit length.  Just perform
		 * some simple checking and jump to where the data should be
		 */
		Message.checkFormat(buf.getShort(18) == -1 &&
				buf.getShort(0x78) == -1, "expected block divider");
		
		buf.position(0x78 + 2);
		
		int nPages = buf.getShort() & 0xffff;
		while (nPages-- > 0) {
			while (true) {
				int lineLength = buf.getShort() & 0xffff;
				if (lineLength == 0xffff)
					break;
				result.write(msg, buf.position(), lineLength);
				result.write('\n');
				buf.position(buf.position() + lineLength);
			}
		}
		
		return result.toByteArray();
	}
}
