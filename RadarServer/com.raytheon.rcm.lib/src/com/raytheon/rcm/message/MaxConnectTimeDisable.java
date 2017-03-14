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

public class MaxConnectTimeDisable extends Message {
	public static byte[] encode(int source, int dest, int minutes) {
		 return Message.encode(Message.MAX_CONNECT_TIME_DISABLE_REQUEST, 
				 source, dest, encodeBlock(minutes));
	}
	
	public static byte[] encodeBlock(int minutes) {
		ByteBuffer buf = ByteBuffer.allocate(3 * 2);
		buf.putShort((short) minutes);
		buf.putShort((short) 0); // "spare"
		buf.putShort((short) 0); // "spare"
		return buf.array();
	}
	
	// We only need the static utility methods for now
	private MaxConnectTimeDisable() { }
}
