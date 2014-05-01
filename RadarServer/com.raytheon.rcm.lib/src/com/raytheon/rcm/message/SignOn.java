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

public class SignOn extends Message {
	public static byte[] encode(int source, int dest, String userPassword, String portPassword, boolean overrideDisconnect) {
		 return Message.encode(Message.SIGN_ON, source, dest, 
				 encodeSignOnBlock(userPassword, portPassword, overrideDisconnect));
	}
	
	public static byte[] encodeSignOnBlock(String userPassword, String portPassword, boolean overrideDisconnect) {
		ByteBuffer buf = ByteBuffer.allocate(7 * 2);
		StringBuilder sb = new StringBuilder(10);
		// TODO: throw exception if passwords too long?
		sb.append(userPassword);
		sb.setLength(6);
		sb.append(portPassword);
		sb.setLength(10);
		String creds = sb.toString().replace('\0', ' ');
		buf.put(creds.getBytes()); // TODO: correct charset
		buf.putShort((short) (overrideDisconnect ? 1 : 0));
		buf.putShort((short) 0); // "spare"
		return buf.array();
	}
	
	// We only need the static utility methods for now
	private SignOn() { }
}
