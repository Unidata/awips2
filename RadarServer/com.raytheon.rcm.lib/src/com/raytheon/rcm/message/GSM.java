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

public class GSM extends Message {
	public static final int OP_MODE_MAINTENANCE = 0;
	public static final int OP_MODE_CLEAR_AIR = 1;
	public static final int OP_MODE_STORM = 2;
	
	public int opMode;
	public int rdaOpStatus;
	public int vcp;
	public int[] cuts; // in tenths of degrees
	public int rdaStatus;
	public int rdaAlarms;
	public int dataAvailability; // "DTE"
	public int rpgOpStatus;
	public int rpgAlarms;
	public int rpgStatus;
	public int rpgNarrowbandStatus;
	public int rcc;
	public int productAvailability;
	public int superResCuts;
	public int rdaVersion;
	public int rdaChannel;
	public int rpgVersion;
	
	public static GSM decode(byte[] msg) {
		return (GSM) MD.decode(msg);
	}

	protected void decodeBlock(int index, ByteBuffer buf) {
		if (index != 1)
			return;
		opMode = buf.getShort();
		rdaOpStatus = buf.getShort();
		vcp = buf.getShort();
		int nCuts = buf.getShort();
		cuts = new int[nCuts];
		for (int i = 0; i < 20; ++i) {
			if (i < cuts.length)
				cuts[i] = buf.getShort();
			else
				buf.getShort();
		}
		rdaStatus = buf.getShort();
		rdaAlarms = buf.getShort();
		dataAvailability = buf.getShort();
		rpgOpStatus = buf.getShort();
		rpgAlarms = buf.getShort();
		rpgStatus = buf.getShort();
		rpgNarrowbandStatus = buf.getShort();
		rcc = buf.getShort();
		productAvailability = buf.getShort();
		superResCuts = buf.getShort();
		buf.position(buf.position() + 4);
		rdaVersion = buf.getShort();
		rdaChannel = buf.getShort();
		buf.position(buf.position() + 4);
		rpgVersion = buf.getShort();
	}	
}
