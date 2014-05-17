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

import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.Calendar;
import java.util.Properties;

public class RequestResponse extends Message {
	public static final int NO_SUCH_MESSAGE_CODE = 0x80000000;
	public static final int NO_SUCH_PRODUCT_CODE = 0x40000000;
	public static final int NOT_GENERATED = 0x20000000;
	public static final int OTR_PROCESS_FAULTED = 0x10000000;
	public static final int NARROWBAND_LOADSHED = 0x08000000;
	public static final int ILLEGAL_REQUEST = 0x04000000;
	public static final int RPG_MEMORY_LOADSHED = 0x02000000;
	public static final int RPG_CPU_LOADSHED = 0x01000000;
	public static final int SLOT_UNAVAILABLE = 0x00800000;
	public static final int TASK_FAILURE= 0x00400000;
	public static final int TASK_UNAVAILABLE = 0x00200000;
	public static final int AVAILABLE_NEXT_SCAN = 0x00100000;
	public static final int MOMENT_DISABLED = 0x00080000;
	public static final int INVALID_PASSWORD = 0x00040000;
	//public static final int <unused> = 0x00020000;
	public static final int ABORTED_SCAN = 0x00010000;
	public static final int INVALID_PRODUCT_PARAMETERS = 0x00008000;
	public static final int DATA_SEQUENCE_ERROR = 0x00004000;
	public static final int TASK_SELF_TERMINATED = 0x00002000;
	
	public int errorCode;
	public int sequence;
	public int productCode;
	public int elevationAngle;
	public Calendar volumeScanTime;
	
	static private Properties messages;
	static {
		try {
			messages = new Properties();
			InputStream ins = RequestResponse.class.getResourceAsStream("prrMessages.txt");
			if (ins != null) {
				try {
					messages.load(ins);
				} finally {
					ins.close();
				}
			}
		} catch (Exception e) {
			// nothing
		}
	}
	
	public static RequestResponse decode(byte[] msg) {
		RequestResponse result = new RequestResponse();
		result.decode(ByteBuffer.wrap(msg));
		return result;
	}

	@Override
	protected void decodeBlock(int index, ByteBuffer buf) {
		if (index != 1)
			return;
		errorCode = buf.getInt();
		sequence = buf.getShort();
		productCode = buf.getShort();
		elevationAngle = buf.getShort();
		volumeScanTime = Message.decodeTime(buf);
		/* Remaining bytes are documented as "spares". */
	}

	
	
	public String getErrorMessages() {
		if (errorCode != 0) {
			StringBuilder result = new StringBuilder();
			if (messages != null) {
				for (int i = 31; i >= 0; --i) {
					if ((errorCode & (1 << i)) != 0) {
						if (result.length() > 0)
							result.append(", ");
						
						String msg = messages.getProperty(Integer.toString(i));
						if (msg != null)
							result.append(msg);
						else
							result.append("Unknown Error");
					}
				}
			}
			return result.toString();
		}
		return "";
	}
}
