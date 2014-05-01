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
import java.util.Calendar;

import com.raytheon.rcm.request.Request;


public class ProductRequest extends Message {
	
	public static byte[] encode(Request request) {
		Request[] requests = { request }; 
		Message msg = new Message();
		msg.messageCode = Message.PRODUCT_REQUEST;
		msg.time = Calendar.getInstance();		
		msg.blocks = encodeRequestBlocks(requests);
		return msg.encode();
	}

	public static byte[] encode(Request[] requests) {
		Message msg = new Message();
		msg.messageCode = Message.PRODUCT_REQUEST;
		msg.time = Calendar.getInstance();		
		msg.blocks = encodeRequestBlocks(requests);
		return msg.encode();
	}
	
	public static byte[][] encodeRequestBlocks(Request[] requests) {
		byte[][] result = new byte[requests.length][];
		int i = 0;
		for (Request req : requests) {
			ByteBuffer buf = ByteBuffer.allocate(14 * 2);
			buf.putShort(req.productCode);
			buf.putShort((short)( (req.highPriority ? 1 << 15 : 0) |
					(req.mapRequested ? 1 << 14 : 0)) );
			buf.putShort(req.sequence);
			buf.putShort(req.count);
			buf.putShort(req.interval);
			if (req.getVolumeScanSelection() == Request.SELECT_SPECIFIC)
				Message.encodeTime(buf, req.getVolumeScanTime());
			else {
				buf.putShort((short) 0); // TODO: ? Awips1 code puts the negative value here too.  So does nbtcp stuff in CODE
				buf.putInt(req.getVolumeScanSelection());
			}
			buf.putShort((short) req.pdw20);
			buf.putShort((short) req.pdw21);
			buf.putShort((short) req.pdw22);
			buf.putShort((short) req.pdw23);
			buf.putShort((short) req.pdw24);
			buf.putShort((short) req.pdw25);			

			result[i++] = buf.array();
		}
		return result;
	}
}
