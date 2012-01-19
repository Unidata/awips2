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
import java.util.BitSet;
import java.util.Calendar;

public class AlertRequest {
	public static class Threshold {
		short category;
		short threshold;
		boolean requestProduct;
		
		public Threshold(short category, short threshold, boolean requestProduct) {
			this.category = category;
			this.threshold = threshold;
			this.requestProduct = requestProduct;
		}
	}
	
	public static byte[] encode(int area, Threshold[] thresholds, BitSet boxBits) {
		Message msg = new Message();
		msg.messageCode = Message.ALERT_REQUEST;
		msg.time = Calendar.getInstance();		
		msg.blocks = new byte[][] { encodeAlertBlock(area, thresholds, boxBits) };
		return msg.encode();
	}

	private static byte[] encodeAlertBlock(int area, Threshold[] thresholds,
			BitSet boxBits) {
		/*Message.checkFormat(boxBits.size() == 58 * 58,
				"Alert request box bits must be 58 x 58.");*/
		Message.checkFormat(thresholds.length <= 10,
			"Exceeded maximum of 10 threshold categories in an alert request.");

		ByteBuffer buf = ByteBuffer.allocate(2 + 2 + // area and count 
				thresholds.length * 6 + // threshold definitions
				58 * 4 * 2); // 58 rows of four shorts
		buf.putShort((short) area);
		buf.putShort((short) thresholds.length);
		for (Threshold threshold : thresholds) {
			buf.putShort(threshold.category);
			buf.putShort(threshold.threshold);
			buf.putShort((short) (threshold.requestProduct ? 1 : 0));
		}
		
		int srcFlagIndex = 0;
		for (int row = 0; row < 58; ++row) {
			short word;
			// First 3 * 16 = 48 bits
			for (int wordIndex = 0; wordIndex < 3; ++wordIndex) {
				word = 0;
				// The first/leftmost flag is stored in the most significant bit
				for (int destBitIndex = 15; destBitIndex >= 0; --destBitIndex)
					word |= (int)(boxBits.get(srcFlagIndex++) ? 1 : 0) << destBitIndex;
				buf.putShort(word);
			}
			// Last 10 usable bits + 6 clear bits
			word = 0;
			for (int destBitIndex = 15; destBitIndex >= 6; --destBitIndex)
				word |= (int)(boxBits.get(srcFlagIndex++) ? 1 : 0) << destBitIndex;
			buf.putShort(word);
		}
		
		return buf.array();
	}
}
