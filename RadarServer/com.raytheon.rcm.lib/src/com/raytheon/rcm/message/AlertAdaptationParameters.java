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


public class AlertAdaptationParameters extends Message {
	public class ParameterSpec {
		public int group;
		public int category;
		public short[] thresholds;
		public short productCode;
	}
	
	public ParameterSpec[] parameterSpecs;
	
	public static ParameterSpec[] decode(byte[] msg) {
		return ((AlertAdaptationParameters) MD.decode(msg)).parameterSpecs;
	}

	@Override
	protected void decodeBlocks(ByteBuffer buf) {
		/* Unlike other messages that are received from the RPG (but like
		 * the messages that are sent), AAP's block length seems to include
		 * the divider and block size.
		 * 
		 * psv_process_wx_alerts.c:
		 *   aap->length = ALERT_THRESHOLD_CATEGORIES * 20 + 4;
		 */
		int nBlocks = buf.getShort();
		// Start at 1 because the header block counts as 1
		for (int i = 1; i < nBlocks; ++i) {
			int divider = buf.getShort();
			Message.checkFormat(divider == -1, "expected block divider");
			int blockSize = buf.getShort();
			blockSize = Math.min(blockSize, buf.limit() - buf.position());
			ByteBuffer block = buf.duplicate();
			block.limit(block.position() + blockSize);
			decodeBlock(i, block);
			buf.position(buf.position() + blockSize);			
		}
	}

	@Override
	protected void decodeBlock(int index, ByteBuffer buf) {
		if (index == 1) {
			int nSpecs = buf.remaining() / 20;
			parameterSpecs = new ParameterSpec[nSpecs];			
			for (int i = 0; i < nSpecs; ++i) {
				ParameterSpec spec = new ParameterSpec();
				spec.group = buf.getShort();
				spec.category = buf.getShort();
				int nThresholds = buf.getShort();
				checkFormat(nThresholds <= 6, "Unexpected threshold count");				
				spec.thresholds = new short[nThresholds];
				for (int j = 0; j < 6; ++j) {
					short v = buf.getShort();
					if (j < spec.thresholds.length)
						spec.thresholds[j] = v;
				}								
				spec.productCode = buf.getShort();
				
				parameterSpecs[i] = spec;
			}
		}
	}

}
