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
package com.raytheon.uf.viz.radarapps.alertreq.impl;

import java.nio.ByteBuffer;

import com.raytheon.rcm.message.MD;
import com.raytheon.rcm.message.Message;

public class AlertAdaptationParameters extends Message {
	public class ParameterSpec {
		int group;
		int category;
		short[] thresholds;
		short productCode;
	}
	
	public ParameterSpec[] parameterSpecs;
	
	public static ParameterSpec[] decode(byte[] msg) {
		return ((AlertAdaptationParameters) MD.decode(msg)).parameterSpecs;
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
