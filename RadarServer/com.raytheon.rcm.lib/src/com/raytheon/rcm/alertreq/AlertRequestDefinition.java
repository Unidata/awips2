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
package com.raytheon.rcm.alertreq;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

public class AlertRequestDefinition {
	public static class Threshold {
		public int category;
		// Short thresholdValue; // TODO: Logical threshold value.   
		public Short thresholdIndex;
		public boolean requestProduct;
	}
	
	private static final int N_BOX_BITS = 58 * 58;

	private static class BBAdapter extends XmlAdapter<byte[], BitSet> {

		@Override
		public byte[] marshal(BitSet v) throws Exception {
			byte[] result = new byte[(N_BOX_BITS+7)/8 + 1];
			result[0] = 1; // version
			for (int si = 0; si < N_BOX_BITS; ++si)
				if (v.get(si))
					result[1 + si / 8] |= 1 << (si & 7);
			return result;
		}

		@Override
		public BitSet unmarshal(byte[] v) throws Exception {
			BitSet result = new BitSet(N_BOX_BITS);
			for (int si = 0; si < N_BOX_BITS; ++si)
				if ((v[1 + si / 8] & (1 << (si & 7))) != 0)
					result.set(si);
			return result;
		}
		
	}
	
	
	public List<Threshold> elements = new ArrayList<Threshold>();
	@XmlJavaTypeAdapter(BBAdapter.class)
	public BitSet boxBits = new BitSet(58*58);
}
