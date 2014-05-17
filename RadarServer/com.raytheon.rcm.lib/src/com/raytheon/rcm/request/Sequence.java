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
package com.raytheon.rcm.request;

import java.util.HashMap;

/** Utility class used to generate unique request sequence numbers. */
public class Sequence {
    // The RPS list manager uses the sequence number one.
    private static final short MIN_SEQUENCE_NUMBER = 2;

	protected static HashMap<String, short[]> map = new HashMap<String, short[]>();
	
	public static short next(String radarID) {
		synchronized (map) {
			short[] value = map.get(radarID);
			if (value == null) {
				value = new short[1];
				value[0] = MIN_SEQUENCE_NUMBER;
				map.put(radarID, value);
			}
			short nextValue = value[0]++;
			if (nextValue == Short.MAX_VALUE)
				value[0] = MIN_SEQUENCE_NUMBER;
			return nextValue; 
		}
	}
}
