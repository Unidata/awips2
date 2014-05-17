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

import com.raytheon.rcm.config.RadarType;

public abstract class Filter {
	
	public abstract boolean requestsEqual(Request a, Request b, int[] elevList, RadarType radarType);
	
	public abstract Request mergeRequests(Request a, Request b, int[] elevList, RadarType radarType);
	
	protected static DefaultFilter instance = new DefaultFilter();
	
	public static Filter getFilterForCode(int code) {
		return instance;		
	}
	
	/*
	static void registerFilter(Set<Integer> codes, Filter filter) {
	
	}
	*/
}
