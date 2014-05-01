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
package com.raytheon.rcm.products;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Scanner;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/07/2013   DR15495    zwang       Load elevation info for SSSS radars                                 
 * 
 * </pre>
 * 
 * @author dfriedman
 * @version 1.0
 */

public class ElevationInfo {
	// TODO: check all handling of -1/null vcp
	
	private static ElevationInfo instance;
	public static ElevationInfo getInstance() {
		synchronized (ElevationInfo.class) {
			if (instance == null)
				instance = new ElevationInfo();
		}
		return instance;
	}
	
	/* want to be private, but needed by Loader */
	static class Sel {
		public String radarID;
		public int vcp;
		public Sel(String radarID, int vcp) {
			super();
			this.radarID = radarID;
			this.vcp = vcp;
		}
		public boolean equals(Object obj) {
			if (obj == this)
				return true;
			else if (obj instanceof Sel) {
				Sel oth = (Sel) obj;
				return ((radarID == null && oth.radarID == null) ||
						radarID != null && oth.radarID != null &&
							radarID.equalsIgnoreCase(oth.radarID)) &&
					vcp == oth.vcp;
			} else
				return false;
		}
		public int hashCode() {
			return (radarID != null ? radarID.hashCode() : 0) + vcp;
		}
	}
	
	public static class VCPInfo {
		public int vcp;
		public int opMode;
		// public RadarType radarType;
		public String toString() {
			return String.format("VCP%d", vcp);
		}
	}
	
	private ArrayList<VCPInfo> vcpInfo = new ArrayList<VCPInfo>(); 
	private HashMap<Sel, int[]> staticInfo = new HashMap<Sel, int[]>();
	
	public ElevationInfo() {
		Scanner fs;
		InputStream s;
		
		s = ElevationInfo.class.getResourceAsStream("elevationLists.txt");
		fs = new Scanner(s);
		try {
			Loader.loadElevationInfo(fs, staticInfo, vcpInfo);
		} finally {
			fs.close();
		}

		// Load SSSS radar elevation lists
		s = ElevationInfo.class.getResourceAsStream("ssssElevationLists.txt");
		fs = new Scanner(s);
		try {
			Loader.loadSsssElevationInfo(fs, staticInfo);
		} finally {
			fs.close();
		}
		
		s = ElevationInfo.class.getResourceAsStream("tdwrElevations.txt");
		fs = new Scanner(s);
		try {
			Loader.loadTdwrElevationInfo(fs, staticInfo);
		} finally {
			fs.close();
		}
	
	}
	
	static final int OTR_OR_RMR_CODE = -1;
	
	public Collection<VCPInfo> getVcpInfo() {
		return vcpInfo;
	}
	
	public int getOpModeForVcp(int vcp) {
		for (VCPInfo vi : vcpInfo)
			if (vi.vcp == vcp)
				return vi.opMode;
		return -1;
	}
	
    /**
     * Retrieves the list of elevation angles used by the given radar in the
     * given VCP in the order they are scanned. This is intended to match the
     * ORPGVCP_get_all_elevation_angles function in CODE.
     * 
     * @param radarID
     * @param vcp
     *            the VCP number or {@code OTR_OR_RMR_CODE} for a set of
     *            generic angles
     * @return the list of angles in scanning order or {@code null} if not
     *         available
     */
	public int[] getScanElevations(String radarID, int vcp) {
        Sel sel1 = new Sel(radarID, vcp);
        Sel sel2 = new Sel(null, vcp);
        int[] elevs;
        
        elevs = staticInfo.get(sel1);
        if (elevs != null)
            return elevs;
        elevs = staticInfo.get(sel2);
        if (elevs != null)
            return elevs;
        
        return null;
	}
	
    /**
     * Retrieves a set of generic elevation angles used by the given
     * radar.
     * 
     * @param radarID
     * @return the set of angles in ascending order or {@code null} if not
     *         available
     */
	public int[] getUniqueElevations(String radarID) {
	    return getUniqueElevations(radarID, OTR_OR_RMR_CODE);
	}
	
    /**
     * Retrieves the set of unique elevation angles used by the given radar in
     * the given VCP.
     * 
     * @param radarID
     * @param vcp
     *            the VCP number or {@code OTR_OR_RMR_CODE} for a set of
     *            generic angles
     * @return the set of angles in ascending order or {@code null} if not
     *         available
     */
	public int[] getUniqueElevations(String radarID, int vcp) {
	    // TODO: Unique, ascending
        int[] elevs = getScanElevations(radarID, vcp);
        if (elevs != null) {
            HashSet<Integer> set = new HashSet<Integer>(elevs.length);
            for (int e : elevs)
                set.add(e);
            
            elevs = new int[set.size()];
            int i = 0;
            for (int e : set)
                elevs[i++] = e;
            Arrays.sort(elevs);
        }
        return elevs;
	}
	
}
