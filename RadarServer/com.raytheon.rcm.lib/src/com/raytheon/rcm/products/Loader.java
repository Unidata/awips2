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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.products.ElevationInfo.Sel;
import com.raytheon.rcm.products.ElevationInfo.VCPInfo;
import com.raytheon.rcm.products.RadarProduct.Format;
import com.raytheon.rcm.products.RadarProduct.Param;

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

public class Loader {
	static List<RadarProduct> loadRadarInfoData(Scanner fs) {
		List<RadarProduct> result = new ArrayList<RadarProduct>();
		while (fs.hasNext()) {
			String line = fs.nextLine();
			Scanner ls = new Scanner(line);
			ls.useDelimiter("\\s*\\|\\s*");
			if (skipComments(ls))
				continue;
			RadarProduct rp = new RadarProduct();
			try {
				rp.params = EnumSet.noneOf(Param.class);
				
				rp.pid = ls.nextInt();
				rp.levels = ls.nextInt();
				int layerCode = ls.nextInt();
				if (layerCode == 9)
					rp.params.add(Param.LAYER);
				else if (layerCode > 0)
					rp.layer = layerCode;
				rp.resolution = ls.nextFloat();
				rp.range = ls.nextFloat();
				rp.mnemonic = ls.next().trim();
				rp.name = ls.next().trim();
				try {
					rp.format = RadarProduct.Format.valueOf(ls.next().trim());
				} catch (IllegalArgumentException e) {
					rp.format = RadarProduct.Format.UNKNOWN;
				}
				
				if (ls.next().trim().equalsIgnoreCase("y"))
					rp.params.add(Param.ELEVATION);
				if (ls.next().trim().equalsIgnoreCase("y"))
					rp.params.add(Param.ALTITUDE);
				if (ls.next().trim().equalsIgnoreCase("y"))
					rp.params.add(Param.WINDOW_AZ_RAN);
				if (ls.next().trim().equalsIgnoreCase("y"))
					rp.params.add(Param.STORM_SPEED_DIR);
				ls.next(); // Unused display field
				if (ls.next().trim().equalsIgnoreCase("y"))
					rp.params.add(Param.BASELINE);
				if (ls.next().trim().equalsIgnoreCase("y"))
					rp.params.add(Param.TIME_SPAN);
				if (ls.next().trim().equalsIgnoreCase("y")) // TODO: same as above?
					rp.params.add(Param.STORM_SPEED_DIR);
				if (ls.next().trim().equalsIgnoreCase("y"))
					rp.params.add(Param.CFC_BITMAP);
				if (rp.format == Format.RADIAL)
					rp.azimuthalResolution = 1.0f;
			} catch (NoSuchElementException e) {
				System.err.format("Bad radarInfo line: %s\n", line);
				continue;
			}
			try {
				ls.next(); // AWIPS ID
				rp.azimuthalResolution = ls.nextFloat();
			} catch (NoSuchElementException e) {
				// ignore
			}
			
			// This is not in any baselined database yet.
			switch (rp.pid) {
			case 180: case 181: case 182: case 183: case 185:
			case 186: case 187:
				rp.typeRestriction = EnumSet.of(RadarType.TDWR);
				break;

			case 35: case 36: // 35 and 36 are missing from SPG ICD Table IIa 
			case 37: case 38: 
			case 41: case 57:
			case 58: case 59:	case 61:
			case 141: 
				rp.params.add(Param.MINI_VOLUME);
				
			/* This is special.  For the TDWR, it does *not* have an elevation,
			 * only a mini-volume.  So create two products with code 149.
			 */
			case 149:
				rp.typeRestriction = EnumSet.of(RadarType.WSR);
				result.add(rp);
				rp = (RadarProduct) rp.clone();
				rp.typeRestriction = EnumSet.of(RadarType.TDWR);
				rp.params = EnumSet.of(Param.MINI_VOLUME);
				break;
				
			case 31: case 32: case 33:
			case 48:
			//case 62:// Listed in Table III, but apparently not generated
			case 75:
			case 78: case 79: case 80: case 81: case 82:
			case 84: // Missing from Table III
			case 100: case 101: case 102: case 104:
			case 107: case 108: case 109:
			case 137: case 138: 
			case 152:
				// nothing -- available for both WSR and TDWR
				break;
				
			case 173:
				// Dual pol, so WSR only.
				rp.typeRestriction = EnumSet.of(RadarType.WSR);
				// This product specifies the time span in minutes.
				rp.params.remove(Param.TIME_SPAN);
				rp.params.add(Param.TIME_SPAN_MINUTES);
				break;
				
			default:
				if (rp.pid >= 16) {
					rp.typeRestriction = EnumSet.of(RadarType.WSR);
				}
			}
			
			result.add(rp);
		}
		return result;
	}

	protected static boolean skipComments(Scanner s) {
		try {
			s.skip("^\\s*(#|//).*$");
		} catch (NoSuchElementException e) {
			// nothing
		}
		return ! s.hasNext(); // Also returns true if it was just a blank line
	}

	public static void loadElevationInfo(Scanner fs,
			HashMap<Sel, int[]> staticInfo,
			Collection<VCPInfo> vcpInfo) {
		Pattern p = Pattern.compile("^VCP(\\d+)$");
		while (fs.hasNext()) {
			String line = fs.nextLine();
			Scanner ls = new Scanner(line);		
			if (skipComments(ls))
				continue;
			try {
				String id = ls.next();
				int vcp = 0;
				String opModeText = ls.next(); // Unused op mode
				int nElevs = ls.nextInt();
				int[] elevs = new int[nElevs];
				for (int i = 0; i < elevs.length; ++i)
					elevs[i] = (int) (ls.nextDouble() * 10.0);
				
				Matcher m = p.matcher(id);
				if (m.matches())
					vcp = Integer.parseInt(m.group(1));
				else if (id.equals("OTR"))
					vcp = ElevationInfo.OTR_OR_RMR_CODE;
				else
					/* RMR is the same as OTR. We do not care about the TDWR
					 * entries, we will use the radar-specific one.  Actually,
					 * we shouldn't bother with VCP80 and VCP90 either (TODO: ?)
					 */
					continue; 
				Sel sel = new Sel(null, vcp);
				staticInfo.put(sel, elevs);
				
				int opMode = -1;
				if (opModeText.equalsIgnoreCase("storm"))
					opMode = GSM.OP_MODE_STORM;
				else if (opModeText.equalsIgnoreCase("clear-air"))
					opMode = GSM.OP_MODE_CLEAR_AIR;
				
				if (vcp != ElevationInfo.OTR_OR_RMR_CODE && opMode != -1) {
					VCPInfo vcpinf = new VCPInfo();
					vcpinf.vcp = vcp;
					vcpinf.opMode = opMode;
					vcpInfo.add(vcpinf);
				}
			} catch (NoSuchElementException e) {
				// TODO:
			}
		}		
	}

	public static void loadSsssElevationInfo(Scanner fs,
					HashMap<Sel, int[]> staticInfo) {
				Pattern p = Pattern.compile("^k*VCP(\\d+)$");
				while (fs.hasNext()) {
					String line = fs.nextLine();
					Scanner ls = new Scanner(line);		
					if (skipComments(ls))
						continue;
					try {
						String radarID = ls.next();
						String id = ls.next();
						int vcp = 0;
						ls.next(); // Unused op mode
						int nElevs = ls.nextInt();
						int[] elevs = new int[nElevs];
						for (int i = 0; i < elevs.length; ++i)
							elevs[i] = (int) (ls.nextDouble() * 10.0);
						
						Matcher m = p.matcher(id);
						if (m.matches())
							vcp = Integer.parseInt(m.group(1));
						else if (id.equals("OTR"))
							vcp = ElevationInfo.OTR_OR_RMR_CODE;
						else
							/* RMR is the same as OTR. We do not care about the TDWR
							 * entries, we will use the radar-specific one.  Actually,
							 * we shouldn't bother with VCP80 and VCP90 either (TODO: ?)
							 */
							continue; 
						Sel sel = new Sel(radarID, vcp);
						staticInfo.put(sel, elevs);
					} catch (NoSuchElementException e) {
						// TODO:
					}
				}		
			}
	
	public static void loadTdwrElevationInfo(Scanner fs,
			HashMap<Sel, int[]> staticInfo) {
		ArrayList<Integer> elevs = new ArrayList<Integer>(25);
		HashMap<String, HashSet<Integer>> allElevs = 
			new	HashMap<String, HashSet<Integer>>();
		
		while (fs.hasNext()) {
			String line = fs.nextLine();
			Scanner ls = new Scanner(line);		
			if (skipComments(ls))
				continue;
			/* We currently only need to know the set of unique angles, not
			 * the whole scan strategy.
			 */
			elevs.clear();
			try {
				String radar = "t" + ls.next().toLowerCase();
				ls.next(); // unused WFO ID
				String mode = ls.next();
				int vcp;
				if (mode.equals("HAZ"))
					vcp = 80;
				else if (mode.equals("MON"))
					vcp = 90;
				else
					continue;
				
				int elevIndex = 0;
				while (ls.hasNext()) {
				    int elev = (int)(ls.nextDouble() * 10);
                    /*
                     * For some reason, the third value is a duplicate of the
                     * second.
                     */
				    if (! (elevIndex == 2 && elevs.get(1) == elev))
				        elevs.add(elev);
					++elevIndex;
				}
					
                Sel sel = new Sel(radar, vcp);
                int[] elevsArray = new int[elevs.size()];
                for (int i = 0; i < elevsArray.length; ++i)
                    elevsArray[i] = elevs.get(i);
                staticInfo.put(sel, elevsArray);
				
				HashSet<Integer> allElevsForRadar = allElevs.get(radar);
				if (allElevsForRadar == null) {
					allElevsForRadar = new HashSet<Integer>();
					allElevs.put(radar, allElevsForRadar);
				}
				allElevsForRadar.addAll(elevs);
			} catch (NoSuchElementException e) {
				// TODO:
			}
		}
		
		for (Map.Entry<String, HashSet<Integer>> e: allElevs.entrySet()) {
			elevs.clear();
			elevs.addAll(e.getValue());
			int[] elevsArray = new int[elevs.size()];
			for (int i = 0; i < elevsArray.length; ++i)
				elevsArray[i] = elevs.get(i);
			Arrays.sort(elevsArray);
			
			Sel sel = new Sel(e.getKey(), ElevationInfo.OTR_OR_RMR_CODE);
			staticInfo.put(sel, elevsArray);
		}
	}
}
