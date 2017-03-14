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

/**
 * Filters the duplicate requests
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date   Ticket#    Engineer    Description
 * ------ ---------- ----------- --------------------------
 *                               Initial creation
 * 062413 DR16023    zwang       Support all cuts for nexrad 
 * </pre>
 */

/* Should ref CODE src/cpc104/lib003/product_attr_table */

public class DefaultFilter extends Filter {

    @Override
    public boolean requestsEqual(Request a, Request b, int[] elevList, RadarType radarType) {
        return compareRequests(a, b, elevList, radarType) == COMPLETE_MATCH;
    }

    @Override
    public Request mergeRequests(Request a, Request b, int[] elevList, RadarType radarType) {
        int compareResult = compareRequests(a, b, elevList, radarType);
        
        if (compareResult == NO_MATCH)
            return null;
        else if (compareResult == COMPLETE_MATCH)
            return a;
        
        if (a.getElevationSelection() == Request.ALL_ELEVATIONS && a.getElevationAngle() == 0)
            return a;
        if (b.getElevationSelection() == Request.ALL_ELEVATIONS && b.getElevationAngle() == 0)
            return b;
        
        for (int z = 0; z < 2; ++z) {
            if (elevList != null) {
                int aSel = a.getElevationSelection();
                int bSel = b.getElevationSelection();
                int ia = -1;
                int ib = -1;
                if (a.getElevationSelection() == Request.N_ELEVATIONS)
                    ia = a.getElevationAngle() - 1;
                else if (elevList != null)
                    ia = findMatchingElevationIndex(elevList, a.getElevationAngle());
                if (b.getElevationSelection() == Request.N_ELEVATIONS)
                    ib = b.getElevationAngle() - 1;
                else if (elevList != null)
                    ib = findMatchingElevationIndex(elevList, b.getElevationAngle());
                
                if (aSel == Request.LOWER_ELEVATIONS) {
                    switch (bSel) {
                    case Request.ALL_ELEVATIONS:
                    case Request.SPECIFIC_ELEVATION:
                        if (ib >= 0 && elevList[ib] <= a.getElevationAngle())
                            return a;
                        break;
                    case Request.LOWER_ELEVATIONS:
                        if (a.getElevationAngle() >= b.getElevationAngle())
                            return a;
                        break;
                    case Request.N_ELEVATIONS:
                    {
                        int i;
                        for (i = 0; i <= ib; ++i)
                            if (elevList[i] >= a.getElevationAngle())
                                break;
                        if (i > ib)
                            return a;
                        break;
                    }
                    }
                } else if (a.getElevationSelection() == Request.N_ELEVATIONS) {
                    switch (bSel) {
                    case Request.ALL_ELEVATIONS:
                        if (ib >= 0) {
                            int i;
                            for (i = ia + 1; i < elevList.length; ++i)
                                if (elevList[i] == elevList[ib])
                                    break;
                            if (i >= elevList.length)
                                return a;
                        }
                        break;
                    case Request.N_ELEVATIONS:
                    case Request.SPECIFIC_ELEVATION:
                        if (ia >= ib)
                            return a;
                        break;
                    case Request.LOWER_ELEVATIONS: {
                        int i;
                        for (i = ia + 1; i < elevList.length; ++i)
                            if (elevList[i] <= b.getElevationAngle())
                                break;
                        if (i >= elevList.length)
                            return a;
                        break;
                    }
                    }
                }
            } else {
                if (a.getElevationSelection() == Request.N_ELEVATIONS &&
                        b.getElevationSelection() == Request.N_ELEVATIONS &&
                        a.getElevationAngle() >= b.getElevationAngle())
                    return a;
                else if (a.getElevationSelection() == Request.LOWER_ELEVATIONS &&
                        (b.getElevationSelection() == Request.LOWER_ELEVATIONS ||
                                b.getElevationSelection() == Request.SPECIFIC_ELEVATION) &&
                            a.getElevationAngle() >= b.getElevationAngle())
                    return a;
            }
            
            // try again, swapped
            Request t = a;
            a = b;
            b = t;
        }
        
        return null;
    }
    
	private static int findMatchingElevationIndex(int[] elevList, int elevationAngle) {
		if (elevList == null || elevList.length == 0)
			return -1;
	
		// TODO: should elevationAngle == 0 be treated specially?
		
		if (elevationAngle <= elevList[0])
			return 0;
		
		for (int i = 1; i < elevList.length; ++i) {
			if (elevationAngle == elevList[i])
				return i;
			else if (elevationAngle < elevList[i]) {
				if (elevationAngle <= ((elevList[i - 1] + elevList[i]) / 2))
					return i - 1;
				else
					return i;
			}
		}
		
		return elevList.length - 1;
	}
	
	// Result may not be the index of a unique angle
    private static int findIndexOfGreatestAngle(int[] elevList,
            int maxElevationAngle) {
        if (elevList == null || elevList.length == 0)
            return -1;
    
        if (maxElevationAngle <= elevList[0])
            return -1;
        
        int best = elevList[0];
        int bestIndex = 0;
        
        for (int i = 1; i < elevList.length; ++i) {
            if (elevList[i] <= maxElevationAngle && elevList[i] > best) {
                best = elevList[i];
                bestIndex = i;
            }
        }
        
        return bestIndex;
    }
	
	
    private static int NO_MATCH = 0;
    private static int ELEV_MISMATCH = 1;
    private static int COMPLETE_MATCH = 2;

    private static int compareRequests(Request a, Request b, int[] elevList, RadarType radarType) {
        if (a.productCode != b.productCode ||
                a.highPriority != b.highPriority ||
                a.mapRequested != b.mapRequested ||
                a.interval != b.interval ||
                a.count != b.count ||
                a.getVolumeScanSelection() != b.getVolumeScanSelection() ||
                (a.getVolumeScanSelection() == Request.SELECT_SPECIFIC &&
                        ! a.getVolumeScanTime().equals(b.getVolumeScanTime())))
            return NO_MATCH;
        
        boolean paramsEqual = false;
        boolean checkElev = false;
        // from ProductRequestEntry.C ProductRequestEntry::getFilter
        switch (a.productCode)
        {
        case 31: case 173: // User selectable precip and accum
        case 150: case 151: // User selectable snow accumulation // missing from ProductRequestEntry::getFilter 
            paramsEqual = a.getEndHour() == b.getEndHour() &&
                a.getTimeSpan() == b.getTimeSpan();
            break;

        case 34: // Clutter filter control
            paramsEqual = a.getCfcWord() == b.getCfcWord();
            break;

        case 42: // Echo tops contour // Obsolete?
            paramsEqual = a.getContourInterval() == b.getContourInterval();
            break;
            
        case 43: case 44: case 45: case 46: // Sever weather analysis
            paramsEqual = a.getAzimuth() == b.getAzimuth() && a.getRange() == b.getRange();
            checkElev = true;
            break;

        case 49: // Combined moment
            paramsEqual = a.getAzimuth() == b.getAzimuth() && a.getRange() == b.getRange();
            checkElev = true;
            break;

        case 50: case 51: case 52: case 85: case 86: // Cross sections
            paramsEqual = a.getAzimuth() == b.getAzimuth() && a.getRange() == b.getRange() &&
                a.getAzimuth2() == b.getAzimuth2() && a.getRange2() == b.getRange2();
            break;

        case 53: // Weak echo region
            // TODO: no doc?! is it obsolete?
            paramsEqual = a.getAzimuth() == b.getAzimuth() && a.getRange() == b.getRange() &&
            // CODE sez different fields? double check...
                a.pdw22 == b.pdw22 && a.pdw23 == b.pdw23; // 'bitmap 0' and 'bitmap 1' representing which elevations to use
            break;

        case 55: // SRM radial velocity region
            paramsEqual = a.getAzimuth() == b.getAzimuth() && a.getRange() == b.getRange() &&
                    a.getStormSpeed() == b.getStormSpeed() && 
                    a.getStormDirection() == b.getStormDirection();
            checkElev = true;
            break;

        case 56: // SRM radial velocity map
            paramsEqual = a.getStormSpeed() == b.getStormSpeed() &&
                    a.getStormDirection() == b.getStormDirection();
            checkElev = true;
            break;

        case 84: // VAD
            paramsEqual = a.getAltitude() == b.getAltitude();
            break;

        case 73: case 74: case 75: case 82: // Text based
            paramsEqual = true;
            break;
            
        case 47: 
        case 48: case 60:  
        case 62: case 63: case 64: case 65: case 66: case 67: 
        case 78: case 79: case 80: case 81: case 89: case 90:
        case 134: case 135: 
        case 169: case 170: case 171: case 172: case 174: case 175: case 176:
        case 177: // Volume based
            paramsEqual = true;
            break;

        case 137: // User selectable layer reflectivity
            paramsEqual = a.getBottomAltitude() == b.getBottomAltitude() &&
                a.getTopAltitude() == b.getTopAltitude();
            break;

        // Handle SPG mini volume products
        // ICD says 41 is echo tops (not echo top contour)
        case 35: case 36: case 37: case 38: 
        case 41: case 57: case 58: case 59: case 61: case 141:
            if (radarType == RadarType.TDWR)
                paramsEqual = a.getMiniVolume() == b.getMiniVolume();
            else
                paramsEqual = true;
            break;

        case 149:
            if (radarType == RadarType.TDWR)
                paramsEqual = a.getMiniVolume() == b.getMiniVolume();
            else
                paramsEqual = checkElev = true;
            break;

        default:
            paramsEqual = checkElev = true;
            break;
        }
        
        if (! paramsEqual)
            return NO_MATCH;
        else if (! checkElev)
            return COMPLETE_MATCH;
        else {
            boolean elevEq = false;
            
            if (elevList != null) { 
                // If one of the request is an all cuts, perform exact comparison
                if ((a.getElevationSelection() == Request.ALL_ELEVATIONS &&
                    a.getElevationAngle() != 0)	||
                    (b.getElevationSelection() == Request.ALL_ELEVATIONS &&
                    b.getElevationAngle() != 0)) {
                    if (a.getElevationSelection() == b.getElevationSelection() &&
                        a.getElevationAngle() == b.getElevationAngle()	) {
                        return COMPLETE_MATCH;
                    }
                    else {
                        return NO_MATCH;
                    }
                }
                // If elevList is provided, compare equivalent elevation angles.
                if (a.getElevationSelection() == b.getElevationSelection()) {
                    if (a.getElevationSelection() == Request.ALL_ELEVATIONS &&
                            a.getElevationAngle() == 0 && b.getElevationAngle() == 0)
                        elevEq = true;                        
                    else if (a.getElevationSelection() == Request.ALL_ELEVATIONS ||
                            a.getElevationSelection() == Request.SPECIFIC_ELEVATION) {
                        int ea = findMatchingElevationIndex(elevList, a.getElevationAngle());
                        int eb = findMatchingElevationIndex(elevList, b.getElevationAngle());
                        elevEq = ea == eb && ea != -1;
                    } else if (a.getElevationSelection() == Request.LOWER_ELEVATIONS) {
                        int ea = findIndexOfGreatestAngle(elevList, a.getElevationAngle());
                        int eb = findIndexOfGreatestAngle(elevList, b.getElevationAngle());
                        elevEq = ea == eb && ea != -1;
                    } else if (a.getElevationSelection() == Request.N_ELEVATIONS) {
                        elevEq = a.getElevationAngle() == b.getElevationAngle();
                    }
                }
            } else
                // Otherwise, perform exact comparison.
                elevEq = a.getElevationSelection() == b.getElevationSelection() &&
                    a.getElevationAngle() == b.getElevationAngle();
            
            return elevEq ? COMPLETE_MATCH : ELEV_MISMATCH;
        }
    }
}
