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
package com.raytheon.rcm.config.awips1;

import java.io.File;

import com.raytheon.rcm.config.ProductDistributionInfo;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.StandardProductDistInfoDB;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.products.ProductInfo.Selector;


/*
 * The WSR-88D national RPS list asks for 0.5, 1.5, 2.5, and 3.5 elevations.  
 * If the actual available elevations are 1.3, 2.4, etc. there is no problem
 * because these are covered by a few additional entries in the WSR-88D 
 * prodList.txt.  There is one low elevations that is not covered -- 1.8.  But
 * there will always either 1.3 or 1.5 to fill the N1R/N1V categories.
 * 
 * For TDWRs, there are (too) many possible angles that could be returned
 * for a given request.  To account for this, the AWIPS 1 code maps an received
 * product's elevation to one of the AWIPS 1 standard tilt angle groups.  The
 * tdwrProdList.txt then references the representative values of the tilt angle
 * groups.  There is an exception for the 0.6 elevation for the long range
 * reflectivity product (which, I guess, is the same for all TDWRs so it would
 * look awkward to reference it with "0.5")
 * 
 * What should really be done is to specify elevation ranges in the 
 * prodList.txt file.  Also, it should be explicit about what parameter is 
 * matched so that the code does not have to hardcode matches against field 
 * other than elevation.
 * 
 * The new design should work this way.  In the mean time, we'll try to isolate
 * all the nasty implicit/ambiguous stuff here.
 */

public class Awips1ProdDistInfoBuilder {
	
    private static final int[] DUA_DURATION_TABLE = {0, 60, 120, 180, 360, 720, 1440};
	
	public static void addProdListEntry(File path, int messageCode, int elev, boolean isTdwr,
			ProductDistributionInfo info, StandardProductDistInfoDB db)	{
	    
	    RadarProduct product = ProductInfo.getInstance().selectOne(
                new Selector(isTdwr ? RadarType.TDWR : RadarType.WSR, null,
                        messageCode, null));
	    
	    if (product == null) {
	        Log.errorf("%s: unknown product code '%d'", path, elev);
	        return;
	    }

	    if (product.params.contains(Param.ELEVATION)) {
            /* From AWIPS 1:
             *      // For TDWR radar, use the primary angle instead of the real
                    // elevation angle except 0.6 (long range product 186 and 187)
                    if (sourceId >= 3000 && sourceId <= 3045
                        && (int)(elev*10) != 0 
                    && (int)prodCode != 186 && (int)prodCode != 187)
                       elev = getPrimaryAngle(newFileName);

             */

            // Note the code makes no reference to 0.6.  It is just the case
            // that tdwrProdList.txt has 0.6 listed for product 186.
            // The code actually checks from product 186 or 187
            if (isTdwr && messageCode != 186 && messageCode != 187) {
                int[] range = getAngleGroupRange(elev);
                db.add(messageCode, Param.ELEVATION, range[0], range[1], info);
            } else {
                db.add(messageCode, Param.ELEVATION, elev, elev, info);
            }
	    } else if (product.params.contains(Param.CFC_BITMAP)) {
            if (elev != 0) {
                if (elev < 0 && (elev % 10) == 0) {
                    int value = - elev / 10;
                    db.add(messageCode, Param.CFC_BITMAP, value, value, info);
                } else {
                    Log.errorf("CFC segment number must be negative multiple of 10.  Input was '%d'.",
                            elev);
                    return;
                }
            } else 
                db.add(messageCode, info);
	    } else if (product.params.contains(Param.TIME_SPAN_MINUTES)) {
            /* From AWIPS 1 RadarStorageController.C...
                {
                    int p = Cvt2ToULong(&msg[54]);
                    int elevl;
                    if (p==60)
                        elev = -1;
                    else if (p==120)
                        elev = -2;
                    else if (p==180)
                        elev = -3;
                    else if (p==360)
                        elev = -4;
                    else if (p==720)
                        elev = -5;
                    else if (p==1440)
                        elev = -6;
                    else
                        elev = 0;
                }
               
               DUA is currently not in prodList.txt and it is not clear where
               the requirements for this are defined.          
            */
            if (elev >= -6 && elev <= -1) {
                int value = DUA_DURATION_TABLE[-elev];
                db.add(messageCode, Param.TIME_SPAN_MINUTES, value, value, info);
            } else {
                Log.errorf("DUA duration specifier must be in the range [-6,-1].  Input was '%d'.",
                        elev);
                return;
            }
	    } else
	        db.add(messageCode, info);
	}

	// AWIPS1 note:
	// 	Because the radar ingest system uses zero as the value that stands for the
	// 	null tilt, any bin including 0.0 must have 0.0 as the primary tilt.
	// ... not sure if this applies to us

	// Sequence of (group-rep1, min1, max1, group-rep2, min2, max2, ...)
	protected static int[] angleGroups = { 
			0, 0, 3,
			5, 4, 7,
			9, 8, 11,
			15, 12, 16,
			18, 17, 20,
			24, 21, 26,
			34, 27, 36,
			43, 37, 46,
			53, 47, 56,
			60, 57, 66,
			75, 67, 80,
			87, 81, 95,
			100, 96, 110,
			120, 111, 130,
			140, 131, 156,
			167, 157, 179,
			195, 180, 220,
			250, 221, 275,
			300, 276, 325,
			350, 326, 375,
			400, 376, 425,
			450, 426, 475,
			500, 476, 525,
			550, 526, 575,
			600, 576, 625
	};

	// In case we want to load from tiltAngleGroups.txt...
	public static void setAngleGroups(int[] newAngleGroups) {
		angleGroups = newAngleGroups;
	}

	private static final int REP = 0;
	private static final int MIN = 1;
	private static final int MAX = 2;

	private static int[] getAngleGroupRange(int elev) {
		int[] result = new int[2];
		
		if (angleGroups == null || elev < -9 || elev > 900) {
			result[0] = result[1] = elev;
			return result;
		}

		elev = Math.abs(elev);
		
		int i = 0;
		for (i = 0; i < angleGroups.length; i += 3) {
			if (elev >= angleGroups[i+MIN] && elev <= angleGroups[i+MAX]) {
				break;
			} else if (elev < angleGroups[i+MIN]) { // shouldn't happen because tiltAngleGroups.txt should have full coverage
				if (i == 0) {
					result[0] = result[1] = elev;
					return result;
				} else {
					i -= 3;
					break;
				}
			} 
		}
		
		result[0] = angleGroups[i+MIN];
		result[1] = angleGroups[i+MAX];
		return result;
	}
	
	public static int getAngleGroup(int elev) {
		int i = 0;
		for (i = 0; i < angleGroups.length; i += 3) {
			if (elev >= angleGroups[i+MIN] && elev <= angleGroups[i+MAX]) {
				return angleGroups[i+REP];
			}
		}
		return -1;
	}
}
