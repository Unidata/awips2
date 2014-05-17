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

import java.util.Arrays;
import java.util.HashSet;

/** Provides general information about Nexrad messages and products.
 * 
 * <p>
 * Try to minimize the use of this class.  There should be as few 
 * dependencies as possible on a database that will need to be updated whenever
 * a new elevation-based radar product is developed.  Also, this information
 * may not be correct in all contexts.  For example, the DMD product (#149)
 * has an elevation parameter on WSR-88D, but not TDWR.
 * 
 * <p>
 * Current uses:
 * 
 * <p>
 * OTRManager: If an product is elevation-based we have to interpret
 *   multi-elevation requests in order to determine how many products to
 *   expect in response to a request.
 *   
 * <p>
 * StandardProductDistInfoDB.getProductDistInfo: If a product is not
 * elevation-based, do not try to match based on elevation. 
 */
public class MessageInfo {
	
	private static HashSet<Integer> elevationProductCodes;
	
	static {
		elevationProductCodes = new HashSet<Integer>(Arrays.asList(
				16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 43, 
				44, 45, 46, 55, 56, 93, 94, 99, 132, 133, 139, 143, 149, 153, 
				154, 155, 158, 159, 160, 161, 162, 163, 164, 165, 180, 181, 
				182, 183, 185, 186, 187));
	}
	
	/** 
	 * Returns true if the specified product accepts an elevation angle
	 * in product-dependent word 22.
	 */
	public static boolean isElevationBasedProduct(int messageCode) {
		return elevationProductCodes.contains(messageCode);
	}
}
