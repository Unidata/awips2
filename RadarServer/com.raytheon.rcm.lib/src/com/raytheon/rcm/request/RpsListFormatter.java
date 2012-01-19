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

import java.io.PrintWriter;
import java.util.Calendar;

import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.ProductInfo.Selector;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;


public class RpsListFormatter {
	/* See AWIPS-1 ProductRequestList::writeList()
	 * 
	 */
	public static void formatAwips1RpsList(RpsList list, String fileName, PrintWriter s) {
		RadarType radarType = list.getVcp() == 80 || list.getVcp() == 90 ?
		        RadarType.TDWR : RadarType.WSR;		
		Calendar cal = Calendar.getInstance();
		s.format("RPS List %s %s... %d products\n", 
	          fileName != null ? fileName : "",
    		  String.format("%1$tY:%1$tm:%1$td:%1$tH:%1$tM:%1$tS", cal),
			  list.getRequests().length);
		s.print(" An RPS list contains the fields: Prod-Name, Mnemonic, Prod-Code\n" +
				" Number of Data Levels, Resolution, Layer Code, Elevation, Contour Interval,\n" +
				" Priority, Req Interval, Map, Lower Layer, Upper Layer, multCut, endHour, timeSpan\n" +
				" The record format is: '%-39s  %-3s%4d%4d%6d %c%6d%7d%2d%2d%c%3d%3d %c%7d%7d'\n");

		for (Request r : list.getRequests()) {
			s.println(formatAwips1Request(r, radarType));
		}
	}

	private static final char[] layerCodes = {'L', 'M', 'H'};
	
	public static String formatAwips1Request(Request r, RadarType radarType) {
	    RadarProduct rp = ProductInfo.getInstance().selectOne(
	            new Selector(radarType, null, (int) r.productCode, null));
		
		/* Could probably guess how to format the important parts, but this
		 * is what AWIPS-1 does anyway...
		 */
		if (rp == null)
			throw new IllegalArgumentException("Cannot format unknown product type " + 
					r.productCode + ".");
		
		/* Note there is no check for rp.contains(ELEVATION)  This
		 * is fine for the current set of products that are available in
		 * the RPS list editor...
		 */
		/* // Mimic:
	    if (_multCuts == 'Y' && _elev > 16384)
        {
        _elev -= 16384;
        }
        */
		boolean multiCuts = false;
		int elev = r.pdw22;
		if (r.getElevationSelection() == Request.ALL_ELEVATIONS &&
		        r.getElevationAngle() != 0) {
		    elev = r.getElevationAngle();
	        multiCuts = true;
		}
		 
		char layerCode = '-';
		if (rp.layer != null) {
			try {
				layerCode = layerCodes[rp.layer - 1];
			} catch (ArrayIndexOutOfBoundsException e) {
				// nothing
			}
		}
		int lowerLayer = -1;
		int upperLayer = -1;
		if (rp.params.contains(Param.LAYER)) {
			lowerLayer = r.getBottomAltitude();
			upperLayer = r.getTopAltitude();
		} else if (rp.params.contains(Param.MINI_VOLUME)) {
		    lowerLayer = r.getMiniVolume();
		}
		int endHour = -1;
		int timeSpan = -1;
		if (rp.params.contains(Param.TIME_SPAN)) {
			endHour = r.getEndHour();
			timeSpan = r.getTimeSpan();
		}
    
	    return String.format("%-39.39s  %-3.3s%4d%4d%6d %c%6d%7d%2d%2d%c%3d%3d %c%7d%7d",
	    		rp.name, rp.mnemonic,
	    		r.productCode, 
	    		rp.levels != null ? rp.levels : 0,
				rp.resolution != null ? (int)(rp.resolution * 100) : 0,
				layerCode, elev, -1 /*contour interval*/,
				r.highPriority ? 1 : 0, r.interval, 
				r.mapRequested ? 'Y' : 'N',
				lowerLayer, upperLayer,
				multiCuts ? 'Y' : 'N',
				endHour, timeSpan);
	}
}
