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
package com.raytheon.rcm.config;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.rcm.message.GraphicProduct.PDB;
import com.raytheon.rcm.products.RadarProduct.Param;

/**
 * Simple implementation of ProductDistInfoDB that can represent the AWIPS 1
 * prodList.txt and tdwrProdList.txt.
 * <p>
 * This should only be use directly within the ...config.* sub-packages.
 * <p>
 * Implementation note: Does not use MessageInfo to determine if a product
 * is elevation based.  Whether or not the elevation angle is specified in
 * the matching criteria.
 */
public class StandardProductDistInfoDB extends ProductDistInfoDB {

	protected static class SimpleMatch {
		public int messageCode;
		public Param matchParam;
		public int minValue;
		public int maxValue;

		public SimpleMatch(int messageCode) {
			this.messageCode = messageCode;
		}

		/**
		 * NOTE: If matchParam is Param.CFC_BITMAP, minValue and maxValue
		 * refer to the segment number (not the bitmap value).
		 */
		public SimpleMatch(int messageCode, Param matchParam, int minValue, int maxValue) {
		    if (matchParam != null && matchParam != Param.ELEVATION &&
		            matchParam != Param.CFC_BITMAP)
		        throw new IllegalArgumentException("Unsupported parameter "
		                + matchParam);
            this.messageCode = messageCode;
            this.matchParam = matchParam;
            this.minValue = minValue;
            this.maxValue = maxValue;
        }

		@Override
		public boolean equals(Object obj) {
			try {
				SimpleMatch other = (SimpleMatch) obj;
				return messageCode == other.messageCode
						&& matchParam == other.matchParam
						&& minValue == other.minValue
						&& maxValue == other.maxValue;
			} catch (ClassCastException e) {
				return false;
			}
		}

		@Override
		public int hashCode() {
		    int result = matchParam != null ? matchParam.hashCode() : 0;
		    result = (result * 31) ^ minValue;
		    result = (result * 31) ^ maxValue;
		    result = (result * 31) ^ messageCode;
		    return result;
		}
	}

	HashMap<SimpleMatch, ProductDistributionInfo> matches = new HashMap<SimpleMatch, ProductDistributionInfo>();

	@Override
	public ProductDistributionInfo getProductDistInfo(RadarConfig rc,
			int messageCode, PDB pdb) {
		for (Map.Entry<SimpleMatch, ProductDistributionInfo> e : matches
				.entrySet()) {
			SimpleMatch match = e.getKey();
			if (match.messageCode == messageCode) {
			    boolean matched = false;
			    if (match.matchParam != null) {
			        if (pdb != null) {
			            int value;
			            if (match.matchParam == Param.ELEVATION)
			                value = pdb.getElevationAngle();
			            else if (match.matchParam == Param.CFC_BITMAP)
			                value = pdb.getElevationSegmentNumber();
			            else if (match.matchParam == Param.TIME_SPAN_MINUTES)
			                value = pdb.getTimeSpan();
			            else
			                continue;
			            matched = value >= match.minValue &&
			                value <= match.maxValue;
			        }
			    } else
			        matched = true;

			    if (matched)
			        return e.getValue();
			}
		}
		return null;
	}

	/*
	 * The following two methods are all that are needed now for the current
	 * AWIPS 1 prodList.txt and tdwrProdList.txt files. If there are 
	 * conflicting terms, an IllegalArgumentException will be thrown which means 
	 * a redesign is needed.
	 */
	public void add(int messageCode, ProductDistributionInfo info) {
		add(new SimpleMatch(messageCode), info);
	}

	public void add(int messageCode, Param param, int minValue, int maxValue,
			ProductDistributionInfo info) {
		add(new SimpleMatch(messageCode, param, minValue, maxValue), info);
	}

	protected void add(SimpleMatch match, ProductDistributionInfo info) {
		ProductDistributionInfo existing = matches.get(match);
		if (existing != null) {
			if (existing.equals(info))
				return;
			else
				throw new IllegalArgumentException(
						"Attempted to add conflicting information to the product distribution table.");
		} else
			matches.put(match, info);
	}
}
