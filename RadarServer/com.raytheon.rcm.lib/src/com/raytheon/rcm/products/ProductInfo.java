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
import java.util.Collection;
import java.util.Scanner;

import com.raytheon.rcm.config.RadarType;


public class ProductInfo {
	public static class Selector {
		public RadarType radarType;
		// dedicated?
		public Integer code;
		public String mnemonic;
		public Usage usage;
		public Selector() { }
		public Selector(RadarType radarType, String mnemonic, Integer code, Usage usage) {
			this.radarType = radarType;
			this.mnemonic = mnemonic;
			this.code = code;
			this.usage = usage;
		}
		public Selector duplicate() {
			return new Selector(radarType, mnemonic, code, usage);
		}
		public Selector narrow(Selector sel) {
			Selector result = duplicate();
			// TODO: conflicting non-null values.. exception or
			// make as something that will return the empty set?
			
			// Menu item selectors should be pre-merged...
			if (result.radarType == null)
				result.radarType = sel.radarType;
			if (result.code == null)
				result.code = sel.code;
			if (result.mnemonic == null)
				result.mnemonic = sel.mnemonic;
			if (result.usage == null)
				result.usage = sel.usage;
			return result;
		}
		public boolean matches(RadarProduct rp) {
			if (radarType != null && rp.typeRestriction != null &&
					! rp.typeRestriction.contains(radarType))
				return false;
			if (mnemonic != null && (rp.mnemonic == null ||
					! mnemonic.equalsIgnoreCase(rp.mnemonic)))
				return false;
			if (code != null && code.intValue() != rp.pid)
				return false;
			if (usage != null) {
				/* TODO: Usage information is in PMenuItem so this is a bit
				 * out of place.
				 */
				
			}
			return true;
		}
	}
	
	private static ProductInfo instance;

	public static ProductInfo getInstance() {
		if (instance == null)
			instance = new ProductInfo();
		return instance;
	}
	
	private Collection<RadarProduct> products;
	
	public ProductInfo() {
		InputStream s = ProductInfo.class.getResourceAsStream("radarInfo.txt");
		Scanner fs = new Scanner(s);
		try {
			products = Loader.loadRadarInfoData(fs);
		} finally {
			fs.close();
		}
	}
	
	public String getFullNameForMnemonic(String mnemonic) {
		for (RadarProduct p : products)
			if (p.mnemonic.equalsIgnoreCase(mnemonic) && p.name != null)
				return p.name;
		return null;
	}
	
	public String getMnemonicForCode(int code) {
		for (RadarProduct p : products)
			if (p.pid == code && p.mnemonic != null)
				return p.mnemonic;
		return null;
	}
	
	public Collection<RadarProduct> select(Selector sel) {
		ArrayList<RadarProduct> result = new ArrayList<RadarProduct>();
		for (RadarProduct rp : products) {
			if (sel.matches(rp))
				result.add(rp);
		}
		return result;
	}
	
    public RadarProduct selectOne(Selector sel) {
        Collection<RadarProduct> result = select(sel);
        if (! result.isEmpty())
            return result.iterator().next();
        else
            return null;
    }
	
	/* Assumes only one product descriptor for a code.  Probably okay
	 * for now. Possible issues: Old/new versions of a product. Non-issue:
	 * TDWR mini-volume.  Only available for TDWRs so we can handle it
	 * with extra context information.
	 */
	public RadarProduct getPoductForCode(int code) {
		for (RadarProduct rp : products) {
			if (rp.pid == code)
				return rp;
		}
		return null;
	}
}
