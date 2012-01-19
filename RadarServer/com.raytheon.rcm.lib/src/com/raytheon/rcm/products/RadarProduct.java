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

import java.util.EnumSet;
import java.util.HashMap;

import com.raytheon.rcm.config.RadarType;


public class RadarProduct implements Cloneable {
	public enum Format {
		GSM, PRR, AAP, PTL, AM, RADIAL, RASTER, GRAPHIC, TEXT, GENERIC, UNKNOWN
	}
	public enum Param {
		ELEVATION, ALTITUDE, BASELINE, WINDOW_AZ_RAN, STORM_SPEED_DIR, 
		MINI_VOLUME, TIME_SPAN, TIME_SPAN_MINUTES, LAYER, CFC_BITMAP
	}

	public int pid;
	public HashMap<String,String> variations = new HashMap<String,String>();
	public Integer levels;
	public Integer layer;
	public Float resolution;
	public Float range;
	public Float azimuthalResolution;
	public String mnemonic;
	public String name;
	public Format format;
	public EnumSet<Param> params;
	public EnumSet<RadarType> typeRestriction;
	
	public Object clone() {
		try {
			return super.clone();
		} catch (CloneNotSupportedException e) {
			throw new UnsupportedOperationException(e);
		}
	}
}
