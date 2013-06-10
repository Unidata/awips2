/*
* The following software products were developed by Raytheon:
*
* ADE (AWIPS Development Environment) software
* CAVE (Common AWIPS Visualization Environment) software
* EDEX (Environmental Data Exchange) software
* uFrame™ (Universal Framework) software
*
* Copyright (c) 2010 Raytheon Co.
* All rights reserved. This program and the accompanying materials
* are made available under the terms of the Eclipse Public License v1.0
* which accompanies this distribution, and is available at
* http://www.eclipse.org/org/documents/epl-v10.php
*
*
* Contractor Name: Raytheon Company
* Contractor Address:
* 6825 Pine Street, Suite 340
* Mail Stop B8
* Omaha, NE 68106
* 402.291.0100
*
*
* SOFTWARE HISTORY
*
* Date         Ticket#    Engineer    Description
* ------------ ---------- ----------- --------------------------
* Feb 17, 2012            bclement     Initial creation
*
*/ 
package com.raytheon.uf.edex.ogc.common.spatial;

import java.util.regex.Pattern;

import org.apache.commons.collections.map.LRUMap;
import org.geotools.referencing.CRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.NoSuchAuthorityCodeException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

/**
 * TODO Add Description
 *
 * @author bclement
 * @version 1.0	
 */
public class CrsLookup {

	private static final int N_OBJECTS = 10;

	public static final String GOOGLE_CRS_WKT = "PROJCS[\"Google Mercator\","
			+ "GEOGCS[\"WGS 84\","
			+ "DATUM[\"World Geodetic System 1984\","
			+ "SPHEROID[\"WGS 84\", 6378137.0, 298.257223563, AUTHORITY[\"EPSG\",\"7030\"]],"
			+ "AUTHORITY[\"EPSG\",\"6326\"]],"
			+ "PRIMEM[\"Greenwich\", 0.0, AUTHORITY[\"EPSG\",\"8901\"]],"
			+ "UNIT[\"degree\", 0.017453292519943295],"
			+ "AXIS[\"Geodetic latitude\", NORTH],"
			+ "AXIS[\"Geodetic longitude\", EAST],"
			+ "AUTHORITY[\"EPSG\",\"4326\"]],"
			+ "PROJECTION[\"Mercator_1SP\"],"
			+ "PARAMETER[\"semi_minor\", 6378137.0],"
			+ "PARAMETER[\"latitude_of_origin\", 0.0],"
			+ "PARAMETER[\"central_meridian\", 0.0],"
			+ "PARAMETER[\"scale_factor\", 1.0],"
			+ "PARAMETER[\"false_easting\", 0.0],"
			+ "PARAMETER[\"false_northing\", 0.0]," + "UNIT[\"m\", 1.0],"
			+ "AXIS[\"Easting\", EAST]," + " AXIS[\"Northing\", NORTH],"
			+ "AUTHORITY[\"EPSG\",\"900913\"]]";

	protected static final LRUMap cache = new LRUMap(N_OBJECTS);

	protected static CoordinateReferenceSystem googleCrs;

	protected static final Pattern OGC_CODE_PATTERN = Pattern
			.compile("^([a-zA-Z]+)([0-9]+)$");

	public static CoordinateReferenceSystem lookup(String crs)
			throws NoSuchAuthorityCodeException, FactoryException {
		if (crs == null) {
			return null;
		}
		crs = normalize(crs);
		CoordinateReferenceSystem rval;
		synchronized (cache) {
			rval = (CoordinateReferenceSystem) cache.get(crs);
			if (rval == null) {
				rval = decodeCrs(crs);
				if (rval != null) {
					cache.put(crs, rval);
				}
			}
		}
		return rval;
	}

	protected static CoordinateReferenceSystem decodeCrs(String crs)
			throws NoSuchAuthorityCodeException, FactoryException {
		if (crs.equalsIgnoreCase("epsg:900913")
				|| crs.equalsIgnoreCase("epsg:3857")) {
			return getGoogleCrs();
		}
		return CRS.decode(crs, true);
	}

	protected static CoordinateReferenceSystem getGoogleCrs()
			throws FactoryException {
		if (googleCrs == null) {
			googleCrs = CRS.parseWKT(GOOGLE_CRS_WKT);
		}
		return googleCrs;
	}

	protected static String normalize(String crs) {
		String[] parts = crs.split(":");
		String rval;
		if (parts.length == 2) {
			// good form
			rval = crs;
		} else if (parts.length == 7) {
			// probably an OGC URN
			rval = constructCode(parts[4], parts[6]);
		} else if (parts.length == 6) {
			// OGC URN without version?
			rval = constructCode(parts[4], parts[5]);
		} else {
			// unkown form, try it anyway
			rval = crs;
		}
		return rval.toLowerCase();
	}

	protected static String constructCode(String authority, String code) {
		if (!authority.equalsIgnoreCase("epsg")
				&& !authority.equalsIgnoreCase("crs")) {
			// geotools database only has epsg codes
			// try a generic crs authority
			authority = "crs";
		}
		return authority + ":" + code;
	}

}
