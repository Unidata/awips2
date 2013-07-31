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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.quantity.Quantity;
import javax.measure.unit.Unit;

import org.apache.commons.collections.map.LRUMap;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultCompoundCRS;
import org.geotools.referencing.crs.DefaultVerticalCRS;
import org.geotools.referencing.cs.DefaultCoordinateSystemAxis;
import org.geotools.referencing.cs.DefaultVerticalCS;
import org.geotools.referencing.datum.DefaultVerticalDatum;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.NoSuchAuthorityCodeException;
import org.opengis.referencing.ReferenceIdentifier;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.GeographicCRS;
import org.opengis.referencing.cs.AxisDirection;

import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;

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

    public static final Pattern EXTENDED_3D_CRS_PATTERN = Pattern.compile(
            "(urn.*)_plus_z_in_([^_]+)(_([^_]+))?", Pattern.CASE_INSENSITIVE);

    /**
     * Lookup coordinate reference system object from OGC URN or Code
     * 
     * @param crs
     * @return
     * @throws NoSuchAuthorityCodeException
     * @throws FactoryException
     * @throws OgcException
     */
	public static CoordinateReferenceSystem lookup(String crs)
            throws NoSuchAuthorityCodeException, FactoryException, OgcException {
		if (crs == null) {
			return null;
		}
        Matcher m = EXTENDED_3D_CRS_PATTERN.matcher(crs);
        if (m.matches()) {
            return decodeExtended3D(m);
        }
        return lookupFromCache(crs);
	}

    /**
     * Parse extended CRS string that has matched
     * {@link CrsLookup#EXTENDED_3D_CRS_PATTERN}
     * 
     * @param m
     * @return
     * @throws NoSuchAuthorityCodeException
     * @throws FactoryException
     * @throws OgcException
     */
    protected static CoordinateReferenceSystem decodeExtended3D(Matcher m)
            throws NoSuchAuthorityCodeException, FactoryException, OgcException {
        String base2dName = m.group(1);
        CoordinateReferenceSystem base2d = lookupFromCache(base2dName);
        Unit<? extends Quantity> units = Unit.valueOf(m.group(2));
        Reference ref = Reference.UNKNOWN;
        if (m.group(4) != null) {
            ref = Reference.fromAbbreviation(m.group(4).toUpperCase());
        } else {
            // check if unit string matches reference level
            ref = Reference.fromAbbreviation(m.group(2).toUpperCase());
        }
        AxisDirection dir = ref.equals(Reference.PRESSURE_LEVEL) ? AxisDirection.DOWN
                : AxisDirection.UP;
        DefaultCoordinateSystemAxis axis = new DefaultCoordinateSystemAxis(
                ref.longName, dir, units);
        DefaultVerticalCS cs = new DefaultVerticalCS(axis);
        DefaultVerticalDatum datum = new DefaultVerticalDatum(ref.longName,
                DefaultVerticalDatum
                        .getVerticalDatumTypeFromLegacyCode(ref.datumType));
        DefaultVerticalCRS vertCrs = new DefaultVerticalCRS(ref.longName,
                datum, cs);
        return new DefaultCompoundCRS(m.group(0), base2d, vertCrs);
    }

    /**
     * Lookup coordinate reference system object from OGC URN or Code
     * 
     * @param crs
     * @return
     * @throws NoSuchAuthorityCodeException
     * @throws FactoryException
     * @throws OgcException
     */
    protected static CoordinateReferenceSystem lookupFromCache(String crs)
            throws NoSuchAuthorityCodeException, FactoryException, OgcException {
        if (crs.startsWith(NativeCrsAuthority.NATIVE_CRS_PREFIX)) {
            return NativeCrsFactory.lookup(crs);
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

    /**
     * Lookup coordinate reference system object from Code
     * 
     * @param crs
     * @return
     * @throws NoSuchAuthorityCodeException
     * @throws FactoryException
     */
	protected static CoordinateReferenceSystem decodeCrs(String crs)
			throws NoSuchAuthorityCodeException, FactoryException {
		if (crs.equalsIgnoreCase("epsg:900913")
				|| crs.equalsIgnoreCase("epsg:3857")) {
			return getGoogleCrs();
		}
		return CRS.decode(crs, true);
	}

    /**
     * Construct google crs, caches result. Can be called multiple times.
     * 
     * @return
     * @throws FactoryException
     */
	protected static CoordinateReferenceSystem getGoogleCrs()
			throws FactoryException {
		if (googleCrs == null) {
			googleCrs = CRS.parseWKT(GOOGLE_CRS_WKT);
		}
		return googleCrs;
	}

    /**
     * Normalize OGC URNs and Codes to be code in the following format
     * <pre>
     * [auth]:[code]
     * </pre>
     * 
     * @param crs
     * @return
     */
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

    /**
     * Construct an Extended OGC CRS URN from the composite bounds
     * 
     * @param bbox
     * @return
     */
    public static String createCrsURN(Composite3DBoundingBox bbox) {
        ReferencedEnvelope horiz = bbox.getHorizontal();
        String rval;
        if (bbox.hasNative2DCrs()) {
            rval = bbox.getNative2DCrsUrn();
        } else {
            rval = createCrsURN(horiz.getCoordinateReferenceSystem());
        }
        if (bbox.hasVertical()) {
            VerticalCoordinate vert = bbox.getVertical();
            StringBuilder sb = new StringBuilder(rval);
            sb.append("_plus_Z_in_");
            sb.append(vert.getUnits().toString());
            Reference ref = vert.getRef();
            if (!ref.equals(Reference.UNKNOWN)) {
                sb.append("_").append(ref.abbreviation);
            }
            rval = sb.toString();
        }
        return rval;
    }

    /**
     * Create an OGC CRS URN from the crs object
     * 
     * @param crs
     * @return
     */
    protected static String createCrsURN(CoordinateReferenceSystem crs) {
        ReferenceIdentifier id = crs.getIdentifiers().iterator().next();
        return String.format("urn:ogc:def:crs:%s::%s", id.getCodeSpace(),
                id.getCode());
    }

    /**
     * Return true if urn matches Extended CRS pattern
     * 
     * @param urn
     * @return
     */
    public static boolean isExtended3dCRS(String urn) {
        return EXTENDED_3D_CRS_PATTERN.matcher(urn).matches();
    }

    /**
     * Construct a crs code from authority and code
     * 
     * @param authority
     * @param code
     * @return
     */
	protected static String constructCode(String authority, String code) {
		if (!authority.equalsIgnoreCase("epsg")
				&& !authority.equalsIgnoreCase("crs")) {
			// geotools database only has epsg codes
			// try a generic crs authority
			authority = "crs";
		}
		return authority + ":" + code;
	}

    /**
     * @param crs
     * @return true if crs has EPSG as the authority and is a geographic crs
     */
    public static boolean isEpsgGeoCrs(CoordinateReferenceSystem crs) {
        try {
            String auth = crs.getName().getCodeSpace();
            return "epsg".equalsIgnoreCase(auth)
                    && crs instanceof GeographicCRS;
        } catch (NullPointerException e) {
            return false;
        }
    }

}
