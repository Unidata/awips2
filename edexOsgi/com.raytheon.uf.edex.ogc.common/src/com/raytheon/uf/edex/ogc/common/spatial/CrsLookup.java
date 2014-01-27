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
package com.raytheon.uf.edex.ogc.common.spatial;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.quantity.Quantity;
import javax.measure.unit.SI;
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

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;

/**
 * Coordinate Reference System utility methods and constants. Used to parse CRS
 * codes and URNs to geotools objects and format geotools objects for output.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2012            bclement     Initial creation
 * 
 * </pre>
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
        return create3d(m.group(0), base2d, units, ref);
    }

    /**
     * Create 3D CRS from horizontal and vertical components
     * 
     * @param base2d
     * @param vertUnits
     * @param vertRef
     * @return
     */
    protected static CoordinateReferenceSystem create3d(String name,
            CoordinateReferenceSystem base2d, Unit<?> vertUnits,
            Reference vertRef) {
        AxisDirection dir = vertRef.equals(Reference.PRESSURE_LEVEL) ? AxisDirection.DOWN
                : AxisDirection.UP;
        DefaultCoordinateSystemAxis axis = new DefaultCoordinateSystemAxis(
                vertRef.longName, dir, vertUnits);
        DefaultVerticalCS cs = new DefaultVerticalCS(axis);
        DefaultVerticalDatum datum = new DefaultVerticalDatum(vertRef.longName,
                DefaultVerticalDatum
                        .getVerticalDatumTypeFromLegacyCode(vertRef.datumType));
        DefaultVerticalCRS vertCrs = new DefaultVerticalCRS(vertRef.longName,
                datum, cs);
        return new DefaultCompoundCRS(name, base2d, vertCrs);
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
        if (crs.equalsIgnoreCase("epsg:4979")) {
            return create3d(crs, MapUtil.LATLON_PROJECTION, SI.METER,
                    Reference.ABOVE_ELLIPSOID);
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
            // unknown form, try it anyway
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
        String codeSpace = id.getCodeSpace();
        String code = id.getCode();
        if (codeSpace.equalsIgnoreCase("crs") && code.equalsIgnoreCase("84")) {
            return "urn:ogc:def:crs:OGC:2:84";
        }
        return String.format("urn:ogc:def:crs:%s::%s", codeSpace, code);
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
