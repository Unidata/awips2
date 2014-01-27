/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.filter;

import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Map;

import org.apache.commons.collections.map.LRUMap;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * In memory data record filtering using spatial fields
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SpatialFilter extends AbstractPdoFilter {

    public static enum SpatialOp {
        EQUALS, CONTAINS, COVERS, COVEREDBY, CROSSES, DISJOINT, INTERSECTS, OVERLAPS, TOUCHES, WITHIN
    };

	protected final Geometry geometry;

	protected final SpatialOp op;
    
    private static final Polygon CRS84_BOUNDS;
    
	private static final GeometryFactory geomFact = new GeometryFactory();

	@SuppressWarnings("unchecked")
	private final Map<String, Boolean> cache = new LRUMap(32);

    static {
		Coordinate[] coords = new Coordinate[5];
		coords[0] = new Coordinate(-180, 90);
		coords[1] = new Coordinate(180, 90);
		coords[2] = new Coordinate(180, -90);
		coords[3] = new Coordinate(-180, -90);
		coords[4] = coords[0];
		LinearRing shell = geomFact.createLinearRing(coords);
		CRS84_BOUNDS = geomFact.createPolygon(shell, new LinearRing[0]);
    }

    /**
	 * Geometry is assumed to be same CRS as PDO (CRS:84)
	 * 
	 * @param geometry
	 * @param op
	 */
    public SpatialFilter(SpatialOp op, Geometry geometry) {
        this.geometry = geometry;
        this.op = op;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ogc.common.filter.AbstractFilterOp#matches(com.raytheon
     * .uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public boolean matches(PluginDataObject pdo) {
		boolean rval = true;
        if (geometry == null) {
			return rval;
        }
		ISpatialObject spat;
        if (pdo instanceof ISpatialEnabled) {
			spat = ((ISpatialEnabled) pdo).getSpatialObject();
        } else if (pdo instanceof ISpatialObject) {
			spat = (ISpatialObject) pdo;
		} else {
			return rval;
		}
		String cacheKey = getCacheKey(spat);
		synchronized (cache) {
			Boolean cachedResult = cache.get(cacheKey);
			if (cachedResult != null) {
				return cachedResult;
			}
		}
		if (!matchSpatial(spat)) {
			rval = false;
        }
		synchronized (cache) {
			cache.put(cacheKey, rval);
		}
		return rval;
    }

	/**
	 * Generate unique key for spatial object
	 * 
	 * @param spat
	 * @return
	 */
	private static String getCacheKey(ISpatialObject spat) {
        CoordinateReferenceSystem crs = spat.getCrs();
        String rval = (crs == null ? "" : crs.toWKT());
        Geometry geom = spat.getGeometry();
        if (geom != null) {
            rval = rval + geom.toText();
        }
        return rval;
	}

	/**
	 * Get geometries from internal bounds. Takes into account any needed
	 * adjustments to map to OGC bounds
	 * 
	 * Assumes any geographic geometry is Lon/Lat order
	 * 
	 * @param spat
	 * @return
	 */
	private static Geometry[] getGeometries(ISpatialObject spatial) {
		// we don't support wrapping over the poles
		Geometry copy = truncateYAxis(spatial.getGeometry());
		// create multiple geometries that all fit inside CRS84 bounds
		Geometry diff = copy.difference(CRS84_BOUNDS);
		Geometry[] rval = new Geometry[diff.getNumGeometries() + 1];
		rval[0] = copy.intersection(CRS84_BOUNDS);
		for (int i = 0; i < diff.getNumGeometries(); ++i) {
			rval[i + 1] = convertXAxis(diff.getGeometryN(i));
		}
		return merge(rval);
	}

	/**
	 * Attempt to merge any intersecting geometries
	 * 
	 * @param geoms
	 * @return
	 */
	private static Geometry[] merge(Geometry[] geoms) {
		if (geoms.length < 2) {
			return geoms;
		}
		LinkedList<Geometry> blobs = new LinkedList<Geometry>();
		blobs.add(geoms[0]);
		for (int i = 1; i < geoms.length; ++i) {
			Geometry geom = geoms[i];
			ListIterator<Geometry> iter = blobs.listIterator();
			while (iter.hasNext()) {
				Geometry blob = iter.next();
				if (blob.intersects(geom)) {
					geom = blob.union(geom);
					iter.remove();
				}
			}
			blobs.add(geom);
		}
		return blobs.toArray(new Geometry[blobs.size()]);
	}

	/**
	 * Truncate geometry Y coordinates to fit between -90 and 90
	 * 
	 * @param geom
	 */
	private static Geometry truncateYAxis(Geometry geom) {
		Geometry rval = (Geometry) geom.clone();
		rval.apply(new CoordinateFilter() {
			@Override
			public void filter(Coordinate coord) {
				if (coord.y > 90) {
					coord.y = 90;
				} else if (coord.y < -90) {
					coord.y = -90;
				}
			}
		});
		rval.geometryChanged();
		return rval;
	}

	/**
	 * Convert geometry X coordinates to fit between -180 and 180
	 * 
	 * @param geom
	 * @return
	 */
	private static Geometry convertXAxis(Geometry geom) {
		Geometry rval = (Geometry) geom.clone();
		rval.apply(new CoordinateFilter() {
			@Override
			public void filter(Coordinate coord) {
				coord.x = ((coord.x + 180) % 360) - 180;
			}
		});
		geom.geometryChanged();
		return rval;
	}

	/**
	 * Return true if spatial object matches the geographic operator
	 * 
	 * @param spat
	 * @return
	 */
	private boolean matchSpatial(ISpatialObject spat) {
        if (spat.getGeometry() == null) {
            return false;
        }
		Geometry[] geometries = getGeometries(spat);
		return matchGeom(geometries);
	}

	protected static interface ForEach {
		public boolean eval(Geometry other);
	}

	/**
	 * Return true if geometry matches the geographic operator
	 * 
	 * @param other
	 * @return
	 */
	private boolean matchGeom(Geometry[] others) {
        switch (op) {
        case EQUALS:
        	return all(new ForEach() {
				public boolean eval(Geometry other) {
					return other.equals(geometry);
				}
			}, others);
        case CONTAINS:
			return any(new ForEach() {
				public boolean eval(Geometry other) {
					return other.contains(geometry);
				}
			}, others);
        case COVERS:
			return any(new ForEach() {
				public boolean eval(Geometry other) {
					return other.covers(geometry);
				}
			}, others);
        case COVEREDBY:
			return all(new ForEach() {
				public boolean eval(Geometry other) {
					return other.coveredBy(geometry);
				}
			}, others);
        case CROSSES:
			return any(new ForEach() {
				public boolean eval(Geometry other) {
					return other.crosses(geometry);
				}
			}, others);
        case DISJOINT:
			return all(new ForEach() {
				public boolean eval(Geometry other) {
					return other.disjoint(geometry);
				}
			}, others);
        case INTERSECTS:
			return any(new ForEach() {
				public boolean eval(Geometry other) {
					return other.intersects(geometry);
				}
			}, others);
        case OVERLAPS:
			return any(new ForEach() {
				public boolean eval(Geometry other) {
					return other.overlaps(geometry);
				}
			}, others);
        case TOUCHES:
			return any(new ForEach() {
				public boolean eval(Geometry other) {
					return other.touches(geometry);
				}
			}, others);
        case WITHIN:
			return all(new ForEach() {
				public boolean eval(Geometry other) {
					return other.within(geometry);
				}
			}, others);
        default:
			return true;
        }
    }

	/**
	 * Return true if any of others evaluate true
	 * 
	 * @param fe
	 * @param others
	 * @return
	 */
	private boolean any(ForEach fe, Geometry[] others) {
		for (Geometry other : others) {
			if (fe.eval(other)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Return true if all of other evaluate true
	 * 
	 * @param fe
	 * @param others
	 * @return
	 */
	private boolean all(ForEach fe, Geometry[] others) {
		for (Geometry other : others) {
			if (!fe.eval(other)) {
				return false;
			}
		}
		return true;
	}

    /**
     * @return the geometry
     */
    public Geometry getGeometry() {
        return geometry;
    }

    /**
     * @return the op
     */
    public SpatialOp getOp() {
        return op;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "[geometry " + op + " " + geometry + "]";
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((geometry == null) ? 0 : geometry.toText().hashCode());
        result = prime * result + ((op == null) ? 0 : op.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SpatialFilter other = (SpatialFilter) obj;
        if (geometry == null) {
            if (other.geometry != null)
                return false;
        } else if (!geometry.equals(other.geometry))
            return false;
        if (op != other.op)
            return false;
        return true;
    }

}
