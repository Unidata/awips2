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

package com.raytheon.uf.common.geospatial;

import org.apache.commons.collections.map.LRUMap;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.referencing.CRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.NoninvertibleTransformException;

/**
 * Keep a cache of geospatial related objects.
 * 
 * Many of these objects are expensive to create, so keep the last N (currently
 * hardcoded to 50) objects around for faster use.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 11, 2006             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class CRSCache {

    private static final int N_OBJECTS = 50;

    private static final CRSCache instance = new CRSCache();;

    private final LRUMap wktMapping = new LRUMap(N_OBJECTS);

    private final LRUMap mtMapping = new LRUMap(N_OBJECTS);

    private final LRUMap mtInvMapping = new LRUMap(N_OBJECTS);

    private final LRUMap llMapping = new LRUMap(N_OBJECTS);

    private final LRUMap revllMapping = new LRUMap(N_OBJECTS);

    private final LRUMap crsMapping = new LRUMap(N_OBJECTS);

    private final LRUMap crsStereoMapping = new LRUMap(N_OBJECTS);

    public class GGMappingPK {
        public GeneralGridGeometry gg;

        public PixelInCell pic;

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((gg == null) ? 0 : gg.hashCode());
            result = prime * result
                    + ((pic == null) ? 0 : pic.name().hashCode());
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
            GGMappingPK other = (GGMappingPK) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (gg == null) {
                if (other.gg != null)
                    return false;
            } else if (!gg.equals(other.gg))
                return false;
            if (pic == null) {
                if (other.pic != null)
                    return false;
            } else if (!pic.name().equals(other.pic.name()))
                return false;
            return true;
        }

        private CRSCache getOuterType() {
            return CRSCache.this;
        }

    }

    private class StereoCrsPK {
        public double semiMajor;

        public double semiMinor;

        public double centralMeridian;

        public double originLatitude;

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            long temp;
            temp = Double.doubleToLongBits(centralMeridian);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            temp = Double.doubleToLongBits(originLatitude);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            temp = Double.doubleToLongBits(semiMajor);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            temp = Double.doubleToLongBits(semiMinor);
            result = prime * result + (int) (temp ^ (temp >>> 32));
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            StereoCrsPK other = (StereoCrsPK) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (Double.doubleToLongBits(centralMeridian) != Double
                    .doubleToLongBits(other.centralMeridian))
                return false;
            if (Double.doubleToLongBits(originLatitude) != Double
                    .doubleToLongBits(other.originLatitude))
                return false;
            if (Double.doubleToLongBits(semiMajor) != Double
                    .doubleToLongBits(other.semiMajor))
                return false;
            if (Double.doubleToLongBits(semiMinor) != Double
                    .doubleToLongBits(other.semiMinor))
                return false;
            return true;
        }

        private CRSCache getOuterType() {
            return CRSCache.this;
        }
    }

    private class CRSMappingPK {
        public CoordinateReferenceSystem crs1;

        public CoordinateReferenceSystem crs2;

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((crs1 == null) ? 0 : crs1.hashCode());
            result = prime * result + ((crs2 == null) ? 0 : crs2.hashCode());
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
            CRSMappingPK other = (CRSMappingPK) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (crs1 == null) {
                if (other.crs1 != null)
                    return false;
            } else if (!crs1.equals(other.crs1))
                return false;
            if (crs2 == null) {
                if (other.crs2 != null)
                    return false;
            } else if (!crs2.equals(other.crs2))
                return false;
            return true;
        }

        private CRSCache getOuterType() {
            return CRSCache.this;
        }

    }

    public static CRSCache getInstance() {
        return instance;
    }

    private CRSCache() {
    }

    public CoordinateReferenceSystem getCoordinateReferenceSystem(String wkt)
            throws FactoryException {
        CoordinateReferenceSystem crs = null;
        synchronized (wktMapping) {
            crs = (CoordinateReferenceSystem) wktMapping.get(wkt);
        }
        if (crs == null) {
            crs = CRS.parseWKT(wkt);
            synchronized (wktMapping) {
                this.wktMapping.put(wkt, crs);
            }
        }

        return crs;

    }

    public MathTransform getGridToCoordinateSystem(GeneralGridGeometry gg,
            PixelInCell pic) {
        GGMappingPK pk = new GGMappingPK();
        pk.gg = gg;
        pk.pic = pic;

        MathTransform mt = null;
        synchronized (mtMapping) {
            mt = (MathTransform) mtMapping.get(pk);
        }
        if (mt == null) {
            mt = gg.getGridToCRS(pic);
            synchronized (mtMapping) {
                mtMapping.put(pk, mt);
            }
        }

        return mt;
    }

    public MathTransform getCoordinateSystemToGrid(GeneralGridGeometry gg,
            PixelInCell pic) throws NoninvertibleTransformException {
        GGMappingPK pk = new GGMappingPK();
        pk.gg = gg;
        pk.pic = pic;

        MathTransform mt = null;
        synchronized (mtInvMapping) {
            mt = (MathTransform) mtInvMapping.get(pk);
        }
        if (mt == null) {
            mt = gg.getGridToCRS(pic).inverse();
            synchronized (mtInvMapping) {
                mtInvMapping.put(pk, mt);
            }
        }

        return mt;
    }

    public MathTransform getTransformToLatLon(CoordinateReferenceSystem crs)
            throws FactoryException {
        MathTransform mt = null;
        synchronized (llMapping) {
            mt = (MathTransform) llMapping.get(crs);
        }
        if (mt == null) {
            mt = MapUtil.getTransformToLatLon(crs);
            synchronized (llMapping) {
                llMapping.put(crs, mt);
            }
        }

        return mt;
    }

    public MathTransform findMathTransform(CoordinateReferenceSystem crs1,
            CoordinateReferenceSystem crs2) throws FactoryException {
        CRSMappingPK pk = new CRSMappingPK();
        pk.crs1 = crs1;
        pk.crs2 = crs2;

        MathTransform mt = null;
        synchronized (crsMapping) {
            mt = (MathTransform) crsMapping.get(pk);
        }
        if (mt == null) {
            mt = CRS.findMathTransform(crs1, crs2);
            synchronized (crsMapping) {
                crsMapping.put(pk, mt);
            }
        }

        return mt;
    }

    public MathTransform getTransformFromLatLon(CoordinateReferenceSystem crs)
            throws FactoryException {
        MathTransform mt = null;
        synchronized (revllMapping) {
            mt = (MathTransform) revllMapping.get(crs);
        }
        if (mt == null) {
            mt = MapUtil.getTransformFromLatLon(crs);
            synchronized (revllMapping) {
                revllMapping.put(crs, mt);
            }
        }

        return mt;
    }

    /**
     * Creates a projected stereographic coordinate reference system for use
     * with radar.
     * 
     * @param majorAxis
     *            the major axis in meters
     * @param minorAxis
     *            the minor axis in meters
     * @param originLatitude
     *            latitude of origin
     * @param centralMeridian
     *            central meridian
     * 
     * @return A projected coordinate reference system
     */
    public ProjectedCRS constructStereographic(double majorAxis,
            double minorAxis, double originLatitude, double centralMeridian) {
        // create a key, check LRU map for key, if not call MapUtil.construct
        StereoCrsPK pk = new StereoCrsPK();
        pk.semiMajor = majorAxis;
        pk.semiMinor = minorAxis;
        pk.originLatitude = originLatitude;
        pk.centralMeridian = centralMeridian;
        ProjectedCRS crs = null;

        if (crsStereoMapping.containsKey(pk)) {
            synchronized (crsStereoMapping) {
                crs = (ProjectedCRS) crsStereoMapping.get(pk);
            }
        }
        if (crs == null) {
            crs = MapUtil.constructStereographic(majorAxis, minorAxis,
                    originLatitude, centralMeridian);
            synchronized (crsStereoMapping) {
                crsStereoMapping.put(pk, crs);
            }
        }

        return crs;
    }
}
