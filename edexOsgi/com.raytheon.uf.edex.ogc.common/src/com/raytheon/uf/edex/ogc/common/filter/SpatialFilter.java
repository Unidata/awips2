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

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.vividsolutions.jts.geom.Geometry;

/**
 * TODO Add Description
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

    protected Geometry geometry;

    protected SpatialOp op;

    /**
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
        if (geometry == null) {
            return true;
        }
        if (pdo instanceof ISpatialEnabled) {
            if (!matchSpatial(((ISpatialEnabled) pdo).getSpatialObject())) {
                return false;
            }
        } else if (pdo instanceof ISpatialObject) {
            if (!matchSpatial((ISpatialObject) pdo)) {
                return false;
            }
        }
        return true;
    }

    private boolean matchSpatial(ISpatialObject spat) {
        Geometry other = spat.getGeometry();
        boolean rval;
        switch (op) {
        case EQUALS:
            rval = other.equals(this.geometry);
            break;
        case CONTAINS:
            rval = other.contains(this.geometry);
            break;
        case COVERS:
            rval = other.covers(this.geometry);
            break;
        case COVEREDBY:
            rval = other.coveredBy(this.geometry);
            break;
        case CROSSES:
            rval = other.crosses(this.geometry);
            break;
        case DISJOINT:
            rval = other.disjoint(this.geometry);
            break;
        case INTERSECTS:
            rval = other.intersects(this.geometry);
            break;
        case OVERLAPS:
            rval = other.overlaps(this.geometry);
            break;
        case TOUCHES:
            rval = other.touches(this.geometry);
            break;
        case WITHIN:
            rval = other.within(this.geometry);
            break;
        default:
            rval = true;
            break;
        }
        return rval;
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
