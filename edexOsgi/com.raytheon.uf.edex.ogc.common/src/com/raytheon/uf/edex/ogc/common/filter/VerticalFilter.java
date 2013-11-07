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

import java.util.Map;

import javax.measure.unit.Unit;

import org.apache.commons.collections.map.LRUMap;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.ogc.common.spatial.AltUtil;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalEnabled;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalSpatialFactory;

/**
 * In memory data record filtering using vertical fields
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class VerticalFilter extends AbstractPdoFilter {

    public static enum VerticalOp {
        ABOVE, BELOW, BETWEEN
    };

    private final VerticalOp op;

    private final VerticalCoordinate vert;

    @SuppressWarnings("unchecked")
    private final Map<Unit<?>, VerticalCoordinate> cache = new LRUMap(2);

    /**
     * @param op
     * @param alt
     */
    public VerticalFilter(VerticalOp op, VerticalCoordinate vert) {
        this.op = op;
        this.vert = vert;
        cache.put(vert.getUnits(), vert);
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.edex.ogc.common.filter.AbstractPdoFilter#matches(com.raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @SuppressWarnings("unchecked")
    @Override
    public boolean matches(PluginDataObject pdo) {
        VerticalEnabled<PluginDataObject> enabled;
        if (pdo instanceof VerticalEnabled<?>) {
            enabled = (VerticalEnabled<PluginDataObject>) pdo;
        } else if ( (enabled = (VerticalEnabled<PluginDataObject>) VerticalSpatialFactory
                .getEnabled(pdo.getClass()))!= null) {
        } else {
            return false;
        }
        VerticalCoordinate left = enabled.getVerticalCoordinate(pdo);
        if (left == null) {
            return false;
        }
        VerticalCoordinate right;
        try {
            right = getConverted(left.getUnits(), left.getRef());
        } catch (Exception e) {
            // unable to convert
            return false;
        }
        switch (op) {
        case ABOVE:
            return left.compareTo(right) > 0;
        case BELOW:
            return left.compareTo(right) < 0;
        case BETWEEN:
            return left.compareTo(right) == 0;
        default:
            return false;
        }
    }

    private VerticalCoordinate getConverted(Unit<?> units, Reference ref) {
        VerticalCoordinate rval = cache.get(units);
        if (rval != null) {
            return rval;
        }
        rval = AltUtil.convert(units, ref, vert);
        cache.put(units, rval);
        return rval;
    }

    /**
     * @return the op
     */
    public VerticalOp getOp() {
        return op;
    }

    /**
     * @return the vert
     */
    public VerticalCoordinate getVert() {
        return vert;
    }

}
