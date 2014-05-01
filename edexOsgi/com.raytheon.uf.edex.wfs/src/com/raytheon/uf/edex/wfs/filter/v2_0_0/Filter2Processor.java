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
package com.raytheon.uf.edex.wfs.filter.v2_0_0;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_2_0_0.AbstractIdType;
import net.opengis.filter.v_2_0_0.BinaryComparisonOpType;
import net.opengis.filter.v_2_0_0.ComparisonOpsType;
import net.opengis.filter.v_2_0_0.FilterType;
import net.opengis.filter.v_2_0_0.LogicOpsType;
import net.opengis.filter.v_2_0_0.PropertyIsBetweenType;
import net.opengis.filter.v_2_0_0.PropertyIsLikeType;
import net.opengis.filter.v_2_0_0.PropertyIsNilType;
import net.opengis.filter.v_2_0_0.PropertyIsNullType;
import net.opengis.filter.v_2_0_0.SpatialOpsType;
import net.opengis.filter.v_2_0_0.TemporalOpsType;

import com.raytheon.uf.edex.wfs.filter.v2_0_0.AbstractCompOp.Between;
import com.raytheon.uf.edex.wfs.filter.v2_0_0.AbstractCompOp.BinaryOp;
import com.raytheon.uf.edex.wfs.filter.v2_0_0.AbstractCompOp.Like;
import com.raytheon.uf.edex.wfs.filter.v2_0_0.AbstractCompOp.Nil;
import com.raytheon.uf.edex.wfs.filter.v2_0_0.AbstractCompOp.Null;

/**
 * Top level visitor pattern support class for filters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class Filter2Processor {

    protected static final Map<Class<? extends ComparisonOpsType>, AbstractCompOp> compMap;
    static {
        compMap = new HashMap<Class<? extends ComparisonOpsType>, AbstractCompOp>();
        compMap.put(BinaryComparisonOpType.class, new BinaryOp());
        compMap.put(PropertyIsLikeType.class, new Like());
        compMap.put(PropertyIsNullType.class, new Null());
        compMap.put(PropertyIsNilType.class, new Nil());
        compMap.put(PropertyIsBetweenType.class, new Between());
    }

    protected static final Map<String, AbstractLogicOp> logicMap;
    static {
        logicMap = new HashMap<String, AbstractLogicOp>();
        logicMap.put("And", new AbstractLogicOp.And());
        logicMap.put("Or", new AbstractLogicOp.Or());
        logicMap.put("Not", new AbstractLogicOp.Not());
    }

    protected static final Map<String, AbstractSpatialOp> spatialMap;
    static {
        spatialMap = new HashMap<String, AbstractSpatialOp>();
        spatialMap.put("Equals", new AbstractSpatialOp.SpatialEquals());
        spatialMap.put("Disjoint", new AbstractSpatialOp.Disjoint());
        spatialMap.put("Touches", new AbstractSpatialOp.Touches());
        spatialMap.put("Within", new AbstractSpatialOp.Within());
        spatialMap.put("Overlaps", new AbstractSpatialOp.Overlaps());
        spatialMap.put("Crosses", new AbstractSpatialOp.Crosses());
        spatialMap.put("Intersects", new AbstractSpatialOp.Intersects());
        spatialMap.put("Contains", new AbstractSpatialOp.Contains());
        spatialMap.put("DWithin", new AbstractSpatialOp.DWithin());
        spatialMap.put("Beyond", new AbstractSpatialOp.Beyond());
        spatialMap.put("BBOX", new AbstractSpatialOp.Bbox());
    }

    protected static final Map<String, AbsTemporalOp> temporalMap;
    static {
        temporalMap = new HashMap<String, AbsTemporalOp>();
        temporalMap.put("After", new AbsTemporalOp.After());
        temporalMap.put("Before", new AbsTemporalOp.Before());
        temporalMap.put("Begins", new AbsTemporalOp.Begins());
        temporalMap.put("BegunBy", new AbsTemporalOp.BegunBy());
        temporalMap.put("TContains", new AbsTemporalOp.TContains());
        temporalMap.put("During", new AbsTemporalOp.During());
        temporalMap.put("EndedBy", new AbsTemporalOp.EndedBy());
        temporalMap.put("Ends", new AbsTemporalOp.Ends());
        temporalMap.put("TEquals", new AbsTemporalOp.TEquals());
        temporalMap.put("Meets", new AbsTemporalOp.Meets());
        temporalMap.put("MetBy", new AbsTemporalOp.MetBy());
        temporalMap.put("TOverlaps", new AbsTemporalOp.TOverlaps());
        temporalMap.put("OverlappedBy", new AbsTemporalOp.OverlappedBy());
        temporalMap.put("AnyInteracts", new AbsTemporalOp.AnyInteracts());
    }

    protected FilterType filter;

    /**
     * @param filter
     *            filter to process
     */
    public Filter2Processor(FilterType filter) {
        this.filter = filter;
    }

    /**
     * Utility method to create processor from logic operators
     * 
     * @param ops
     * @return
     */
    public static Filter2Processor newFromLogic(
            JAXBElement<? extends LogicOpsType> ops) {
        FilterType f = new FilterType();
        f.setLogicOps(ops);
        return new Filter2Processor(f);
    }

    /**
     * Utility method to create processor from comparison operators
     * 
     * @param ops
     * @return
     */
    public static Filter2Processor newFromComparison(
            JAXBElement<? extends ComparisonOpsType> ops) {
        FilterType f = new FilterType();
        f.setComparisonOps(ops);
        return new Filter2Processor(f);
    }

    /**
     * Utility method to create processor from spatial operators
     * 
     * @param ops
     * @return
     */
    public static Filter2Processor newFromSpatial(
            JAXBElement<? extends SpatialOpsType> ops) {
        FilterType f = new FilterType();
        f.setSpatialOps(ops);
        return new Filter2Processor(f);
    }

    /**
     * Utility method to create processor from temporal operators
     * 
     * @param ops
     * @return
     */
    public static Filter2Processor newFromTemporal(
            JAXBElement<? extends TemporalOpsType> ops) {
        FilterType f = new FilterType();
        f.setTemporalOps(ops);
        return new Filter2Processor(f);
    }

    /**
     * Entry point for visitor
     * 
     * @param visitor
     * @param obj
     * @return
     * @throws Exception
     */
    public Object accept(IFilter2Visitor visitor, Object obj) throws Exception {
        JAXBElement<? extends ComparisonOpsType> comps = filter
                .getComparisonOps();
        JAXBElement<? extends LogicOpsType> logics = filter.getLogicOps();
        JAXBElement<? extends SpatialOpsType> spats = filter.getSpatialOps();
        JAXBElement<? extends TemporalOpsType> temps = filter.getTemporalOps();
        List<JAXBElement<? extends AbstractIdType>> ids = filter.getId();
        Object rval;
        if (logics != null && !logics.isNil()) {
            String name = logics.getName().getLocalPart();
            AbstractLogicOp op = logicMap.get(name);
            rval = op.visit(logics, visitor, obj);
        } else if (comps != null && !comps.isNil()) {
            Class<? extends ComparisonOpsType> type = comps.getDeclaredType();
            AbstractCompOp op = compMap.get(type);
            rval = op.visit(comps, visitor, obj);
        } else if (spats != null && !spats.isNil()) {
            String name = spats.getName().getLocalPart();
            AbstractSpatialOp op = spatialMap.get(name);
            rval = op.visit(spats, visitor, obj);
        } else if (temps != null && !temps.isNil()) {
            String name = temps.getName().getLocalPart();
            AbsTemporalOp op = temporalMap.get(name);
            rval = op.visit(temps, visitor, obj);
        } else if (ids != null && !ids.isEmpty()) {
            List<AbstractIdType> bareIds = new ArrayList<AbstractIdType>(
                    ids.size());
            for (JAXBElement<? extends AbstractIdType> idElem : ids) {
                bareIds.add(idElem.getValue());
            }
            rval = visitor.id(bareIds, obj);
        } else {
            rval = null;
        }
        return rval;
    }

}
