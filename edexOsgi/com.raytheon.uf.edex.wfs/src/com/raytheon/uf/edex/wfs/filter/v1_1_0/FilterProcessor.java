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
package com.raytheon.uf.edex.wfs.filter.v1_1_0;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_1_1_0.AbstractIdType;
import net.opengis.filter.v_1_1_0.BinaryComparisonOpType;
import net.opengis.filter.v_1_1_0.ComparisonOpsType;
import net.opengis.filter.v_1_1_0.FeatureIdType;
import net.opengis.filter.v_1_1_0.FilterType;
import net.opengis.filter.v_1_1_0.GmlObjectIdType;
import net.opengis.filter.v_1_1_0.LogicOpsType;
import net.opengis.filter.v_1_1_0.PropertyIsBetweenType;
import net.opengis.filter.v_1_1_0.PropertyIsLikeType;
import net.opengis.filter.v_1_1_0.PropertyIsNullType;
import net.opengis.filter.v_1_1_0.SpatialOpsType;

import com.raytheon.uf.edex.wfs.filter.v1_1_0.AbstractCompOp.BinaryOp;

/**
 * Parses OGC Filter objects using visitor pattern.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FilterProcessor {

	protected static final Map<Class<? extends ComparisonOpsType>, AbstractCompOp> compMap;
	static {
		compMap = new HashMap<Class<? extends ComparisonOpsType>, AbstractCompOp>();
		compMap.put(BinaryComparisonOpType.class, new BinaryOp());
		compMap.put(PropertyIsLikeType.class, new AbstractCompOp.Like());
		compMap.put(PropertyIsNullType.class, new AbstractCompOp.Null());
		compMap.put(PropertyIsBetweenType.class, new AbstractCompOp.Between());
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

	protected FilterType filter;

	public FilterProcessor(FilterType filter) {
		this.filter = filter;
	}

	public static FilterProcessor newFromLogic(
			JAXBElement<? extends LogicOpsType> ops) {
		FilterType f = new FilterType();
		f.setLogicOps(ops);
		return new FilterProcessor(f);
	}

	public static FilterProcessor newFromComparison(
			JAXBElement<? extends ComparisonOpsType> ops) {
		FilterType f = new FilterType();
		f.setComparisonOps(ops);
		return new FilterProcessor(f);
	}

	public static FilterProcessor newFromSpatial(
			JAXBElement<? extends SpatialOpsType> ops) {
		FilterType f = new FilterType();
		f.setSpatialOps(ops);
		return new FilterProcessor(f);
	}

	public Object accept(OgcFilterVisitor visitor, Object obj) throws Exception {
		JAXBElement<? extends ComparisonOpsType> comps = filter
				.getComparisonOps();
		JAXBElement<? extends LogicOpsType> logics = filter.getLogicOps();
		JAXBElement<? extends SpatialOpsType> spats = filter.getSpatialOps();
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
        } else if (ids != null && !ids.isEmpty()) {
            rval = visitor.id(getIds(ids), obj);
        } else {
			rval = null;
		}
		return rval;
	}

    protected List<String> getIds(
            List<JAXBElement<? extends AbstractIdType>> ids) {
        List<String> rval = new ArrayList<String>(ids.size());
        for (JAXBElement<? extends AbstractIdType> e : ids) {
            AbstractIdType id = e.getValue();
            if (id instanceof GmlObjectIdType) {
                GmlObjectIdType gml = (GmlObjectIdType) id;
                rval.add(gml.getId());
            } else if (id instanceof FeatureIdType) {
                FeatureIdType fid = (FeatureIdType) id;
                rval.add(fid.getFid());
            }
        }
        return rval;
    }

}
