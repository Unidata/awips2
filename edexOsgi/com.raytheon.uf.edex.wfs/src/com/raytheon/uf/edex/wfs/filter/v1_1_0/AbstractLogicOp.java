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

import net.opengis.filter.v_1_1_0.BinaryLogicOpType;
import net.opengis.filter.v_1_1_0.ComparisonOpsType;
import net.opengis.filter.v_1_1_0.FilterType;
import net.opengis.filter.v_1_1_0.LogicOpsType;
import net.opengis.filter.v_1_1_0.SpatialOpsType;
import net.opengis.filter.v_1_1_0.UnaryLogicOpType;

/**
 * OGC Filter parsing for logic operators.
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
public abstract class AbstractLogicOp {

	public abstract Object visit(JAXBElement<? extends LogicOpsType> op,
			OgcFilterVisitor visitor, Object obj) throws Exception;

	protected Map<Class<?>, List<JAXBElement<?>>> sortByType(
			List<JAXBElement<?>> elements) {
		Map<Class<?>, List<JAXBElement<?>>> rval = new HashMap<Class<?>, List<JAXBElement<?>>>();
		for (JAXBElement<?> e : elements) {
			List<JAXBElement<?>> list = rval.get(e.getDeclaredType());
			if (list == null) {
				list = new ArrayList<JAXBElement<?>>();
				rval.put(e.getDeclaredType(), list);
			}
			list.add(e);
		}
		return rval;
	}

	@SuppressWarnings("unchecked")
	protected List<FilterProcessor> getProcessors(List<JAXBElement<?>> elements)
			throws Exception {
		List<FilterProcessor> rval = new ArrayList<FilterProcessor>(
				elements.size());
		for (JAXBElement<?> e : elements) {
			Class<?> type = e.getDeclaredType();
			FilterProcessor p;
			// FIXME this is slow
			if (ComparisonOpsType.class.isAssignableFrom(type)) {
				p = FilterProcessor
						.newFromComparison((JAXBElement<? extends ComparisonOpsType>) e);
			} else if (LogicOpsType.class.isAssignableFrom(type)) {
				p = FilterProcessor
						.newFromLogic((JAXBElement<? extends LogicOpsType>) e);
			} else if (SpatialOpsType.class.isAssignableFrom(type)) {
				p = FilterProcessor
						.newFromSpatial((JAXBElement<? extends SpatialOpsType>) e);
			} else {
				throw new Exception("Unknown operator: " + type);
			}
			rval.add(p);
		}
		return rval;
	}

	protected FilterType createFilter(
			JAXBElement<? extends ComparisonOpsType> comps,
			JAXBElement<? extends SpatialOpsType> spats,
			JAXBElement<? extends LogicOpsType> logic) {
		FilterType rval = new FilterType();
		rval.setComparisonOps(comps);
		rval.setLogicOps(logic);
		rval.setSpatialOps(spats);
		return rval;
	}

	public static class And extends AbstractLogicOp {
		@Override
		public Object visit(JAXBElement<? extends LogicOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinaryLogicOpType and = (BinaryLogicOpType) op.getValue();
			List<JAXBElement<?>> gah = and
					.getComparisonOpsOrSpatialOpsOrLogicOps();
			List<FilterProcessor> processors = getProcessors(gah);
			return visitor.and(processors, obj);
		}
	}

	public static class Or extends AbstractLogicOp {
		@Override
		public Object visit(JAXBElement<? extends LogicOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinaryLogicOpType or = (BinaryLogicOpType) op.getValue();
			List<JAXBElement<?>> gah = or
					.getComparisonOpsOrSpatialOpsOrLogicOps();
			List<FilterProcessor> processors = getProcessors(gah);
			return visitor.or(processors, obj);
		}
	}

	public static class Not extends AbstractLogicOp {
		@Override
		public Object visit(JAXBElement<? extends LogicOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			UnaryLogicOpType not = (UnaryLogicOpType) op.getValue();
			JAXBElement<? extends ComparisonOpsType> comps = not
					.getComparisonOps();
			JAXBElement<? extends SpatialOpsType> spats = not.getSpatialOps();
			JAXBElement<? extends LogicOpsType> logics = not.getLogicOps();
			FilterType filter = createFilter(comps, spats, logics);
			return visitor.not(new FilterProcessor(filter), obj);
		}
	}
}
