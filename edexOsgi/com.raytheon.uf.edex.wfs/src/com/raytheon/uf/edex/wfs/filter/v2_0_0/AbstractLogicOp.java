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
 * Apr 21, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.filter.v2_0_0;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_2_0_0.BinaryLogicOpType;
import net.opengis.filter.v_2_0_0.ComparisonOpsType;
import net.opengis.filter.v_2_0_0.FilterType;
import net.opengis.filter.v_2_0_0.LogicOpsType;
import net.opengis.filter.v_2_0_0.SpatialOpsType;
import net.opengis.filter.v_2_0_0.TemporalOpsType;
import net.opengis.filter.v_2_0_0.UnaryLogicOpType;

/**
 * Visitor pattern support classes for logic operators
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractLogicOp {

    /**
     * @param op
     * @param visitor
     * @param obj
     * @return
     * @throws Exception
     */
    public abstract Object visit(JAXBElement<? extends LogicOpsType> op,
            IFilter2Visitor visitor, Object obj) throws Exception;

    @SuppressWarnings("unchecked")
    protected List<Filter2Processor> getProcessors(List<JAXBElement<?>> elements)
            throws Exception {
        List<Filter2Processor> rval = new ArrayList<Filter2Processor>(
                elements.size());
        for (JAXBElement<?> e : elements) {
            Class<?> type = e.getDeclaredType();
            Filter2Processor p;
            if (ComparisonOpsType.class.isAssignableFrom(type)) {
                p = Filter2Processor
                        .newFromComparison((JAXBElement<? extends ComparisonOpsType>) e);
            } else if (LogicOpsType.class.isAssignableFrom(type)) {
                p = Filter2Processor
                        .newFromLogic((JAXBElement<? extends LogicOpsType>) e);
            } else if (SpatialOpsType.class.isAssignableFrom(type)) {
                p = Filter2Processor
                        .newFromSpatial((JAXBElement<? extends SpatialOpsType>) e);
            } else if (TemporalOpsType.class.isAssignableFrom(type)) {
                p = Filter2Processor
                        .newFromTemporal((JAXBElement<? extends TemporalOpsType>) e);
            } else {
                throw new Exception("Unknown operator: " + type);
            }
            rval.add(p);
        }
        return rval;
    }

    /**
     * Create filter that is composed of the given operations. Uneeded
     * parameters can be null.
     * 
     * @param comps
     * @param spats
     * @param logic
     * @param temps
     * @return
     */
    protected FilterType createFilter(
            JAXBElement<? extends ComparisonOpsType> comps,
            JAXBElement<? extends SpatialOpsType> spats,
            JAXBElement<? extends LogicOpsType> logic,
            JAXBElement<? extends TemporalOpsType> temps) {
        FilterType rval = new FilterType();
        rval.setComparisonOps(comps);
        rval.setLogicOps(logic);
        rval.setSpatialOps(spats);
        rval.setTemporalOps(temps);
        return rval;
    }

    public static class And extends AbstractLogicOp {
        @Override
        public Object visit(JAXBElement<? extends LogicOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryLogicOpType and = (BinaryLogicOpType) op.getValue();
            List<JAXBElement<?>> gah = and
                    .getComparisonOpsOrSpatialOpsOrTemporalOps();
            List<Filter2Processor> processors = getProcessors(gah);
            return visitor.and(processors, obj);
        }
    }

    public static class Or extends AbstractLogicOp {
        @Override
        public Object visit(JAXBElement<? extends LogicOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryLogicOpType or = (BinaryLogicOpType) op.getValue();
            List<JAXBElement<?>> gah = or
                    .getComparisonOpsOrSpatialOpsOrTemporalOps();
            List<Filter2Processor> processors = getProcessors(gah);
            return visitor.or(processors, obj);
        }
    }

    public static class Not extends AbstractLogicOp {
        @Override
        public Object visit(JAXBElement<? extends LogicOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            UnaryLogicOpType not = (UnaryLogicOpType) op.getValue();
            JAXBElement<? extends ComparisonOpsType> comps = not
                    .getComparisonOps();
            JAXBElement<? extends SpatialOpsType> spats = not.getSpatialOps();
            JAXBElement<? extends LogicOpsType> logics = not.getLogicOps();
            JAXBElement<? extends TemporalOpsType> temps = not.getTemporalOps();
            FilterType filter = createFilter(comps, spats, logics, temps);
            return visitor.not(new Filter2Processor(filter), obj);
        }
    }
}
