/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wfs.filter.v2_0_0;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_2_0_0.BinaryTemporalOpType;
import net.opengis.filter.v_2_0_0.TemporalOpsType;

/**
 * Visitor pattern support classes for temporal operations
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

public abstract class AbsTemporalOp {

    /**
     * @param op
     * @param visitor
     * @param obj
     * @return
     * @throws Exception
     */
    public abstract Object visit(JAXBElement<? extends TemporalOpsType> op,
            IFilter2Visitor visitor, Object obj) throws Exception;

    /**
     * tests if time instance/range is after other time instance/range
     */
    public static class After extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.after(bin, obj);
        }
    }

    /**
     * tests if time instance/range is before other time instance/range
     */
    public static class Before extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.before(bin, obj);
        }
    }

    /**
     * tests if time instance is the start of time range
     */
    public static class Begins extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.begins(bin, obj);
        }
    }

    /**
     * tests if time instance is the end of time range
     */
    public static class Ends extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.ends(bin, obj);
        }
    }

    /**
     * tests if time range is started by time instance
     */
    public static class BegunBy extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.begunBy(bin, obj);
        }
    }

    /**
     * tests if time range contains time instance
     */
    public static class TContains extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.tContains(bin, obj);
        }
    }

    /**
     * tests if time instance is contained by time range
     */
    public static class During extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.during(bin, obj);
        }
    }

    /**
     * tests if time instance/range is equal to other time instance/range
     */
    public static class TEquals extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.tEquals(bin, obj);
        }
    }

    /**
     * tests if time range overlaps other time range
     */
    public static class TOverlaps extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.tOverlaps(bin, obj);
        }
    }

    /**
     * tests if time range is right before other time range
     */
    public static class Meets extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.meets(bin, obj);
        }
    }

    /**
     * tests if time range is overlapped by other time range
     */
    public static class OverlappedBy extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.overlappedBy(bin, obj);
        }
    }

    /**
     * tests if time range is right after other time range
     */
    public static class MetBy extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.metBy(bin, obj);
        }
    }

    /**
     * tests if time range is ended by time instance
     */
    public static class EndedBy extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.endedBy(bin, obj);
        }
    }

    /**
     * tests if time range intersects other time range
     */
    public static class AnyInteracts extends AbsTemporalOp {
        @Override
        public Object visit(JAXBElement<? extends TemporalOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinaryTemporalOpType bin = (BinaryTemporalOpType) op.getValue();
            return visitor.anyInteracts(bin, obj);
        }
    }

}
