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

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_2_0_0.BBOXType;
import net.opengis.filter.v_2_0_0.BinarySpatialOpType;
import net.opengis.filter.v_2_0_0.DistanceBufferType;
import net.opengis.filter.v_2_0_0.SpatialOpsType;

/**
 * Visitor pattern support classes for spatial operations
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
public abstract class AbstractSpatialOp {

    /**
     * @param op
     * @param visitor
     * @param obj
     * @return
     * @throws Exception
     */
    public abstract Object visit(JAXBElement<? extends SpatialOpsType> op,
            IFilter2Visitor visitor, Object obj) throws Exception;

    public static class SpatialEquals extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
            return visitor.spatialEquals(binary, obj);
        }
    }

    public static class Disjoint extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
            return visitor.disjoint(binary, obj);
        }
    }

    public static class Touches extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
            return visitor.touches(binary, obj);
        }
    }

    public static class Within extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
            return visitor.within(binary, obj);
        }
    }

    public static class Overlaps extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
            return visitor.overlaps(binary, obj);
        }
    }

    public static class Crosses extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
            return visitor.crosses(binary, obj);
        }
    }

    public static class Intersects extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
            return visitor.intersects(binary, obj);
        }
    }

    public static class Contains extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
            return visitor.contains(binary, obj);
        }
    }

    /**
     * distance within
     */
    public static class DWithin extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            DistanceBufferType dist = (DistanceBufferType) op.getValue();
            return visitor.dWithin(dist, obj);
        }
    }

    public static class Beyond extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            DistanceBufferType dist = (DistanceBufferType) op.getValue();
            return visitor.beyond(dist, obj);
        }
    }

    /**
     * bounding box
     */
    public static class Bbox extends AbstractSpatialOp {
        @Override
        public Object visit(JAXBElement<? extends SpatialOpsType> op,
                IFilter2Visitor visitor, Object obj) throws Exception {
            BBOXType bbox = (BBOXType) op.getValue();
            return visitor.bbox(bbox, obj);
        }
    }
}
