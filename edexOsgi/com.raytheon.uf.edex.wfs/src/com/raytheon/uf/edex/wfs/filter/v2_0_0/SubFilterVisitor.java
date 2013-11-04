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

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_2_0_0.AbstractIdType;
import net.opengis.filter.v_2_0_0.BBOXType;
import net.opengis.filter.v_2_0_0.BinarySpatialOpType;
import net.opengis.filter.v_2_0_0.BinaryTemporalOpType;
import net.opengis.filter.v_2_0_0.DistanceBufferType;
import net.opengis.filter.v_2_0_0.PropertyIsLikeType;
import net.opengis.filter.v_2_0_0.PropertyIsNilType;
import net.opengis.filter.v_2_0_0.PropertyIsNullType;
import net.opengis.filter.v_2_0_0.ResourceIdType;
import net.opengis.gml.v_3_2_1.EnvelopeType;

import org.geotools.geometry.jts.JTS;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.ogc.common.filter.AbstractPdoFilter;
import com.raytheon.uf.edex.ogc.common.filter.ComparisonFilter;
import com.raytheon.uf.edex.ogc.common.filter.ComparisonFilter.CompOp;
import com.raytheon.uf.edex.ogc.common.filter.LogicFilter;
import com.raytheon.uf.edex.ogc.common.filter.SpatialFilter;
import com.raytheon.uf.edex.ogc.common.filter.SpatialFilter.SpatialOp;
import com.raytheon.uf.edex.ogc.common.filter.TemporalFilter;
import com.raytheon.uf.edex.ogc.common.filter.TemporalFilter.TimeOp;
import com.raytheon.uf.edex.ogc.common.filter.VerticalFilter;
import com.raytheon.uf.edex.ogc.common.filter.VerticalFilter.VerticalOp;
import com.raytheon.uf.edex.ogc.common.gml3_2_1.EnvelopeConverter;
import com.raytheon.uf.edex.ogc.common.spatial.BoundingBoxUtil;
import com.raytheon.uf.edex.ogc.common.spatial.Composite3DBoundingBox;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.provider.VisitorBag;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

/**
 * visitor for parsing filters into subscription filters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SubFilterVisitor extends AbstractQueryFilterVisitor {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#equal(com.raytheon.uf.edex
     * .filter.ExpressionProcessor,
     * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
     * java.lang.Object)
     */
    @Override
    public AbstractPdoFilter equal(ExpressionProcessor left,
            ExpressionProcessor right, boolean matchCase, Object obj)
            throws Exception {
        VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);
        
        if(operand == null) {
            return AbstractPdoFilter.noFilter();
        }
        
        if (operand.sql) {
            // subscriptions do not go to the DB, if we need sql, likely from a
            // function, throw exception
            throw new Exception(
                    "SQL filtering such as functions not supported for subscriptions.");
        }
        return new ComparisonFilter((String) operand.left, CompOp.EQ,
                operand.right);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#notEqual(com.raytheon.uf
     * .edex.filter.ExpressionProcessor,
     * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
     * java.lang.Object)
     */
    @Override
    public AbstractPdoFilter notEqual(ExpressionProcessor left,
            ExpressionProcessor right, boolean matchCase, Object obj)
            throws Exception {
        VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);
        
        if(operand == null) {
            return AbstractPdoFilter.noFilter();
        }
        
        if (operand.sql) {
            // subscriptions do not go to the DB, if we need sql, likely from a
            // function, throw exception
            throw new Exception(
                    "SQL filtering such as functions not supported for subscriptions.");
        }
        return new ComparisonFilter((String) operand.left, CompOp.NEQ,
                operand.right);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#lessThan(com.raytheon.uf
     * .edex.filter.ExpressionProcessor,
     * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
     * java.lang.Object)
     */
    @Override
    public AbstractPdoFilter lessThan(ExpressionProcessor left,
            ExpressionProcessor right, boolean matchCase, Object obj)
            throws Exception {
        VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);
        
        if(operand == null) {
            return AbstractPdoFilter.noFilter();
        }
        
        if (operand.sql) {
            // subscriptions do not go to the DB, if we need sql, likely from a
            // function, throw exception
            throw new Exception(
                    "SQL filtering such as functions not supported for subscriptions.");
        }
        return new ComparisonFilter((String) operand.left, CompOp.LT,
                operand.right);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#greaterThan(com.raytheon
     * .uf.edex.filter.ExpressionProcessor,
     * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
     * java.lang.Object)
     */
    @Override
    public AbstractPdoFilter greaterThan(ExpressionProcessor left,
            ExpressionProcessor right, boolean matchCase, Object obj)
            throws Exception {
        VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);
        if (operand == null) {
            return AbstractPdoFilter.noFilter();
        }
        if (operand.sql) {
            // subscriptions do not go to the DB, if we need sql, likely from a
            // function, throw exception
            throw new Exception(
                    "SQL filtering such as functions not supported for subscriptions.");
        }
        return new ComparisonFilter((String) operand.left, CompOp.GT,
                operand.right);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#greaterThanEqual(com.raytheon
     * .uf.edex.filter.ExpressionProcessor,
     * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
     * java.lang.Object)
     */
    @Override
    public AbstractPdoFilter greaterThanEqual(ExpressionProcessor left,
            ExpressionProcessor right, boolean matchCase, Object obj)
            throws Exception {
        VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);
        
        if(operand == null) {
            return AbstractPdoFilter.noFilter();
        }
        
        if (operand.sql) {
            // subscriptions do not go to the DB, if we need sql, likely from a
            // function, throw exception
            throw new Exception(
                    "SQL filtering such as functions not supported for subscriptions.");
        }
        return new ComparisonFilter((String) operand.left, CompOp.GTE,
                operand.right);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#lessThanEqual(com.raytheon
     * .uf.edex.filter.ExpressionProcessor,
     * com.raytheon.uf.edex.filter.ExpressionProcessor, boolean,
     * java.lang.Object)
     */
    @Override
    public AbstractPdoFilter lessThanEqual(ExpressionProcessor left,
            ExpressionProcessor right, boolean matchCase, Object obj)
            throws Exception {
        VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);
        
        if(operand == null) {
            return AbstractPdoFilter.noFilter();
        }
        
        if (operand.sql) {
            // subscriptions do not go to the DB, if we need sql, likely from a
            // function, throw exception
            throw new Exception(
                    "SQL filtering such as functions not supported for subscriptions.");
        }
        return new ComparisonFilter((String) operand.left, CompOp.LTE,
                operand.right);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#isLike(net.opengis.filter
     * .v_1_1_0.PropertyIsLikeType, java.lang.Object)
     */
    @Override
    public ComparisonFilter isLike(PropertyIsLikeType op, Object obj)
            throws Exception {
        // TODO
        throw new Exception("isLike filter not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#isNull(net.opengis.filter
     * .v_1_1_0.PropertyIsNullType, java.lang.Object)
     */
    @Override
    public AbstractPdoFilter isNull(PropertyIsNullType op, Object obj)
            throws Exception {
        ExpressionProcessor eproc = new ExpressionProcessor(op.getExpression());
        String field = getRef(eproc, (VisitorBag) obj).value;
        if (field == null) {
            return AbstractPdoFilter.noFilter();
        }
        return ComparisonFilter.isNull(field);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#between(com.raytheon.uf.
     * edex.filter.ExpressionProcessor,
     * com.raytheon.uf.edex.filter.ExpressionProcessor,
     * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
     */
    @Override
    public AbstractPdoFilter between(ExpressionProcessor lower,
            ExpressionProcessor exp, ExpressionProcessor upper, Object obj)
            throws Exception {
        VisitorBag bag = (VisitorBag) obj;

        Operand lowerPart = getBinaryProps(exp, lower, bag);
        Operand upperPart = getBinaryProps(exp, upper, bag);

        if(lowerPart == null || upperPart == null) {
            return AbstractPdoFilter.noFilter();
        }
        
        if (lowerPart.sql || upperPart.sql) {
            // subscriptions do not go to the DB, if we need sql, likely from a
            // function, throw exception
            throw new Exception(
                    "SQL filtering such as functions not supported for subscriptions.");
        }

        ComparisonFilter lowerFilter = new ComparisonFilter(
                (String) lowerPart.left, CompOp.GT, lowerPart.right);
        ComparisonFilter upperFilter = new ComparisonFilter(
                (String) upperPart.left, CompOp.LT, upperPart.right);
        return LogicFilter.and(lowerFilter, upperFilter);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.filter.OgcFilterVisitor#and(java.util.List,
     * java.lang.Object)
     */
    @Override
    public LogicFilter and(List<Filter2Processor> filters, Object obj)
            throws Exception {
        return LogicFilter.and(acceptAll(filters, obj));
    }

    protected AbstractPdoFilter[] acceptAll(List<Filter2Processor> filters,
            Object obj) throws Exception {
        AbstractPdoFilter[] rval = new AbstractPdoFilter[filters.size()];
        Iterator<Filter2Processor> iter = filters.iterator();
        for (int i = 0; iter.hasNext(); ++i) {
            rval[i] = (AbstractPdoFilter) iter.next().accept(this, obj);
        }
        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.filter.OgcFilterVisitor#or(java.util.List,
     * java.lang.Object)
     */
    @Override
    public LogicFilter or(List<Filter2Processor> filters, Object obj)
            throws Exception {
        return LogicFilter.or(acceptAll(filters, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#not(com.raytheon.uf.edex
     * .filter.FilterProcessor, java.lang.Object)
     */
    @Override
    public LogicFilter not(Filter2Processor filter, Object obj)
            throws Exception {
        return LogicFilter.not((AbstractPdoFilter) filter.accept(this, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#spatialEquals(net.opengis
     * .filter.v_1_1_0.BinarySpatialOpType, java.lang.Object)
     */
    @Override
    public SpatialFilter spatialEquals(BinarySpatialOpType op, Object obj)
            throws Exception {
        VisitorBag bag = (VisitorBag) obj;
        Entry<String, Geometry> e = getBinarySpatial(op, bag);
        return new SpatialFilter(SpatialOp.EQUALS, e.getValue());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#disjoint(net.opengis.filter
     * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
     */
    @Override
    public SpatialFilter disjoint(BinarySpatialOpType op, Object obj)
            throws Exception {
        Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
        return new SpatialFilter(SpatialOp.DISJOINT, e.getValue());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#touches(net.opengis.filter
     * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
     */
    @Override
    public SpatialFilter touches(BinarySpatialOpType op, Object obj)
            throws Exception {
        Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
        return new SpatialFilter(SpatialOp.TOUCHES, e.getValue());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#within(net.opengis.filter
     * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
     */
    @Override
    public SpatialFilter within(BinarySpatialOpType op, Object obj)
            throws Exception {
        Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
        return new SpatialFilter(SpatialOp.WITHIN, e.getValue());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#overlaps(net.opengis.filter
     * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
     */
    @Override
    public SpatialFilter overlaps(BinarySpatialOpType op, Object obj)
            throws Exception {
        Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
        return new SpatialFilter(SpatialOp.OVERLAPS, e.getValue());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#crosses(net.opengis.filter
     * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
     */
    @Override
    public SpatialFilter crosses(BinarySpatialOpType op, Object obj)
            throws Exception {
        Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
        return new SpatialFilter(SpatialOp.CROSSES, e.getValue());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#intersects(net.opengis.filter
     * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
     */
    @Override
    public SpatialFilter intersects(BinarySpatialOpType op, Object obj)
            throws Exception {
        Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
        return new SpatialFilter(SpatialOp.INTERSECTS, e.getValue());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#contains(net.opengis.filter
     * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
     */
    @Override
    public SpatialFilter contains(BinarySpatialOpType op, Object obj)
            throws Exception {
        Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
        return new SpatialFilter(SpatialOp.CONTAINS, e.getValue());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#dWithin(net.opengis.filter
     * .v_1_1_0.DistanceBufferType, java.lang.Object)
     */
    @Override
    public SpatialFilter dWithin(DistanceBufferType op, Object obj)
            throws Exception {
        throw new Exception("dWithin queries not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#beyond(net.opengis.filter
     * .v_1_1_0.DistanceBufferType, java.lang.Object)
     */
    @Override
    public SpatialFilter beyond(DistanceBufferType op, Object obj)
            throws Exception {
        throw new Exception("Beyond queries not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.filter.OgcFilterVisitor#bbox(net.opengis.filter.
     * v_1_1_0.BBOXType, java.lang.Object)
     */
    @Override
    public LogicFilter bbox(BBOXType op, Object obj) throws Exception {
        if (!(obj instanceof VisitorBag)) {
            log.error("Mismatched visitor and helper object", new Exception());
            throw new WfsException(Code.OperationProcessingFailed);
        }
        VisitorBag bag = (VisitorBag) obj;
        Object any = op.getAny();
        if (any instanceof JAXBElement<?>) {
            any = ((JAXBElement<?>) any).getValue();
        }
        if (!(any instanceof EnvelopeType)) {
            throw new Exception("Unsupported bbox type: " + any.getClass());
        }
        EnvelopeType value = (EnvelopeType) any;
        int dims = EnvelopeConverter.getDims(value);
        Envelope env;
        if (dims == 2) {
            env = BoundingBoxUtil.convert2D(value);
            SpatialFilter disjoint = new SpatialFilter(SpatialOp.DISJOINT,
                    JTS.toGeometry(env));
            return LogicFilter.not(disjoint);
        } else if (dims == 3) {
            String verticalField = bag.getVerticalField();
            if (verticalField == null) {
                throw new WfsException(Code.InvalidParameterValue,
                        "Feature does not have vertical information");
            }
            Composite3DBoundingBox composite = BoundingBoxUtil
                    .separate3DEnvelope(value);
            env = composite.getHorizontal().transform(
                    MapUtil.LATLON_PROJECTION, true);

            // horiz
            SpatialFilter disjoint = new SpatialFilter(SpatialOp.DISJOINT,
                    JTS.toGeometry(env));
            LogicFilter horiz = LogicFilter.not(disjoint);
            // vert
            VerticalFilter vert = new VerticalFilter(VerticalOp.BETWEEN,
                    composite.getVertical());
            return LogicFilter.and(horiz, vert);
        } else {
            throw new Exception("Unsupported number of dimensions: " + dims);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#isNil(net.opengis
     * .filter.v_2_0_0.PropertyIsNilType, java.lang.Object)
     */
    @Override
    public Object isNil(PropertyIsNilType op, Object obj) throws Exception {
        ExpressionProcessor eproc = new ExpressionProcessor(op.getExpression());
        String field = getRef(eproc, (VisitorBag) obj).value;
        if (field == null) {
            return AbstractPdoFilter.noFilter();
        }
        return ComparisonFilter.isNull(field);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#after(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter after(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.After, parseTimeOp(op, obj));
    }

    protected DataTime parseTimeOp(BinaryTemporalOpType op, Object obj)
            throws Exception {
        TimeOperand operand = getOperand(op, Date.class, obj);
        DataTime dt;
        if (operand.instance) {
            dt = new DataTime((Date) operand.start);
        } else {
            Date start = (Date) operand.start;
            TimeRange range = new TimeRange(start, (Date) operand.end);
            dt = new DataTime(start.getTime(), range);
        }
        return dt;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#before(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter before(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.Before, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#begins(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter begins(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.Begins, parseTimeOp(op, obj));
    }

    @Override
    public TemporalFilter ends(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.Ends, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#begunBy(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter begunBy(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.BegunBy, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#tContains(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter tContains(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.TContains, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#during(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter during(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.During, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#tEquals(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter tEquals(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.TEquals, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#tOverlaps(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter tOverlaps(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.TOverlaps, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#meets(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter meets(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.Meets, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#overlappedBy(net
     * .opengis.filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter overlappedBy(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.OverlappedBy, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#metBy(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter metBy(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.MetBy, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#endedBy(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter endedBy(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.EndedBy, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#anyInteracts(net
     * .opengis.filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public TemporalFilter anyInteracts(BinaryTemporalOpType op, Object obj)
            throws Exception {
        return new TemporalFilter(TimeOp.AnyInteracts, parseTimeOp(op, obj));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#id(java.util.List,
     * java.lang.Object)
     */
    @Override
    public AbstractPdoFilter id(List<AbstractIdType> ids, Object obj)
            throws Exception {
        VisitorBag bag = (VisitorBag) obj;
        ArrayList<AbstractPdoFilter> filters = new ArrayList<AbstractPdoFilter>(
                ids.size());
        for (AbstractIdType id : ids) {
            if (id instanceof ResourceIdType) {
                ResourceIdType rid = (ResourceIdType) id;
                ComparisonFilter comp = new ComparisonFilter(bag.getIdField(),
                        rid.getRid());
                filters.add(comp);
            }
        }
        return LogicFilter.or(filters.toArray(new AbstractPdoFilter[filters
                .size()]));
    }
}
