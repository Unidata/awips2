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

import java.lang.reflect.Field;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_2_0_0.BinarySpatialOpType;
import net.opengis.filter.v_2_0_0.BinaryTemporalOpType;
import net.opengis.gml.v_3_2_1.AbstractGeometryType;
import net.opengis.gml.v_3_2_1.EnvelopeType;
import net.opengis.gml.v_3_2_1.TimeInstantPropertyType;
import net.opengis.gml.v_3_2_1.TimeInstantType;
import net.opengis.gml.v_3_2_1.TimePeriodType;
import net.opengis.gml.v_3_2_1.TimePositionType;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Junction;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ogc.common.gml3_2_1.EnvelopeConverter;
import com.raytheon.uf.edex.ogc.common.gml3_2_1.GeometryConverter;
import com.raytheon.uf.edex.ogc.common.spatial.CrsLookup;
import com.raytheon.uf.edex.ogc.common.util.ConvertService;
import com.raytheon.uf.edex.wfs.provider.VisitorBag;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.WKTReader;

/**
 * Abstract base for filter parsing. Provides common utility methods.
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
public abstract class AbstractQueryFilterVisitor implements IFilter2Visitor {

    protected QueryExpressionVisitor exprVisitor = new QueryExpressionVisitor();

    protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

    protected GeometryConverter geomConverter = new GeometryConverter();

    protected EnvelopeConverter envConverter = new EnvelopeConverter();

    /**
     * Extract left and right side of binary operator
     * 
     * @param left
     * @param right
     * @param bag
     * @return entry with field as key and filter object as value
     * @throws Exception
     */
    protected Operand getBinaryProps(ExpressionProcessor left,
            ExpressionProcessor right, VisitorBag bag) throws Exception {

        OpField field = getRef(left, bag);
        // If the field is explicitly ignored in the filter, return null so it
        // is not processed further
        if (field.value == null) {
            return null;
        }

        Object value = right.accept(exprVisitor, bag);

        if (!field.sql) {
            Class<?> ent = bag.getRootEntity();
            String[] path = field.value.split("\\.");
            value = ConvertService.get().convertAsType((String) value, ent,
                    path);
        }

        Operand operand = new Operand(field.value, value);
        operand.sql = field.sql;
        return operand;
    }

    /**
     * Parse reference string as dotted field path
     * 
     * @param refProc
     * @param bag
     * @return
     * @throws Exception
     */
    protected OpField getRef(ExpressionProcessor refProc, VisitorBag bag)
            throws Exception {

        OpField field = new OpField();
        Object obj = refProc.accept(exprVisitor, bag);
        if (obj instanceof FilterFunction) {
            field.value = ((FilterFunction) obj).toSql();
            field.sql = true;
        } else {
            String prop = (String) refProc.accept(exprVisitor, bag);
            String[] path = parseProp(prop);
            field.value = bag.filterField(StringUtils.join(path, "."));
        }

        return field;
    }

    /**
     * Parse reference string into field path parts
     * 
     * @param prop
     * @return
     */
    protected String[] parseProp(String prop) {
        // TODO we may want to keep the namespaces
        String[] rval = prop.trim().split("\\/");
        for (int i = 0; i < rval.length; ++i) {
            int index = rval[i].lastIndexOf(':');
            if (index > -1) {
                rval[i] = rval[i].substring(index + 1);
            }
        }
        return rval;
    }

    /**
     * Sends this object to all filters in list as visitor. Adds results to
     * junction.
     * 
     * @param filters
     * @param obj
     * @param junc
     * @throws Exception
     */
    protected void acceptAll(List<Filter2Processor> filters, Object obj,
            Junction junc) throws Exception {
        Iterator<Filter2Processor> i = filters.iterator();
        while (i.hasNext()) {
            Criterion crit = (Criterion) i.next().accept(this, obj);
            if (crit != null) {
                junc.add(crit);
            }
        }
    }

    /**
     * Combine two lists. May reuse parameters as return value.
     * 
     * @param <T>
     * @param l1
     * @param l2
     * @return
     */
    protected <T> List<T> addAll(List<T> l1, List<T> l2) {
        if (l1 == null) {
            return l2;
        }
        if (l2 == null) {
            return l1;
        }
        ArrayList<T> rval = new ArrayList<T>(l1.size() + l2.size());
        rval.addAll(l1);
        rval.addAll(l2);
        return rval;
    }

    /**
     * Get operands of binary spatial operator
     * 
     * @param binary
     * @param bag
     * @return entry that has the field path as key and filter geometry as the
     *         value
     * @throws Exception
     */
    protected Entry<String, Geometry> getBinarySpatial(
            BinarySpatialOpType binary, VisitorBag bag) throws Exception {
        String str = binary.getValueReference();
        String prop = StringUtils.join(parseProp(str), '.');
        Geometry shape = getGeometry(binary, bag);
        return new SimpleEntry<String, Geometry>(bag.filterField(prop), shape);
    }

    /**
     * Extract JTS geometry from spatial operator
     * 
     * @param binary
     * @param bag
     * @return
     * @throws Exception
     */
    protected Geometry getGeometry(BinarySpatialOpType binary, VisitorBag bag)
            throws Exception {
        if (binary.isSetAny()) {
            return getGeometry(binary.getAny());
        } else if (binary.isSetExpression()) {
            JAXBElement<?> exp = binary.getExpression();
            return getGeometry(new ExpressionProcessor(exp));
        } else {
            throw new Exception("Missing geometry value");
        }
    }

    /**
     * Convert XSD 'any' reference to JTS geometry if possible.
     * 
     * @param obj
     * @return
     * @throws Exception
     */
    protected Geometry getGeometry(Object obj) throws Exception {
        if (obj instanceof JAXBElement<?>) {
            obj = ((JAXBElement<?>) obj).getValue();
        }
        Geometry shape;
        String crsStr;
        if (obj instanceof EnvelopeType) {
            EnvelopeType env = (EnvelopeType) obj;
            Envelope envelope = envConverter.convert(env);
            shape = geomConverter.convert(envelope);
            crsStr = env.getSrsName();
        } else if (obj instanceof AbstractGeometryType) {
            AbstractGeometryType geom = (AbstractGeometryType) obj;
            shape = geomConverter.convert(geom);
            crsStr = geom.getSrsName();
        } else {
            throw new Exception("Unsupported geometry format");
        }
        CoordinateReferenceSystem crs = CrsLookup.lookup(crsStr);
        if (CrsLookup.isEpsgGeoCrs(crs)) {
            shape.apply(new CoordinateFilter() {
                @Override
                public void filter(Coordinate coord) {
                    double tmp = coord.x;
                    coord.x = coord.y;
                    coord.y = tmp;
                }
            });
            shape.geometryChanged();
        }
        return shape;
    }

    /**
     * Convert WKT literal expression to JTS geometry
     * 
     * @param proc
     * @param bag
     * @return
     * @throws Exception
     */
    protected Geometry getGeometry(ExpressionProcessor proc, VisitorBag bag)
            throws Exception {
        Object literal = proc.accept(exprVisitor, bag);
        if (!(literal instanceof String)) {
            throw new Exception("Geometry literals must be WKT");
        }
        try {
            return new WKTReader().read((String) literal);
        } catch (Exception e) {
            throw new Exception("Geometry literals must be WKT");
        }
    }

    /**
     * Log warning if list is not of size 1
     * 
     * @param lst
     * @param msg
     * @return
     */
    protected String getStringWarn(List<Object> lst, String msg) {
        if (lst.size() != 1) {
            log.warn(msg);
        }
        return (String) lst.get(0);
    }

    /**
     * Sreies of inner classes to facilitate passing parsed fields with select
     * metadata obtained during the parsing
     */
    protected static class OpField {
        public String value;

        public boolean sql = false;

        public OpField() {
            this.value = "";
        }

        public OpField(String val) {
            this.value = val;
        }
    }

    protected static class Operand {
        public Object right;

        public Object left;

        public boolean sql = false;

        public Operand(Object left, Object right) {
            this.right = right;
            this.left = left;
        }
    }

    protected static class TimeOperand {
        public Object start;

        public Object end;

        public boolean instance = false;

        public boolean sql = false;

        public TimeOperand(Object instance) {
            start = instance;
            this.instance = true;
        }

        public TimeOperand(Object start, Object end) {
            this.start = start;
            this.end = end;
        }
    }

    /**
     * Get right operand of binary temporal operation
     * 
     * @param binary
     * @param timeClass
     *            class of left operand for the right to match
     * @param obj
     *            visitor object
     * @return either an instance or period time operand
     * @throws Exception
     */
    protected TimeOperand getOperand(BinaryTemporalOpType binary,
            Class<?> timeClass, Object obj) throws Exception {
        TimeOperand rval;
        if (binary.isSetExpression()) {
            ExpressionProcessor eproc = new ExpressionProcessor(
                    binary.getExpression());
            Object timeObj = eproc.accept(exprVisitor, obj);
            if (timeObj instanceof TimeFunction) {
                rval = new TimeOperand(((TimeFunction) timeObj).toSql());
                rval.sql = true;
            } else {
                rval = parseTimeString((String) timeObj, timeClass);
            }
        } else if (binary.isSetAny()) {
            Object any = binary.getAny();
            if (any instanceof JAXBElement<?>) {
                any = ((JAXBElement<?>) any).getValue();
            }
            rval = extractTime(any, timeClass);
        } else {
            throw new Exception(
                    "No temporal value set for binary temporal operation");
        }
        return rval;
    }

    /**
     * Parse right operand from string
     * 
     * @param time
     *            ISO 8601 formatted time instance or period
     * @param timeClass
     *            class of left operand for the right to match
     * @return either an instance or period time operand
     */
    protected TimeOperand parseTimeString(String time, Class<?> timeClass) {
        TimeOperand rval;
        if (time.contains("/")) {
            // range
            String[] parts = StringUtils.split(time, '/');
            Object start = parseInstance(parts[0], timeClass);
            Object end = parseInstance(parts[1], timeClass);
            rval = new TimeOperand(start, end);
        } else {
            // instance
            Object instance = parseInstance(time, timeClass);
            rval = new TimeOperand(instance);
        }
        return rval;
    }

    /**
     * Get time operand from GML time object
     * 
     * @param obj
     *            GML time primitive type
     * @param timeClass
     *            class of left operand for the right to match
     * @return either an instance or period time operand
     * @throws Exception
     */
    protected TimeOperand extractTime(Object obj, Class<?> timeClass)
            throws Exception {
        TimeOperand rval;
        if (obj instanceof TimeInstantType) {
            Object inst = parseInstance((TimeInstantType) obj, timeClass);
            rval = new TimeOperand(inst);
        } else if (obj instanceof TimePositionType) {
            Object inst = parseInstance((TimePositionType) obj, timeClass);
            rval = new TimeOperand(inst);
        } else if (obj instanceof TimePeriodType) {
            rval = parsePeriod((TimePeriodType) obj, timeClass);
        } else {
            throw new Exception("Unsupported time type: " + obj.getClass());
        }
        return rval;
    }

    /**
     * Get field path and type for left operand
     * 
     * @param binary
     * @param bag
     * @return pair of dotted field and target class
     * @throws Exception
     */
    protected Entry<String, Class<?>> getRef(BinaryTemporalOpType binary,
            VisitorBag bag) throws Exception {
        if (!binary.isSetValueReference()) {
            // default to refTime
            return new SimpleEntry<String, Class<?>>("dataTime.refTime",
                    Date.class);
        }
        String prop = binary.getValueReference();
        String[] unfilteredPath = parseProp(prop);
        String unfilteredField = StringUtils.join(unfilteredPath, ".");
        String field = bag.filterField(unfilteredField);
        String[] path = field.split("\\.");
        Class<?> fieldType = getFieldType(bag.getRootEntity(), path);
        return new SimpleEntry<String, Class<?>>(field, fieldType);
    }

    /**
     * Determine class for field
     * 
     * @param entity
     * @param fieldPath
     * @return
     * @throws SecurityException
     * @throws NoSuchFieldException
     */
    protected static Class<?> getFieldType(Class<?> entity, String[] fieldPath)
            throws SecurityException, NoSuchFieldException {
        Field f = getField(fieldPath[0], entity);
        for (int i = 1; i < fieldPath.length; ++i) {
            f = getField(fieldPath[i], f.getType());
        }
        return f.getType();
    }

    /**
     * Extract field from class, searches parent classes
     * 
     * @param fieldName
     * @param c
     * @return
     * @throws SecurityException
     * @throws NoSuchFieldException
     */
    protected static Field getField(String fieldName, Class<?> c)
            throws SecurityException, NoSuchFieldException {
        Field rval;
        try {
            rval = c.getDeclaredField(fieldName);
        } catch (NoSuchFieldException e) {
            Class<?> parent = c.getSuperclass();
            if (parent.isInstance(Object.class)) {
                throw e;
            } else {
                rval = getField(fieldName, c.getSuperclass());
            }
        }
        return rval;
    }

    /**
     * Get time instance for right operand
     * 
     * @param binary
     * @param timeClass
     *            class of left operand for the right to match
     * @param obj
     *            visitor object
     * @return object of type timeClass
     * @throws Exception
     */
    protected Object getInstance(BinaryTemporalOpType binary,
            Class<?> timeClass, Object obj) throws Exception {
        if (binary.isSetExpression()) {
            ExpressionProcessor eproc = new ExpressionProcessor(
                    binary.getExpression());
            String timeStr = (String) eproc.accept(exprVisitor, obj);
            return parseInstance(timeStr, timeClass);
        } else if (binary.isSetAny()) {
            Object any = binary.getAny();
            if (any instanceof JAXBElement<?>) {
                any = ((JAXBElement<?>) any).getValue();
            }
            if (any instanceof TimeInstantType) {
                return parseInstance((TimeInstantType) any, timeClass);
            } else {
                throw new Exception("Unsupported time instance type: "
                        + any.getClass());
            }
        }
        throw new Exception(
                "No temporal value set for binary temporal operation");
    }

    @SuppressWarnings("unchecked")
    protected <T> T parseInstance(String str, Class<T> timeClass) {
        return (T) ConvertService.get().convertObject(str, timeClass);
    }

    /**
     * Get time instance from GML time primitive
     * 
     * @param ttype
     * @param timeClass
     *            class of left operand for the right to match
     * @return object of type timeClass
     * @throws Exception
     */
    protected Object parseInstance(TimeInstantType ttype, Class<?> timeClass)
            throws Exception {
        TimePositionType pos = ttype.getTimePosition();
        return parseInstance(pos, timeClass);
    }

    /**
     * Get time instance from GML time primitive
     * 
     * @param pos
     * @param timeClass
     *            class of left operand for the right to match
     * @return object of type timeClass
     * @throws Exception
     */
    protected Object parseInstance(TimePositionType pos, Class<?> timeClass)
            throws Exception {
        if (pos.isSetFrame()) {
            Pattern p = Pattern.compile("^.*ISO.?8601.*$",
                    Pattern.CASE_INSENSITIVE);
            String frame = pos.getFrame();
            if (!p.matcher(frame).matches()) {
                throw new Exception("Only ISO-8601 times are supported");
            }
        }
        List<String> values = pos.getValue();
        if (values.size() > 1) {
            throw new Exception(
                    "Multiple values per time position not supported");
        }
        String value = values.get(0);
        return parseInstance(value, timeClass);
    }

    /**
     * Get time period from GML time primitive
     * 
     * @param ttype
     * @param timeClass
     *            class of left operand for the right to match
     * @return period time operand
     * @throws Exception
     */
    protected TimeOperand parsePeriod(TimePeriodType ttype, Class<?> timeClass)
            throws Exception {
        Object begin;
        Object end;
        if (ttype.isSetBegin()) {
            TimeInstantPropertyType prop = ttype.getBegin();
            TimeInstantType inst = prop.getTimeInstant();
            begin = parseInstance(inst, timeClass);
        } else if (ttype.isSetBeginPosition()) {
            TimePositionType pos = ttype.getBeginPosition();
            begin = parseInstance(pos, timeClass);
        } else {
            throw new Exception("Unable to locate beginning of time period");
        }
        if (ttype.isSetBegin()) {
            TimeInstantPropertyType prop = ttype.getEnd();
            TimeInstantType inst = prop.getTimeInstant();
            end = parseInstance(inst, timeClass);
        } else if (ttype.isSetEndPosition()) {
            TimePositionType pos = ttype.getEndPosition();
            end = parseInstance(pos, timeClass);
        } else {
            throw new Exception("Unable to locate end of time period");
        }
        return new TimeOperand(begin, end);
    }

}
