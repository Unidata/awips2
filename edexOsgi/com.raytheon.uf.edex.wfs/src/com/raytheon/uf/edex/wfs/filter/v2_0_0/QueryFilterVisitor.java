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

import java.util.List;
import java.util.Map.Entry;

import javax.measure.unit.Unit;
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
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;
import org.hibernatespatial.criterion.SpatialRelateExpression;
import org.hibernatespatial.criterion.SpatialRestrictions;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.edex.ogc.common.db.SQLParamRestriction;
import com.raytheon.uf.edex.ogc.common.gml3_2_1.EnvelopeConverter;
import com.raytheon.uf.edex.ogc.common.spatial.AltUtil;
import com.raytheon.uf.edex.ogc.common.spatial.BoundingBoxUtil;
import com.raytheon.uf.edex.ogc.common.spatial.Composite3DBoundingBox;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalCoordinate.Reference;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalEnabled;
import com.raytheon.uf.edex.ogc.common.spatial.VerticalSpatialFactory;
import com.raytheon.uf.edex.ogc.common.util.ConvertService;
import com.raytheon.uf.edex.wfs.WfsException;
import com.raytheon.uf.edex.wfs.WfsException.Code;
import com.raytheon.uf.edex.wfs.filter.EscapingLikeExpression;
import com.raytheon.uf.edex.wfs.provider.VisitorBag;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Visitor for building hibernate criterion from filter
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
public class QueryFilterVisitor extends AbstractQueryFilterVisitor {

    private static final Character SQL_WILD = '%';

    private static final Character SQL_SINGLE = '_';

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
	public Criterion equal(ExpressionProcessor left,
			ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);
        
        if(operand == null) {
            return Restrictions.disjunction();
        }

        if (operand.sql) {
            return SQLParamRestriction.restriction((String) operand.left
                    + " = " + (String) operand.right);
        } else {
            return Restrictions.eq((String) operand.left, operand.right);
        }

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
	public Criterion notEqual(ExpressionProcessor left,
			ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);

        if(operand == null) {
            return Restrictions.disjunction();
        }
        
        if (operand.sql) {
            return SQLParamRestriction.restriction((String) operand.left
                    + " <> " + (String) operand.right);
        } else {
            return Restrictions.ne((String) operand.left, operand.right);
        }
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
	public Criterion lessThan(ExpressionProcessor left,
			ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);

        if(operand == null) {
            return Restrictions.disjunction();
        }
        
        if (operand.sql) {
            return SQLParamRestriction.restriction((String) operand.left
                    + " < " + (String) operand.right);
        } else {
            return Restrictions.lt((String) operand.left, operand.right);
        }
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
	public Criterion greaterThan(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception {
		VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);

        if(operand == null) {
            return Restrictions.disjunction();
        }
        
        if (operand.sql) {
            return SQLParamRestriction.restriction((String) operand.left
                    + " > " + (String) operand.right);
        } else {
            return Restrictions.gt((String) operand.left, operand.right);
        }
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
	public Criterion greaterThanEqual(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception {
		VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);

        if(operand == null) {
            return Restrictions.disjunction();
        }
        
        if (operand.sql) {
            return SQLParamRestriction.restriction((String) operand.left
                    + " >= " + (String) operand.right);
        } else {
            return Restrictions.ge((String) operand.left, operand.right);
        }
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
	public Criterion lessThanEqual(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception {
		VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);

        if(operand == null) {
            return Restrictions.disjunction();
        }
        
        if (operand.sql) {
            return SQLParamRestriction.restriction((String) operand.left
                    + " <= " + (String) operand.right);
        } else {
            return Restrictions.le((String) operand.left, operand.right);
        }
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#isLike(net.opengis.filter
	 * .v_1_1_0.PropertyIsLikeType, java.lang.Object)
	 */
	@Override
	public Criterion isLike(PropertyIsLikeType op, Object obj)
			throws Exception {
        List<JAXBElement<?>> expressions = op.getExpression();
        ExpressionProcessor left = new ExpressionProcessor(expressions.get(0));
        ExpressionProcessor right = new ExpressionProcessor(expressions.get(1));

        VisitorBag bag = (VisitorBag) obj;
        Operand operand = getBinaryProps(left, right, bag);

        if(operand == null) {
            return Restrictions.disjunction();
        }
        
        Character wild = checkChar(op.getWildCard(), op.isSetWildCard(),
                SQL_WILD);
        Character esc = checkChar(op.getEscapeChar(), op.isSetEscapeChar(), '!');
        Character single = checkChar(op.getSingleChar(), op.isSetSingleChar(),
                SQL_SINGLE);
        String likeStr = toSqlLike((String) operand.right, wild, esc, single);

        if (operand.sql) {
            return SQLParamRestriction.restriction((String) operand.left
                    + " ~~ " + likeStr);
        } else {
            return new EscapingLikeExpression((String) operand.left, likeStr,
                    MatchMode.ANYWHERE, esc, false);
        }
	}

    /**
     * Ensure that character string is of length 1
     * 
     * @param charStr
     *            character string
     * @param isSet
     *            if false, return fallback
     * @param fallback
     *            default return value
     * @return
     * @throws Exception
     *             if character string is not of length 1
     */
    private Character checkChar(String charStr, boolean isSet,
            Character fallback) throws Exception {
        if (!isSet) {
            return fallback;
        }
        if (charStr.length() != 1) {
            throw new Exception(
                    "Wild, single and escape strings must be 1 character each");
        }
        return charStr.charAt(0);
    }

    /**
     * Convert orig to SQL 'like' operand
     * 
     * @param orig
     * @param wild
     *            glob wildcard character used in orig
     * @param esc
     *            escape character used
     * @param single
     *            single wildcard character used in orig
     * @return
     */
    protected static String toSqlLike(String orig, Character wild,
            Character esc,
            Character single) {
        String rval = orig;
        if (!wild.equals(SQL_WILD)) {
            rval = switchSpecial(rval, wild, SQL_WILD, esc);
        }
        if (!single.equals(SQL_SINGLE)) {
            rval = switchSpecial(rval, single, SQL_SINGLE, esc);
        }
        return rval;
    }

    /**
     * Replace special character in orig with replacement. (Un)escapes
     * characters as needed.
     * 
     * @param orig
     * @param special
     * @param replacement
     * @param escape
     * @return
     */
    protected static String switchSpecial(String orig, Character special,
            Character replacement, Character escape) {
        StringBuilder sb = new StringBuilder();
        char[] arr = orig.toCharArray();
        for (int i = 0; i < arr.length; ++i) {
            Character c = Character.valueOf(arr[i]);
            if (c.equals(escape)) {
                Character next = Character.valueOf(arr[++i]);
                // preserve escapes on other special characters
                if (!next.equals(special)) {
                    sb.append(escape);
                }
                sb.append(next);
            } else if (c.equals(replacement)) {
                // escape literal instances of the new special char
                sb.append(escape).append(c);
            } else if (c.equals(special)) {
                // one for one switch
                sb.append(replacement);
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#isNull(net.opengis.filter
	 * .v_1_1_0.PropertyIsNullType, java.lang.Object)
	 */
	@Override
	public Criterion isNull(PropertyIsNullType op, Object obj)
			throws Exception {
        ExpressionProcessor eproc = new ExpressionProcessor(op.getExpression());
        String field = getRef(eproc, (VisitorBag) obj).value;
        if (field == null) {
            return Restrictions.conjunction();
        }
        return Restrictions.isNull(field);
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
	public Criterion between(ExpressionProcessor lower,
			ExpressionProcessor exp,
			ExpressionProcessor upper, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;

        Operand lowerPart = getBinaryProps(exp, lower, bag);
        Operand upperPart = getBinaryProps(exp, upper, bag);

        if(lowerPart == null || upperPart == null) {
            return Restrictions.disjunction();
        }
        
		return Restrictions.between((String) lowerPart.left, lowerPart.right,
                upperPart.right);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.filter.OgcFilterVisitor#and(java.util.List,
	 * java.lang.Object)
	 */
	@Override
    public Criterion and(List<Filter2Processor> filters, Object obj)
			throws Exception {
		Conjunction rval = Restrictions.conjunction();
		acceptAll(filters, obj, rval);
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.edex.filter.OgcFilterVisitor#or(java.util.List,
	 * java.lang.Object)
	 */
	@Override
    public Criterion or(List<Filter2Processor> filters, Object obj)
			throws Exception {
		Disjunction rval = Restrictions.disjunction();
		acceptAll(filters, obj, rval);
		return rval;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#not(com.raytheon.uf.edex
	 * .filter.FilterProcessor, java.lang.Object)
	 */
	@Override
    public Criterion not(Filter2Processor filter, Object obj) throws Exception {
		return Restrictions.not((Criterion) filter.accept(this, obj));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#spatialEquals(net.opengis
	 * .filter.v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion spatialEquals(BinarySpatialOpType op, Object obj)
			throws Exception {
		VisitorBag bag = (VisitorBag) obj;
		Entry<String, Geometry> e = getBinarySpatial(op, bag);
		return SpatialRestrictions.eq(bag.filterField(e.getKey()), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#disjoint(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion disjoint(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.disjoint(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#touches(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion touches(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.touches(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#within(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion within(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.within(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#overlaps(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion overlaps(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.overlaps(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#crosses(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion crosses(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.crosses(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#intersects(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion intersects(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.intersects(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#contains(net.opengis.filter
	 * .v_1_1_0.BinarySpatialOpType, java.lang.Object)
	 */
	@Override
	public Criterion contains(BinarySpatialOpType op, Object obj)
			throws Exception {
		Entry<String, Geometry> e = getBinarySpatial(op, (VisitorBag) obj);
		return SpatialRestrictions.contains(e.getKey(), e.getValue());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcFilterVisitor#dWithin(net.opengis.filter
	 * .v_1_1_0.DistanceBufferType, java.lang.Object)
	 */
	@Override
	public Object dWithin(DistanceBufferType op, Object obj)
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
	public Object beyond(DistanceBufferType op, Object obj)
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
	public Criterion bbox(BBOXType op, Object obj) throws Exception {
		VisitorBag bag = (VisitorBag) obj;
        String prop;
        if (!op.isSetExpression()) {
            prop = bag.getSpatialField();
            if (prop == null) {
                throw new WfsException(Code.InvalidParameterValue,
                        "Geospatial filter not supported for feature type");
            }
        } else {
            ExpressionProcessor eproc = new ExpressionProcessor(
                    op.getExpression());
            prop = getRef(eproc, bag).value;
            if (prop == null) {
                prop = bag.getSpatialField();
            }
        }
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
            SpatialRelateExpression disjoint = SpatialRestrictions.disjoint(
                    bag.filterField(prop), JTS.toGeometry(env));
            return Restrictions.not(disjoint);
        } else if (dims == 3) {
            String verticalField = bag.getVerticalField();
            if (verticalField == null) {
                throw new WfsException(Code.InvalidParameterValue,
                        "3D filter not supported for feature type");
            }
            Composite3DBoundingBox composite = BoundingBoxUtil
                    .separate3DEnvelope(value);
            env = composite.getHorizontal().transform(
                    MapUtil.LATLON_PROJECTION, true);
            Conjunction and = Restrictions.conjunction();
            // horiz
            SpatialRelateExpression disjoint = SpatialRestrictions.disjoint(
                    bag.filterField(prop), JTS.toGeometry(env));
            and.add(Restrictions.not(disjoint));
            // vert
            VerticalCoordinate vert = composite.getVertical();
			Unit<?> verticalUnits = getVerticalUnits(bag, vert.getUnits());
			vert = AltUtil.convert(verticalUnits, Reference.UNKNOWN, vert);
            String min = Integer.toString((int) Math.floor(vert.getMin()));
            String max = Integer.toString((int) Math.ceil(vert.getMax()));
            String[] path = verticalField.split("\\.");
			Object minObj = ConvertService.get().convertAsType(min,
					bag.getRootEntity(), path);
			Object maxObj = ConvertService.get().convertAsType(max,
					bag.getRootEntity(), path);
            and.add(Restrictions.between(verticalField, minObj, maxObj));
            return and;
        } else {
            throw new Exception("Unsupported number of dimensions: " + dims);
        }
	}

	/**
	 * Find default vertical units for type
	 * 
	 * @param bag
	 * @return
	 */
	private Unit<?> getVerticalUnits(VisitorBag bag, Unit<?> fallback) {
		Class<?> entity = bag.getRootEntity();
		VerticalEnabled<?> enabled = VerticalSpatialFactory.getEnabled(entity);
		Unit<?> rval;
		if (enabled == null
				|| (rval = enabled.getDefaultVerticalUnit()) == null) {
			log.warn("Unable to find default vertical units for type: "
					+ entity);
			log.warn("falling back to using: " + fallback);
			return fallback;
		}
		return rval;
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
            return Restrictions.conjunction();
        }
        return Restrictions.isNull(field);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#after(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Criterion after(BinaryTemporalOpType op, Object obj)
            throws Exception {
        Entry<String, Class<?>> ref = getRef(op, (VisitorBag)obj);
        TimeOperand operand = getOperand(op, ref.getValue(), obj);
        Object rhs = operand.instance ? operand.start : operand.end;

        if (operand.sql) {
            return SQLParamRestriction.restriction("{" + ref.getKey() + "} > "
                    + (String) rhs);
        } else {
            return Restrictions.gt(ref.getKey(), rhs);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#before(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Criterion before(BinaryTemporalOpType op, Object obj)
            throws Exception {
        Entry<String, Class<?>> ref = getRef(op, (VisitorBag) obj);
        TimeOperand operand = getOperand(op, ref.getValue(), obj);
        // both instance and period operands have value in start

        if (operand.sql) {
            return SQLParamRestriction.restriction("{" + ref.getKey() + "} < "
                    + (String) operand.start);
        } else {
            return Restrictions.lt(ref.getKey(), operand.start);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#begins(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Criterion begins(BinaryTemporalOpType op, Object obj)
            throws Exception {
        Entry<String, Class<?>> ref = getRef(op, (VisitorBag) obj);
        TimeOperand operand = getOperand(op, ref.getValue(), obj);
        if (!operand.sql && operand.instance) {
            throw new Exception("Left operand for begins filter must be period");
        }

        if (operand.sql) {
            return SQLParamRestriction.restriction("{" + ref.getKey() + "} = "
                    + (String) operand.start);
        } else {
            return Restrictions.eq(ref.getKey(), operand.start);
        }

    }

    @Override
    public Criterion ends(BinaryTemporalOpType op, Object obj) throws Exception {
        Entry<String, Class<?>> ref = getRef(op, (VisitorBag) obj);
        TimeOperand operand = getOperand(op, ref.getValue(), obj);
        if (!operand.sql && operand.instance) {
            throw new Exception("Left operand for ends filter must be period");
        }

        if (operand.sql) {
            // sql value stored in start regardless of operation
            return SQLParamRestriction.restriction("{" + ref.getKey() + "} = "
                    + (String) operand.start);
        } else {
            return Restrictions.eq(ref.getKey(), operand.end);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#begunBy(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Object begunBy(BinaryTemporalOpType op, Object obj) throws Exception {
        // TODO
        throw new Exception("Begun By filter not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#tContains(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Object tContains(BinaryTemporalOpType op, Object obj)
            throws Exception {
        // TODO
        throw new Exception("TContains filter not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#during(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Criterion during(BinaryTemporalOpType op, Object obj)
            throws Exception {
        Entry<String, Class<?>> ref = getRef(op, (VisitorBag) obj);
        TimeOperand operand = getOperand(op, ref.getValue(), obj);
        if (!operand.sql && operand.instance) {
            throw new Exception("Left operand for during filter must be period");
        }
        Conjunction and = Restrictions.conjunction();

        if (operand.sql) {
            and.add(SQLParamRestriction.restriction("{" + ref.getKey() + "} > "
                    + (String) operand.start));
            and.add(SQLParamRestriction.restriction("{" + ref.getKey() + "} < "
                    + (String) operand.end));
        } else {
            and.add(Restrictions.gt(ref.getKey(), operand.start));
            and.add(Restrictions.lt(ref.getKey(), operand.end));
        }

        return and;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#tEquals(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Criterion tEquals(BinaryTemporalOpType op, Object obj)
            throws Exception {
        Entry<String, Class<?>> ref = getRef(op, (VisitorBag) obj);
        TimeOperand operand = getOperand(op, ref.getValue(), obj);
        if (!operand.sql && !operand.instance) {
            throw new Exception(
                    "Left operand for TEquals filter must be instance");
        }
        if (operand.sql) {
            return SQLParamRestriction.restriction("{" + ref.getKey() + "} = "
                    + (String) operand.start);
        } else {
            return Restrictions.eq(ref.getKey(), operand.start);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#tOverlaps(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Object tOverlaps(BinaryTemporalOpType op, Object obj)
            throws Exception {
        // TODO
        throw new Exception("TOverlaps filter not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#meets(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Object meets(BinaryTemporalOpType op, Object obj) throws Exception {
        // TODO
        throw new Exception("Meets filter not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#overlappedBy(net
     * .opengis.filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Object overlappedBy(BinaryTemporalOpType op, Object obj)
            throws Exception {
        // TODO
        throw new Exception("Overlapped By filter not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#metBy(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Object metBy(BinaryTemporalOpType op, Object obj) throws Exception {
        // TODO
        throw new Exception("Met By filter not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#endedBy(net.opengis
     * .filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Object endedBy(BinaryTemporalOpType op, Object obj) throws Exception {
        // TODO
        throw new Exception("Ended By filter not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#anyInteracts(net
     * .opengis.filter.v_2_0_0.BinaryTemporalOpType, java.lang.Object)
     */
    @Override
    public Object anyInteracts(BinaryTemporalOpType op, Object obj)
            throws Exception {
        // TODO
        throw new Exception("Any Interacts filter not supported");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.Filter2Visitor#id(java.util.List,
     * java.lang.Object)
     */
    @Override
    public Criterion id(List<AbstractIdType> ids, Object obj) throws Exception {
        VisitorBag bag = (VisitorBag) obj;
        Disjunction dis = Restrictions.disjunction();
        for (AbstractIdType id : ids) {
            if (id instanceof ResourceIdType) {
                ResourceIdType rid = (ResourceIdType) id;
                Criterion crit = Restrictions
                        .eq(bag.getIdField(), rid.getRid());
                dis.add(crit);
            }
        }
        return dis;
    }

}
