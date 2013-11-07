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
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2011            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      renamed for standards
 *
 */
package com.raytheon.uf.edex.wfs.filter.v2_0_0;

import java.util.List;

import net.opengis.filter.v_2_0_0.AbstractIdType;
import net.opengis.filter.v_2_0_0.BBOXType;
import net.opengis.filter.v_2_0_0.BinarySpatialOpType;
import net.opengis.filter.v_2_0_0.BinaryTemporalOpType;
import net.opengis.filter.v_2_0_0.DistanceBufferType;
import net.opengis.filter.v_2_0_0.PropertyIsLikeType;
import net.opengis.filter.v_2_0_0.PropertyIsNilType;
import net.opengis.filter.v_2_0_0.PropertyIsNullType;

/**
 * Filter 2.0 visitor interface
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
public interface IFilter2Visitor {

    // comparison

    /**
     * Filter on property equal to value
     * 
     * @param left
     *            expression processor for left side of operator
     * @param right
     *            expression processor for right side of operator
     * @param matchCase
     *            true if comparison is case sensitive
     * @param obj
     * @return
     * @throws Exception
     */
    public Object equal(ExpressionProcessor left, ExpressionProcessor right,
            boolean matchCase, Object obj) throws Exception;

    /**
     * Filter on property not equal to value
     * 
     * @param left
     *            expression processor for left side of operator
     * @param right
     *            expression processor for right side of operator
     * @param matchCase
     *            true if comparison is case sensitive
     * @param obj
     * @return
     * @throws Exception
     */
    public Object notEqual(ExpressionProcessor left, ExpressionProcessor right,
            boolean matchCase, Object obj) throws Exception;

    /**
     * Filter on property less than value
     * 
     * @param left
     *            expression processor for left side of operator
     * @param right
     *            expression processor for right side of operator
     * @param matchCase
     *            true if comparison is case sensitive
     * @param obj
     * @return
     * @throws Exception
     */
    public Object lessThan(ExpressionProcessor left, ExpressionProcessor right,
            boolean matchCase, Object obj) throws Exception;

    /**
     * Filter on property greater than value
     * 
     * @param left
     *            expression processor for left side of operator
     * @param right
     *            expression processor for right side of operator
     * @param matchCase
     *            true if comparison is case sensitive
     * @param obj
     * @return
     * @throws Exception
     */
    public Object greaterThan(ExpressionProcessor left,
            ExpressionProcessor right, boolean matchCase, Object obj)
            throws Exception;

    /**
     * Filter on property greater than or equal to value
     * 
     * @param left
     *            expression processor for left side of operator
     * @param right
     *            expression processor for right side of operator
     * @param matchCase
     *            true if comparison is case sensitive
     * @param obj
     * @return
     * @throws Exception
     */
    public Object greaterThanEqual(ExpressionProcessor left,
            ExpressionProcessor right, boolean matchCase, Object obj)
            throws Exception;

    /**
     * Filter on property less than or equal to value
     * 
     * @param left
     *            expression processor for left side of operator
     * @param right
     *            expression processor for right side of operator
     * @param matchCase
     *            true if comparison is case sensitive
     * @param obj
     * @return
     * @throws Exception
     */
    public Object lessThanEqual(ExpressionProcessor left,
            ExpressionProcessor right, boolean matchCase, Object obj)
            throws Exception;

    /**
     * Filter on matching
     * 
     * @param op
     *            like operator
     * @param obj
     * @return
     * @throws Exception
     */
    public Object isLike(PropertyIsLikeType op, Object obj) throws Exception;

    /**
     * Filter on property being null
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object isNull(PropertyIsNullType op, Object obj) throws Exception;

    /**
     * Filter on property being nil
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object isNil(PropertyIsNilType op, Object obj) throws Exception;

    public Object between(ExpressionProcessor lower, ExpressionProcessor exp,
            ExpressionProcessor upper, Object obj) throws Exception;

    // logic

    /**
     * Conjunction for filters
     * 
     * @param filters
     *            filters to be conjoined
     * @param obj
     * @return
     * @throws Exception
     */
    public Object and(List<Filter2Processor> filters, Object obj)
            throws Exception;

    /**
     * Disjunction for filters
     * 
     * @param filters
     *            filters to be disjoined
     * @param obj
     * @return
     * @throws Exception
     */
    public Object or(List<Filter2Processor> filters, Object obj)
            throws Exception;

    /**
     * Logical inversion operator
     * 
     * @param filter
     *            filter to be inverted
     * @param obj
     * @return
     * @throws Exception
     */
    public Object not(Filter2Processor filter, Object obj) throws Exception;

    // spatial

    /**
     * Filter on spatial equality
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object spatialEquals(BinarySpatialOpType op, Object obj)
            throws Exception;

    /**
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object disjoint(BinarySpatialOpType op, Object obj) throws Exception;

    /**
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object touches(BinarySpatialOpType op, Object obj) throws Exception;

    /**
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object within(BinarySpatialOpType op, Object obj) throws Exception;

    /**
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object overlaps(BinarySpatialOpType op, Object obj) throws Exception;

    /**
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object crosses(BinarySpatialOpType op, Object obj) throws Exception;

    /**
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object intersects(BinarySpatialOpType op, Object obj)
            throws Exception;

    /**
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object contains(BinarySpatialOpType op, Object obj) throws Exception;

    /**
     * Filter on distance within
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object dWithin(DistanceBufferType op, Object obj) throws Exception;

    /**
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object beyond(DistanceBufferType op, Object obj) throws Exception;

    /**
     * Filter on bounding box
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object bbox(BBOXType op, Object obj) throws Exception;

    // temporal

    /**
     * tests if time instance/range is after other time instance/range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object after(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time instance/range is before other time instance/range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object before(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time instance is the start of time range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object begins(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time instance is the end of time range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object ends(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time range is started by time instance
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object begunBy(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time range contains time instance
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object tContains(BinaryTemporalOpType op, Object obj)
            throws Exception;

    /**
     * tests if time instance is contained by time range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object during(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time instance/range is equal to other time instance/range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object tEquals(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time range overlaps other time range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object tOverlaps(BinaryTemporalOpType op, Object obj)
            throws Exception;

    /**
     * tests if time range is right before other time range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object meets(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time range is overlapped by other time range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object overlappedBy(BinaryTemporalOpType op, Object obj)
            throws Exception;

    /**
     * tests if time range is right after other time range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object metBy(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time range is ended by time instance
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object endedBy(BinaryTemporalOpType op, Object obj) throws Exception;

    /**
     * tests if time range intersects other time range
     * 
     * @param op
     * @param obj
     * @return
     * @throws Exception
     */
    public Object anyInteracts(BinaryTemporalOpType op, Object obj)
            throws Exception;

    // id

    /**
     * Filter on list of feature identifiers
     * 
     * @param ids
     * @param obj
     * @return
     * @throws Exception
     */
    public Object id(List<AbstractIdType> ids, Object obj) throws Exception;
}
