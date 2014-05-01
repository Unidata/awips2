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

import java.util.List;

import net.opengis.filter.v_1_1_0.BBOXType;
import net.opengis.filter.v_1_1_0.BinarySpatialOpType;
import net.opengis.filter.v_1_1_0.DistanceBufferType;
import net.opengis.filter.v_1_1_0.PropertyIsLikeType;
import net.opengis.filter.v_1_1_0.PropertyIsNullType;

/**
 * Visitor Pattern interface for parsing OGC Filter objects
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
public interface OgcFilterVisitor {

	// comparison

	public Object equal(ExpressionProcessor left, ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception;

	public Object notEqual(ExpressionProcessor left, ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception;

	public Object lessThan(ExpressionProcessor left, ExpressionProcessor right,
			boolean matchCase, Object obj) throws Exception;

	public Object greaterThan(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception;

	public Object greaterThanEqual(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception;

	public Object lessThanEqual(ExpressionProcessor left,
			ExpressionProcessor right, boolean matchCase, Object obj)
			throws Exception;

	public Object isLike(PropertyIsLikeType op, Object obj) throws Exception;

	public Object isNull(PropertyIsNullType op, Object obj) throws Exception;

	public Object between(ExpressionProcessor lower, ExpressionProcessor exp,
			ExpressionProcessor upper, Object obj) throws Exception;

	// logic

	public Object and(List<FilterProcessor> filters, Object obj)
			throws Exception;

	public Object or(List<FilterProcessor> filters, Object obj)
			throws Exception;

	public Object not(FilterProcessor filter, Object obj) throws Exception;

	// spatial

	public Object spatialEquals(BinarySpatialOpType op, Object obj)
			throws Exception;

	public Object disjoint(BinarySpatialOpType op, Object obj) throws Exception;

	public Object touches(BinarySpatialOpType op, Object obj) throws Exception;

	public Object within(BinarySpatialOpType op, Object obj) throws Exception;

	public Object overlaps(BinarySpatialOpType op, Object obj) throws Exception;

	public Object crosses(BinarySpatialOpType op, Object obj) throws Exception;

	public Object intersects(BinarySpatialOpType op, Object obj)
			throws Exception;

	public Object contains(BinarySpatialOpType op, Object obj) throws Exception;

	public Object dWithin(DistanceBufferType op, Object obj) throws Exception;

	public Object beyond(DistanceBufferType op, Object obj) throws Exception;

	public Object bbox(BBOXType op, Object obj) throws Exception;

    // id

    public Object id(List<String> ids, Object obj) throws Exception;

}
