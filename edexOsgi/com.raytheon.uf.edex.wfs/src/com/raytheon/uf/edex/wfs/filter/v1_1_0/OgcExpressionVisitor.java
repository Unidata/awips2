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

import net.opengis.filter.v_1_1_0.PropertyNameType;

/**
 * Visitor Pattern interface for parsing OGC Filter Expressions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface OgcExpressionVisitor {

	public Object add(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception;

	public Object sub(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception;

	public Object mul(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception;

	public Object div(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception;

	public Object literal(List<Object> values, Object obj) throws Exception;

	public Object property(PropertyNameType prop, Object obj) throws Exception;

	public Object function(List<ExpressionProcessor> expressions, String name,
			Object obj) throws Exception;
}
