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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;


/**
 * Parses OGC Filter Expressions to hibernate query operands
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class QueryExpressionVisitor implements OgcExpressionVisitor {

	protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#add(com.raytheon.uf.
	 * edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
	 */
	@Override
	public Object add(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression: add");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#sub(com.raytheon.uf.
	 * edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
	 */
	@Override
	public Object sub(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression: sub");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#mul(com.raytheon.uf.
	 * edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
	 */
	@Override
	public Object mul(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression: mul");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#div(com.raytheon.uf.
	 * edex.filter.ExpressionProcessor,
	 * com.raytheon.uf.edex.filter.ExpressionProcessor, java.lang.Object)
	 */
	@Override
	public Object div(ExpressionProcessor left, ExpressionProcessor right,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression: div");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#literal(java.util.List,
	 * java.lang.Object)
	 */
	@Override
	public Object literal(List<Object> values, Object obj) throws Exception {
		if (values.size() != 1) {
			log.warn("Unsupported literal values: " + values.toArray());
		}
		return values.get(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#property(net.opengis
	 * .filter.v_1_1_0.PropertyNameType, java.lang.Object)
	 */
	@Override
    public Object property(PropertyNameType prop, Object obj) throws Exception {
        List<Object> content = prop.getContent();
		if (content.size() != 1) {
			log.warn("Unsupported property name values: " + content.toArray());
		}
		return content.get(0);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.edex.filter.OgcExpressionVisitor#function(java.util.List,
	 * java.lang.String, java.lang.Object)
	 */
	@Override
	public Object function(List<ExpressionProcessor> expressions, String name,
			Object obj) throws Exception {
		throw new Exception("Unsupported expression type: function");
	}

}
