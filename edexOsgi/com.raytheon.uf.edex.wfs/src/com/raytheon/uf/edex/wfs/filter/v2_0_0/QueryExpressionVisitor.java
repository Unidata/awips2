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
 * Apr 22, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.filter.v2_0_0;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.wfs.provider.VisitorBag;

/**
 * Visitor for query expressions
 * 
 * @author bclement
 * @version 1.0
 */
public class QueryExpressionVisitor implements IExpressionVisitor {

    protected IUFStatusHandler log = UFStatus.getHandler(this.getClass());

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
     * com.raytheon.uf.edex.filter.OgcExpressionVisitor#function(java.util.List,
     * java.lang.String, java.lang.Object)
     */
    @Override
    public FilterFunction function(List<ExpressionProcessor> expressions,
            String name, Object obj) throws Exception {
        List<Object> args = new ArrayList<Object>();
        for (ExpressionProcessor expression : expressions) {
            Object tmp = expression.accept(this, obj);
            if (tmp instanceof String && obj instanceof VisitorBag) {
                // attempts to map it to the proper field for SQL processing
                String[] unfilteredPath = parseProp((String) tmp);
                String unfilteredField = StringUtils.join(unfilteredPath, ".");
                tmp = ((VisitorBag) obj).filterField(unfilteredField);
            }
            args.add(tmp);
        }
        String[] a = new String[args.size()];
        return FilterFunction.create(name, args.toArray(a));
    }

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

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.wfs.filter.v2_0_0.ExpressionVisitor#valueRef(net
     * .opengis.wfs.v_2_0_0.PropertyType.ValueReference, java.lang.Object)
     */
    @Override
    public Object valueRef(String ref, Object obj) throws Exception {
        return ref;
    }

}
