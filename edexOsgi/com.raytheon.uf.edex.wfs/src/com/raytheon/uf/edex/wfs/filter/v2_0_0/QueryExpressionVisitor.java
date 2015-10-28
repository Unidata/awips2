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

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.wfs.provider.VisitorBag;

/**
 * Visitor for query expressions
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
