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
 * Apr 21, 2011            bclement     Initial creation
 * Aug 18, 2013  #2097     dhladky      renamed for standards
 *
 */
package com.raytheon.uf.edex.wfs.filter.v2_0_0;

import java.util.List;

/**
 * Visitor interface for expressions
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
public interface IExpressionVisitor {

    /**
     * Process literal values
     * 
     * @param values
     * @param obj
     * @return
     * @throws Exception
     */
	public Object literal(List<Object> values, Object obj) throws Exception;

    /**
     * Process value references
     * 
     * @param ref
     * @param obj
     * @return
     * @throws Exception
     */
    public Object valueRef(String ref, Object obj) throws Exception;

    /**
     * Process custom functions
     * 
     * @param expressions
     * @param name
     * @param obj
     * @return
     * @throws Exception
     */
    public FilterFunction function(List<ExpressionProcessor> expressions,
            String name, Object obj) throws Exception;
}
