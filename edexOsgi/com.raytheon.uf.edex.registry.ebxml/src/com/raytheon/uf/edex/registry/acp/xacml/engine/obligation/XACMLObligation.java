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
package com.raytheon.uf.edex.registry.acp.xacml.engine.obligation;

import java.lang.reflect.Method;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;

/**
 * 
 * Class that evaluates policy obligations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public abstract class XACMLObligation {

    /** The method that is executed when evaluating this obligation */
    protected Method method;

    /** Gets the ID of this obligation */
    public abstract String getObligationId();

    /**
     * Creates a new Obligation
     */
    public XACMLObligation() {
        Method[] methods = this.getClass().getMethods();
        for (Method method : methods) {
            if (method.getName().equals("evaluate")) {
                this.method = method;
                break;
            }
        }
    }

    /**
     * Evaluates this obligation with the given arguments
     * 
     * @param <T>
     *            The object type
     * @param args
     *            The arguments to be used to evaluate the obligation
     * @return The result of the obligation evaluation
     * @throws XACMLProcessingException
     *             If problems occur when evaluating the obligation
     */
    @SuppressWarnings("unchecked")
    protected <T> T evaluate(Object[] args) throws XACMLProcessingException {
        if (method == null) {
            throw new XACMLProcessingException(
                    "Obligation implementation does not implement the 'evaluate' method.  Unable to evaluate.");
        }
        try {
            return (T) this.method.invoke(this, args);
        } catch (Exception e) {
            throw new XACMLProcessingException("Error evaluating obligation", e);
        }

    }
}
