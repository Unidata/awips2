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
package com.raytheon.uf.edex.registry.acp.xacml.engine.function;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opensaml.xacml.ctx.RequestType;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;

/**
 * 
 * Class used to evaluate XACML functions
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
public class XACMLFunctionEvaluator {

    /** The singleton instance */
    private static XACMLFunctionEvaluator instance = new XACMLFunctionEvaluator();

    /** The map of functions available to the system */
    private static Map<String, XACMLFunction> functionMap = new HashMap<String, XACMLFunction>();

    private List<XACMLFunction> functionList;

    /**
     * Private constructor
     */
    private XACMLFunctionEvaluator() {

    }

    /**
     * Gets the singleton instance of the XACMLFunctionEvaluator
     * 
     * @return The singleton instance of the XACMLFunctionEvaluator
     */
    public static XACMLFunctionEvaluator getInstance() {
        return instance;
    }

    /**
     * Evaluates the function specified by the function ID with the given
     * arguments
     * 
     * @param <T>
     *            The object type
     * @param functionId
     *            The id of the function to execute
     * @param args
     *            The arguments to be passed to the function
     * @param request
     *            The request object
     * @return The result of the evaluation of the function
     * @throws XACMLException
     *             If errors occur while evaluating the function
     */
    @SuppressWarnings("unchecked")
    public <T> T evaluate(String functionId, Object[] args, RequestType request)
            throws XACMLException {

        XACMLFunction function = functionMap.get(functionId);
        if (function == null) {
            throw new UnsupportedOperationException("Unknown Function: "
                    + functionId);
        }
        // Sets the current request object on the function
        function.setRequest(request);
        return (T) function.execute(args);
    }

    public List<XACMLFunction> getFunctionList() {
        return functionList;
    }

    public void setFunctionList(List<XACMLFunction> functionList) {
        this.functionList = functionList;
        for (XACMLFunction function : functionList) {
            functionMap.put(function.getFunctionId(), function);
        }
    }

}
