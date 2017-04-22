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
package com.raytheon.uf.edex.registry.acp.xacml.engine.policy;

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.ctx.StatusCodeType;
import org.opensaml.xacml.policy.AttributeDesignatorType;
import org.opensaml.xacml.policy.AttributeSelectorType;
import org.opensaml.xacml.policy.AttributeValueType;
import org.opensaml.xacml.policy.ExpressionType;

import com.raytheon.uf.edex.registry.acp.xacml.engine.expression.XACMLExpressionEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.engine.function.XACMLFunctionEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLNotApplicableException;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Base class for implementations used to evaluate various elements of the
 * policy
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
public abstract class ElementEvaluator<T extends XACMLObject> {

    /** The current request object */
    protected RequestType request;

    /**
     * Evaluates the current policy element
     * 
     * @param obj
     *            The policy element to evaluate against the request
     * @return The result of evaluating the policy element against the request
     *         object
     * @throws XACMLProcessingException
     *             If errors occur while evaluating the policy element
     */
    protected abstract Match evaluate(T obj) throws XACMLProcessingException;

    /**
     * Gets the ID of this evaluator
     * 
     * @return The ID of this evaluator
     */
    public abstract String getId();

    /**
     * Bootstrap method used by the Evaluator used to set the request and call
     * the abstract evaluate method
     * 
     * @param obj
     *            The policy element
     * @param request
     *            The request object
     * @return The result of evaluating the policy element agains the request
     *         object
     * @throws XACMLProcessingException
     *             If errors occur while evaluating the policy element
     */
    protected Match eval(T obj, RequestType request)
            throws XACMLProcessingException {
        this.request = request;
        return evaluate(obj);
    }

    /**
     * Convenience method to allow recursive calls to the Evaluator for
     * evaluating embedded policy elements
     * 
     * @param obj
     *            The policy element
     * @return The result of evaluation
     * @throws XACMLProcessingException
     *             If errors occur while evaluating the policy element
     */
    protected Match evaluateElement(XACMLObject obj)
            throws XACMLProcessingException {
        return Evaluator.getInstance().evaluate(obj, request);
    }

    /**
     * Convenience method used to evaluate expression contained inside policy
     * elements
     * 
     * @param <R>
     *            The return type
     * @param currentExpression
     *            The expression object
     * @return The result of evaluating the expression
     * @throws XACMLException
     *             If errors occur when evaluating the expression
     */
    protected <R> R evaluateExpression(ExpressionType currentExpression)
            throws XACMLException {
        return XACMLExpressionEvaluator.getInstance().evaluate(
                currentExpression, request);
    }

    /**
     * Convenience method used to evaluate the function contained inside a
     * policy element
     * 
     * @param <R>
     *            The return type
     * @param functionId
     *            The id of the function to execute
     * @param args
     *            The arguments being passed to the function
     * @return The result of evaluating the function
     * @throws XACMLException
     *             If errors ocur when evaluating the function
     */
    protected <R> R evaluateFunction(String functionId, Object[] args)
            throws XACMLException {
        return XACMLFunctionEvaluator.getInstance().evaluate(functionId, args,
                request);
    }

    /**
     * Evaluates a matcher. Matchers can be subject, resource, action, or
     * environment matchers
     * 
     * @param matchId
     *            The ID of the matcher
     * @param attributeValue
     *            The value of the attribute
     * @param designator
     *            The attribute designator from the matcher
     * @param selector
     *            The attribute selector from the matcher. The selector element
     *            is current not supported by this implementation
     * @return The result of evaluating the matcher
     */
    protected Match evaluateMatcher(String matchId,
            AttributeValueType attributeValue,
            AttributeDesignatorType designator, AttributeSelectorType selector) {
        Match retVal = null;
        String statusCode = StatusCodeType.SC_OK;
        Boolean result = null;
        if (selector != null) {
            selector.getRequestContextPath();
            throw new UnsupportedOperationException(
                    "Attribute Selector not supported!");
        }
        try {
            result = evaluateFunction(matchId, new Object[] {
                    evaluateExpression(attributeValue),
                    evaluateExpression(designator) });
        } catch (Exception e) {
            e.printStackTrace();
            if (e instanceof XACMLException) {
                statusCode = ((XACMLException) e).getStatusCode();
                if (e instanceof XACMLNotApplicableException) {
                    result = false;
                }
            } else {
                statusCode = StatusCodeType.SC_PROCESSING_ERROR;
            }
        }
        if (result == null) {
            retVal = Match.indeterminate(statusCode);
        } else if (result) {
            retVal = Match.PERMIT;
        } else {
            retVal = Match.DENY;
        }
        return retVal;
    }

    /**
     * Applies the truth table shown in Table 3 of section 7.6 of the XACML core
     * specification shown below. SubjectMatch is used as an example but it
     * applies to resource, action, and environment as well.
     * <p>
     * <table border="1">
     * <tr>
     * <th>SubjectMatch Values</th>
     * <th>Subject Value</th>
     * </tr>
     * <tr>
     * <td>All "True"</td>
     * <td>"Match"</td>
     * </tr>
     * <tr>
     * <td>No "False" and at least one "Indeterminate"</td>
     * <td>"Indeterminate"</td>
     * </tr>
     * <tr>
     * <td>At least one "False</td>
     * <td>"No match"</td>
     * </tr>
     * </table>
     * 
     * @param results
     *            The matcher results to combine into a single match result
     * @return The match result
     */
    protected Match applyMatcherTruthTable(Match[] results) {
        int matches = 0;
        int indeterminate = 0;
        int noMatches = 0;
        String statusCode = null;
        for (Match match : results) {
            switch (match.getMatch()) {
            case Indeterminate:
                statusCode = match.getStatusCode();
                indeterminate++;
                break;
            case Permit:
                matches++;
                break;
            case Deny:
                noMatches++;
                break;

            }
        }
        if (matches == results.length) {
            return Match.PERMIT;
        } else if (noMatches == 0 && indeterminate > 0) {
            return Match.indeterminate(statusCode);
        }
        return Match.DENY;
    }

    /**
     * Applies the truth table shown in Table 2 of section 7.6 of the XACML core
     * spec
     * <p>
     * <table border="1">
     * <tr>
     * <th>Subject Values</th>
     * <th>Subjects Value</th>
     * </tr>
     * <tr>
     * <td>At least one "Match"</td>
     * <td>"Match"</td>
     * </tr>
     * <tr>
     * <td>None matches and at least on "Indeterminate"</td>
     * <td>"Indeterminate"</td>
     * </tr>
     * <tr>
     * <td>All "No Match"</td>
     * <td>"No match"</td>
     * </tr>
     * </table>
     * 
     * @param results
     *            The matcher results to combine into a single match result
     * @return The match result
     */
    protected Match applyPolicyElementTruthTable(Match[] results) {
        int matches = 0;
        int indeterminate = 0;
        int noMatches = 0;
        String statusCode = null;
        for (Match match : results) {
            switch (match.getMatch()) {
            case Permit:
                matches++;
                break;
            case Deny:
                noMatches++;
                break;
            case Indeterminate:
                statusCode = match.getStatusCode();
                indeterminate++;
                break;
            }
        }

        if (matches >= 1) {
            return Match.PERMIT;
        } else if (noMatches == results.length) {
            return Match.DENY;
        }
        return Match.indeterminate(statusCode);
    }
}
