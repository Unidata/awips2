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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.DecisionType.DECISION;
import org.opensaml.xacml.ctx.RequestType;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Class used to evaluate elements of the policy
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
@SuppressWarnings("rawtypes")
public class Evaluator {

    private List<ElementEvaluator<?>> evaluatorList;

    /** The map of available evaluators */
    private static Map<String, ElementEvaluator> evaluatorMap = new HashMap<String, ElementEvaluator>();

    /** The singleton instance of Evaluator */
    private static Evaluator instance = new Evaluator();

    /**
     * Private constructor
     */
    private Evaluator() {

    }

    /**
     * Gets the singleton instance of the Evaluator
     * 
     * @return The singleton instance of the Evaluator
     */
    public static Evaluator getInstance() {
        return instance;
    }

    /**
     * Evaluates the given policy element
     * 
     * @param <T>
     *            The type of policy element
     * @param obj
     *            The policy element
     * @param request
     *            The current request object
     * @return The Match result of evaluating the policy element
     * @throws XACMLProcessingException
     *             If errors occur while evaluating the policy element
     */
    @SuppressWarnings("unchecked")
    public <T extends XACMLObject> Match evaluate(T obj, RequestType request)
            throws XACMLProcessingException {
        if (obj == null) {
            return new Match(DECISION.Permit);
        }
        try {
            return getEvaluator(obj).eval(obj, request);
        } catch (Exception e) {
            throw new XACMLProcessingException("Error evaluating object", e);
        }
    }

    /**
     * Gets the evaluator for the policy element
     * 
     * @param <T>
     *            The type of policy element
     * @param obj
     *            The policy element to be evaluated
     * @return The policy element evaluator
     * @throws XACMLProcessingException
     *             If a policy element evaluator does not exist for the given
     *             policy element
     */
    private <T extends XACMLObject> ElementEvaluator getEvaluator(T obj)
            throws XACMLProcessingException {
        try {
            return (ElementEvaluator) evaluatorMap
                    .get(obj.getClass().getName());
        } catch (Exception e) {
            throw new XACMLProcessingException("Error evaluating object", e);
        }
    }

    public List<ElementEvaluator<?>> getEvaluatorList() {
        return evaluatorList;
    }

    public void setEvaluatorList(List<ElementEvaluator<?>> evaluatorList) {
        for (ElementEvaluator<?> evaluator : evaluatorList) {
            evaluatorMap.put(evaluator.getId(), evaluator);
        }
        this.evaluatorList = evaluatorList;
    }
}
