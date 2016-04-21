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
package com.raytheon.uf.edex.registry.acp.xacml.engine.combinealgorithms;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.RequestType;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Class used to evaluate policy and rule combination algorithms
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
public class CombinerAlgorithmEvaluator {

    /** Map of combiner algorithms present in the system */
    private static Map<String, CombinerAlgorithm> algorithmMap = new HashMap<String, CombinerAlgorithm>();

    /** The singleton instance */
    private static CombinerAlgorithmEvaluator instance = new CombinerAlgorithmEvaluator();

    private List<CombinerAlgorithm> algorithmList;

    /**
     * Gets the singleton instance of the CombinerAlgorithmEvaluator
     * 
     * @return The singleton instance of the CombinerAlgorithmEvaluator
     */
    public static CombinerAlgorithmEvaluator getInstance() {
        return instance;
    }

    /**
     * Private constructor
     */
    private CombinerAlgorithmEvaluator() {

    }

    /**
     * Combines the received decisions based on the specified combiner algorithm
     * ID
     * 
     * @param algorithmId
     *            The algorithm to apply to the provided matches
     * @param decisions
     *            The decisions received from evaluating policies or rules
     * @param objects
     *            The objects from which the decisions were derived. For
     *            policies, this will be a list of rules. For PolicySets this
     *            will be a list of PolicySets, Policies, and/or ID references.
     *            This list of objects is required due to the fact that several
     *            combiner algorithms require reevaluation of rules or policies
     * @param request
     *            The XACML request object
     * @return The combiner decision
     * @throws XACMLProcessingException
     *             If there are issues encountered while evaluating the combiner
     *             algorithm
     */
    public Match combine(String algorithmId, Match[] decisions,
            List<XACMLObject> objects, RequestType request)
            throws XACMLProcessingException {
        CombinerAlgorithm algorithm = algorithmMap.get(algorithmId);
        if (algorithm == null) {
            throw new IllegalArgumentException(
                    "No combiner algorithm implemented for id [" + algorithmId
                            + "]");
        }
        return algorithm.evaluate(decisions, objects, request);
    }

    public List<CombinerAlgorithm> getAlgorithmList() {
        return algorithmList;
    }

    public void setAlgorithmList(List<CombinerAlgorithm> algorithmList) {
        this.algorithmList = algorithmList;
        for (CombinerAlgorithm algo : algorithmList) {
            algorithmMap.put(algo.getAlgorithmId(), algo);
        }
    }

}
