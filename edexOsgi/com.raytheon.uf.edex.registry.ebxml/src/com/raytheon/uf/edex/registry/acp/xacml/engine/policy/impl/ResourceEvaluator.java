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
package com.raytheon.uf.edex.registry.acp.xacml.engine.policy.impl;

import java.util.List;

import org.opensaml.xacml.policy.ResourceMatchType;
import org.opensaml.xacml.policy.ResourceType;
import org.opensaml.xacml.policy.impl.ResourceTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Evaluator implementation for evaluating the <Resource> element of a policy
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
public class ResourceEvaluator extends ElementEvaluator<ResourceType> {

    public String getId() {
        return ResourceTypeImpl.class.getName();
    }

    @Override
    protected Match evaluate(ResourceType resource)
            throws XACMLProcessingException {
        List<ResourceMatchType> resourceMatches = resource.getResourceMatches();
        Match[] results = new Match[resourceMatches.size()];
        ResourceMatchType matcher = null;
        for (int i = 0; i < resourceMatches.size(); i++) {
            matcher = resourceMatches.get(i);
            results[i] = evaluateMatcher(matcher.getMatchId(),
                    matcher.getAttributeValue(),
                    matcher.getResourceAttributeDesignator(),
                    matcher.getAttributeSelector());
        }
        return applyMatcherTruthTable(results);
    }

}
