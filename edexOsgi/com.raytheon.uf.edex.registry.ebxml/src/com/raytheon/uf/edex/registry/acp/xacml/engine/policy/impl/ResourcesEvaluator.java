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

import org.opensaml.xacml.policy.ResourceType;
import org.opensaml.xacml.policy.ResourcesType;
import org.opensaml.xacml.policy.impl.ResourcesTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Evaluator implementation for evaluating the <Resources> element of a policy
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
public class ResourcesEvaluator extends ElementEvaluator<ResourcesType> {

    public String getId() {
        return ResourcesTypeImpl.class.getName();
    }

    @Override
    protected Match evaluate(ResourcesType resource)
            throws XACMLProcessingException {
        Match retVal = Match.PERMIT;
        if (resource != null) {
            List<ResourceType> resourceTypes = resource.getResources();
            Match[] matches = new Match[resourceTypes.size()];
            for (int i = 0; i < resourceTypes.size(); i++) {
                matches[i] = evaluateElement(resourceTypes.get(i));
            }
            retVal = applyPolicyElementTruthTable(matches);
        }
        return retVal;
    }

}
