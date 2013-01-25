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

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.policy.IdReferenceType;
import org.opensaml.xacml.policy.impl.IdReferenceTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.XACMLPolicyAdministrator;
import com.raytheon.uf.edex.registry.acp.xacml.engine.policy.ElementEvaluator;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * Evaluator implementation used to evaluate the <PolicySetIdReference> and
 * <PolicyIdReference> elements of a policy
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
public class IdReferenceEvaluator extends ElementEvaluator<IdReferenceType> {

    @Override
    public String getId() {
        return IdReferenceTypeImpl.class.getName();
    }

    @Override
    protected Match evaluate(IdReferenceType obj)
            throws XACMLProcessingException {
        XACMLObject policyObject = null;
        try {
            // Get the policy or policy set object referenced
            policyObject = XACMLPolicyAdministrator.getInstance().getPolicy(
                    obj.getValue());
        } catch (EbxmlRegistryException e) {
            throw new XACMLProcessingException(
                    "Error retrieving policy object with id [" + obj.getValue()
                            + "]", e);
        }
        if (policyObject == null) {
            throw new IllegalArgumentException(
                    "No policy object exists for reference [" + obj.getValue()
                            + "]");
        }
        return evaluateElement(policyObject);
    }

}
