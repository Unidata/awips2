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

import java.util.List;

import org.opensaml.xacml.XACMLObject;
import org.opensaml.xacml.ctx.RequestType;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.acp.xacml.objects.Match;

/**
 * 
 * Combiner algorithm interface.
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
public interface CombinerAlgorithm {

    /** Gets the identifier for this combiner algorithm */
    public String getAlgorithmId();

    /**
     * Combines the match decisions.
     * 
     * @param decisions
     *            The array of decisions derived from evaluation of rules and
     *            policies
     * @param objs
     *            The objects from which the decisions were derived. For
     *            policies, this will be a list of rules. For PolicySets this
     *            will be a list of PolicySets, Policies, and/or ID references.
     *            This list of objects is required due to the fact that several
     *            combiner algorithms require reevaluation of rules or policies
     * @param request
     *            The XACML Request object
     * @return The match decision derived from applying the combiner logic.
     * @throws XACMLProcessingException
     *             If errors occur while evaluating the combiner algorithm
     */
    public Match evaluate(Match[] decisions, List<XACMLObject> objs,
            RequestType request) throws XACMLProcessingException;

}
