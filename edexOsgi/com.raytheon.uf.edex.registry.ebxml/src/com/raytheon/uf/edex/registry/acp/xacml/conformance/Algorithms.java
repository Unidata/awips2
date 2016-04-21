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
package com.raytheon.uf.edex.registry.acp.xacml.conformance;

/**
 * 
 * Rule and policy combining algorithms as defined in table 10.2.3 in the XACML
 * 2.0 core spec
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
public class Algorithms {

    public static final String RULE_DENY_OVERRIDES = "urn:oasis:names:tc:xacml:1.0:rule-combining-algorithm:deny-overrides";

    public static final String POLICY_DENY_OVERRIDES = "urn:oasis:names:tc:xacml:1.0:policy-combining-algorithm:deny-overrides";

    public static final String RULE_PERMIT_OVERRIDES = "urn:oasis:names:tc:xacml:1.0:rule-combining-algorithm:permit-overrides";

    public static final String POLICY_PERMIT_OVERRIDES = "urn:oasis:names:tc:xacml:1.0:policy-combining-algorithm:permit-overrides";

    public static final String RULE_FIRST_APPLICABLE = "urn:oasis:names:tc:xacml:1.0:rule-combining-algorithm:first-applicable";

    public static final String POLICY_FIRST_APPLICABLE = "urn:oasis:names:tc:xacml:1.0:policy-combining-algorithm:first-applicable";

    public static final String POLICY_ONLY_ONE_APPLICABLE = "urn:oasis:names:tc:xacml:1.0:policy-combining-algorithm:only-one-applicable";

    public static final String RULE_ORDERED_DENY_OVERRIDES = "urn:oasis:names:tc:xacml:1.1:rule-combining-algorithm:ordered-deny-overrides";

    public static final String POLICY_ORDERED_DENY_OVERRIDES = "urn:oasis:names:tc:xacml:1.1:policy-combining-algorithm:ordered-deny-overrides";

    public static final String RULE_ORDERED_PERMIT_OVERRIDES = "urn:oasis:names:tc:xacml:1.1:rule-combining-algorithm:ordered-permit-overrides";

    public static final String POLICY_ORDERED_PERMIT_OVERRIDES = "urn:oasis:names:tc:xacml:1.1:policy-combining-algorithm:ordered-permit-overrides";
}
