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
 * Identifiers as defined in table 10.2.6 in the XACML 2.0 core spec
 * <p>
 * 
 * The implementation MUST use the attributes associated with the following
 * identifiers in the way XACML has defined. This requirement pertains primarily
 * to implementations of a PAP or PEP that uses XACML, since the semantics of
 * the attributes are transparent to the PDP.
 * 
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
public class Identifiers {
    public static final String DNS_NAME = "urn:oasis:names:tc:xacml:1.0:subject:authn-locality:dns-name";

    public static final String IP_ADDRESS = "urn:oasis:names:tc:xacml:1.0:subject:authn-locality:ip-address";

    public static final String AUTHENTICATION_METHOD = "urn:oasis:names:tc:xacml:1.0:subject:authentication-method";

    public static final String AUTHENTICATION_TIME = "urn:oasis:names:tc:xacml:1.0:subject:authentication-time";

    public static final String KEY_INFO = "urn:oasis:names:tc:xacml:1.0:subject:key-info";

    public static final String REQUEST_TIME = "urn:oasis:names:tc:xacml:1.0:subject:request-time";

    public static final String SESSION_START_TIME = "urn:oasis:names:tc:xacml:1.0:subject:session-start-time";

    public static final String SUBJECT_ID = "urn:oasis:names:tc:xacml:1.0:subject:subject-id";

    public static final String SUBJECT_ID_QUALIFIER = "urn:oasis:names:tc:xacml:1.0:subject:subject-id-qualifier";

    public static final String ACCESS_SUBJECT = "urn:oasis:names:tc:xacml:1.0:subject-category:access-subject";

    public static final String CODEBASE = "urn:oasis:names:tc:xacml:1.0:subject-category:codebase";

    public static final String INTERMEDIARY_SUBJECT = "urn:oasis:names:tc:xacml:1.0:subject-category:intermediary-subject";

    public static final String RECIPIENT_SUBJECT = "urn:oasis:names:tc:xacml:1.0:subject-category:recipient-subject";

    public static final String REQUESTING_MACHINE = "urn:oasis:names:tc:xacml:1.0:subject-category:requesting-machine";

    public static final String RESOURCE_LOCATION = "urn:oasis:names:tc:xacml:1.0:resource:resource-location";

    public static final String RESOURCE_ID = "urn:oasis:names:tc:xacml:1.0:resource:resource-id";

    public static final String SIMPLE_FILE_NAME = "urn:oasis:names:tc:xacml:1.0:resource:simple-file-name";

    public static final String ACTION_ID = "urn:oasis:names:tc:xacml:1.0:action:action-id";

    public static final String IMPLIED_ACTION = "urn:oasis:names:tc:xacml:1.0:action:implied-action";
}
