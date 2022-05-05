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
 * Identifier prefixes as defined in table 10.2.2 in the XACML 2.0 core spec
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
public class IdentifierPrefixes {

    public static final String STATUS = "urn:oasis:names:tc:xacml:1.0:status";

    public static final String ENVIRONMENT = "urn:oasis:names:tc:xacml:1.0:environment";

    public static final String ACTION = "urn:oasis:names:tc:xacml:1.0:action";

    public static final String RESOURCE = "urn:oasis:names:tc:xacml:1.0:resource";

    public static final String SUBJECT = "urn:oasis:names:tc:xacml:1.0:subject";

    public static final String POLICY = "urn:oasis:names:tc:xacml:2.0:policy";

    public static final String FUNCTION_1 = "urn:oasis:names:tc:xacml:1.0:function";

    public static final String FUNCTION_2 = "urn:oasis:names:tc:xacml:2.0:function";

    public static final String EXAMPLE_1 = "urn:oasis:names:tc:xacml:1.0:example";

    public static final String EXAMPLE_2 = "urn:oasis:names:tc:xacml:2.0:example";

    public static final String CONTEXT = "urn:oasis:names:tc:xacml:2.0:context";

    public static final String CONFORMANCE_TEST = "urn:oasis:names:tc:xacml:2.0:conformance-test";

    public static final String CONFORMANCE_TESTS = "urn:oasis:names:tc:xacml:2.0:conformance-tests";

    public static final String XACML2_0 = "urn:oasis:names:tc:xacml:2.0";

    public static final String[] prefixes;
    static {
        prefixes = new String[14];
        prefixes[0] = STATUS;
        prefixes[1] = ENVIRONMENT;
        prefixes[2] = ACTION;
        prefixes[3] = RESOURCE;
        prefixes[4] = SUBJECT;
        prefixes[5] = POLICY;
        prefixes[6] = FUNCTION_1;
        prefixes[7] = FUNCTION_2;
        prefixes[8] = EXAMPLE_1;
        prefixes[9] = EXAMPLE_2;
        prefixes[10] = CONTEXT;
        prefixes[11] = CONFORMANCE_TESTS;
        prefixes[12] = CONFORMANCE_TEST;
        prefixes[13] = XACML2_0;
    }
}
