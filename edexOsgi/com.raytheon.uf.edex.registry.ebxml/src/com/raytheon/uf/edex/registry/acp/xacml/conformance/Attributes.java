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
 * Attribute values as defined in table 10.2.6 in the XACML 2.0 core spec.
 * <p>
 * 
 * The implementation MUST support the attributes associated with the following
 * identifiers as specified by XACML. If values for these attributes are not
 * present in the decision request, then their values MUST be supplied by the
 * context handler. So, unlike most other attributes, their semantics are not
 * transparent to the PDP.
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
public class Attributes {

    public static final String CURRENT_TIME = "urn:oasis:names:tc:xacml:1.0:environment:current-time";

    public static final String CURRENT_DATE = "urn:oasis:names:tc:xacml:1.0:environment:current-date";

    public static final String CURRENT_DATE_TIME = "urn:oasis:names:tc:xacml:1.0:environment:current-dateTime";
}
