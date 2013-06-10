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
package com.raytheon.uf.common.registry.constants;

/**
 * 
 * Notification delivery types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/9/2013     1802        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class DeliveryMethodTypes {

    public static final String SOAP = "urn:oasis:names:tc:ebxml-regrep:endPointType:soap";

    public static final String REST = "urn:oasis:names:tc:ebxml-regrep:endPointType:rest";

    public static final String EMAIL = "urn:oasis:names:tc:ebxml-regrep:endPointType:mail";

    public static final String PLUGIN = "urn:oasis:names:tc:ebxml-regrep:endPointType:plugin";

}
