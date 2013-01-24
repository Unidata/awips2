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
package com.raytheon.uf.edex.registry.ebxml.constants;

/**
 * 
 * Subject Roles
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SubjectRoles {

    public static final String CHANGE_PROPOSAL_RECEIVER = "urn:oasis:names:tc:ebxml-regrep:SubjectRole:ChangeProposalReceiver";

    public static final String CHANGE_PROPOSAL_REVIEWER = "urn:oasis:names:tc:ebxml-regrep:SubjectRole:ChangeProposalReviewer";

    public static final String CHANGE_PROPOSAL_SUBMITTER = "urn:oasis:names:tc:ebxml-regrep:SubjectRole:ChangeProposalSubmitter";

    public static final String CONTENT_OWNER = "urn:oasis:names:tc:ebxml-regrep:SubjectRole:ContentOwner";

    public static final String REGISTRY_ADMINISTRATOR = "urn:oasis:names:tc:ebxml-regrep:SubjectRole:RegistryAdministrator";

    public static final String REGISTRY_GUEST = "urn:oasis:names:tc:ebxml-regrep:SubjectRole:RegistryGuest";
}
