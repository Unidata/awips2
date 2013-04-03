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
 * This class holds the canonical ClassificationNodes are defined for the
 * DeletionScopeType ClassificationScheme. These constants govern deletion
 * behavior of registry objects
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/9/2012     184         bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class DeletionScope {

    /**
     * Specifies that the request MUST delete both the RegistryObject and the
     * RepositoryItem (if any) for the specified objects
     **/
    public static final String DELETE_ALL = "urn:oasis:names:tc:ebxml-regrep:DeletionScopeType:DeleteAll";

    /**
     * Specifies that the server MUST delete the RepositoryItem for the
     * specified ExtrinsicObjects but MUST NOT delete the specified
     * ExtrinsicObjects
     */
    public static final String DELETE_REPOSITORY_ITEM_ONLY = "urn:oasis:names:tc:ebxml-regrep:DeletionScopeType:DeleteRepositoryItemOnly";

}
