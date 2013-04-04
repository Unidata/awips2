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
package com.raytheon.uf.edex.registry.ebxml.dao;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RoleType;

import com.raytheon.uf.edex.registry.ebxml.constants.AssociationTypes;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * Data Access object for interacting with roles in the registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 7/30/2012    724        bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class RoleDao extends RegistryObjectTypeDao {

    /**
     * Creates a new Role data access object
     */
    public RoleDao() {
        super(RoleType.class);
    }

    /**
     * Gets the role of the given user
     * 
     * @param user
     *            The ID of the user to get the role for
     * @return The role of the user
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public RoleType getUserRole(String user) throws EbxmlRegistryException {
        AssociationDao associationDao = new AssociationDao();
        List<AssociationType> associations = associationDao.getBySourceAndType(
                user, AssociationTypes.HAS_ROLE);
        if (associations.isEmpty()) {
            return null;
        }
        AssociationType roleAssociation = associations.get(0);
        return this.getById(roleAssociation.getTargetObject());
    }

}
