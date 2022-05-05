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

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.AssociationTypes;
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
 * 3/13/2013    1082       bphillip    Modified to use spring injection and transaction boundaries
 * 4/9/2013     1802       bphillip    Removed exception catching
 * 2/13/2014    2769       bphillip    Added read only flags to query methods
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class RoleDao extends RegistryObjectTypeDao<RoleType> {

    private AssociationDao associationDao;

    /**
     * Creates a new Role data access object
     */
    public RoleDao() {

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
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public RoleType getUserRole(String user) throws EbxmlRegistryException {
        List<AssociationType> associations = associationDao.getBySourceAndType(
                user, AssociationTypes.HAS_ROLE);
        if (associations.isEmpty()) {
            return null;
        }
        AssociationType roleAssociation = associations.get(0);
        return this.getById(roleAssociation.getTargetObject());
    }

    public void setAssociationDao(AssociationDao associationDao) {
        this.associationDao = associationDao;
    }

    @Override
    protected Class<RoleType> getEntityClass() {
        return RoleType.class;
    }

}
