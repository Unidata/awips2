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

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * Data Access object for interacting with associations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 7/30/2012    724        bphillip     Initial creation
 * 3/18/2013    1802         bphillip    Modified to use transaction boundaries and spring injection
 * 4/9/2013     1802        bphillip     Removed exception catching
 * 2/13/2014    2769        bphillip     Added read only flags to query methods
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class AssociationDao extends RegistryObjectTypeDao<AssociationType> {

    /**
     * Gets associations based on the association target and type
     * 
     * @param target
     *            The target of the association
     * @param type
     *            The type of the association
     * @return Associations matching the given criteria
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<AssociationType> getByTargetAndType(String target, String type) {
        return executeHQLQuery("from AssociationType obj where obj.targetObject='"
                + target + "' and obj.type='" + type + "'");
    }

    /**
     * Gets associations based on the association source and type
     * 
     * @param source
     *            The source of the association
     * @param type
     *            The type of the association
     * @return Associations matching the given criteria
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<AssociationType> getBySourceAndType(String source, String type) {
        return executeHQLQuery("from AssociationType obj where obj.sourceObject='"
                + source + "' and obj.type='" + type + "'");
    }

    /**
     * Gest associations based on the source, target and type of association
     * 
     * @param source
     *            The association source
     * @param target
     *            The association target
     * @param type
     *            The association type
     * @return Associations matching the given criteria
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<AssociationType> getBySourceTargetAndType(String source,
            String target, String type) {
        return executeHQLQuery("from AssociationType obj where obj.sourceObject='"
                + source
                + "' and obj.type='"
                + type
                + "' and obj.targetObject='" + target + "'");
    }

    /**
     * Gets all associations referencing the provided object ID
     * 
     * @param objReferenced
     *            The id of the object being referenced by an association
     * @return The assocations referencing the given object ID
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<AssociationType> getAllAssociations(String objReferenced) {
        return executeHQLQuery("from AssociationType obj where obj.sourceObject='"
                + objReferenced
                + "' or obj.targetObject='"
                + objReferenced
                + "'");
    }

    /**
     * Gets all associations with the target specified as the given object ID
     * 
     * @param objReferenced
     *            The object ID
     * @return All associations with the target specified as the given object ID
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<AssociationType> getAssociationsTo(String objReferenced) {
        return executeHQLQuery("from AssociationType obj where obj.targetObject='"
                + objReferenced + "'");
    }

    /**
     * Gets all associations with the source specified as the given object ID
     * 
     * @param objReferenced
     *            The object ID
     * @return All associations with the target specified as the given object ID
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<AssociationType> getAssociationsFrom(String objReferenced) {
        return executeHQLQuery("from AssociationType obj where obj.sourceObject='"
                + objReferenced + "'");
    }

    /**
     * Deletes all associations referencing the given object ID
     * 
     * @param objReferenced
     *            The object ID
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public void deleteAssociationsForObj(String objReferenced) {
        List<AssociationType> associations = getAllAssociations(objReferenced);
        if (!associations.isEmpty()) {
            for (AssociationType association : associations) {
                this.delete(association);
            }
        }
    }

    @Override
    protected Class<AssociationType> getEntityClass() {
        return AssociationType.class;
    }
}
