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

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * Data access object for retrieving ClassificationNodeTypes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/21/2012    184        bphillip     Initial creation
 * 8/3/2012     724        bphillip    Added more methods for getting classification nodes
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring injection
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class ClassificationNodeDao extends
        RegistryObjectTypeDao<ClassificationNodeType> {

    /** Regex to use when querying for telephone types */
    public static final String TELEPHONE_TYPE_REGEX = "urn:oasis:names:tc:ebxml-regrep:PhoneType:%";

    /** Regex to use when querying for postal address types */
    public static final String ADDRESS_TYPE_REGEX = "urn:oasis:names:tc:ebxml-regrep:PostalAddressType%";

    /** Regex to use when querying for email types */
    public static final String EMAIL_TYPE_REGEX = "urn:oasis:names:tc:ebxml-regrep:EmailType:%";

    public ClassificationNodeDao() {

    }

    /**
     * Retrieves ClassificationNode objects based on the path
     * 
     * @param path
     *            The path to get the classification node type for
     * @return The ClassificationNode object with the specified path
     * @throws EbxmlRegistryException
     *             If errors occur during the query
     */
    public ClassificationNodeType getByPath(String path)
            throws EbxmlRegistryException {
        List<ClassificationNodeType> result;
        try {
            result = this
                    .executeHQLQuery("select obj from ClassificationNodeType obj where obj.path='"
                            + path + "'");
        } catch (DataAccessLayerException e) {
            throw new EbxmlRegistryException("Data Access Error", e);
        }
        if (result.isEmpty()) {
            return null;
        } else {
            return result.get(0);
        }
    }

    /**
     * Gets the ID of the classification node given the code
     * 
     * @param code
     *            The code of the classification node
     * @return The ID of the classification node with the given code
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public String getNodeFromCode(String code) throws EbxmlRegistryException {
        List<String> results;
        try {
            results = this
                    .executeHQLQuery("select obj.id from ClassificationNodeType obj where obj.code='"
                            + code + "'");
        } catch (DataAccessLayerException e) {
            throw new EbxmlRegistryException("Data Access Error", e);
        }

        if (results.isEmpty()) {
            return null;
        } else {
            return results.get(0);
        }
    }

    /**
     * Gets the Code of the classification node given the object ID
     * 
     * @param id
     *            The object ID of the classification node
     * @return The code of the classification node with the given ID
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public String getCodeFromNode(String id) throws EbxmlRegistryException {
        List<String> results;
        try {
            results = this
                    .executeHQLQuery("select obj.code from ClassificationNodeType obj where obj.id='"
                            + id + "'");
        } catch (DataAccessLayerException e) {
            throw new EbxmlRegistryException("Data Access Error", e);
        }

        if (results.isEmpty()) {
            return null;
        } else {
            return results.get(0);
        }
    }

    /**
     * Gets the codes of the telephone types in the registry
     * 
     * @return The codes of the telephone types in the registry
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public List<String> getTelephoneTypes() throws EbxmlRegistryException {
        try {
            return this
                    .executeHQLQuery("select obj.code from ClassificationNodeType obj where obj.lid like '"
                            + TELEPHONE_TYPE_REGEX + "'");
        } catch (DataAccessLayerException e) {
            throw new EbxmlRegistryException("Data Access Error", e);
        }
    }

    /**
     * Gets the codes of the address types in the registry
     * 
     * @return The codes of the address types in the registry
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public List<String> getAddressTypes() throws EbxmlRegistryException {
        try {
            return this
                    .executeHQLQuery("select obj.code from ClassificationNodeType obj where obj.lid like '"
                            + ADDRESS_TYPE_REGEX + "'");
        } catch (DataAccessLayerException e) {
            throw new EbxmlRegistryException("Data Access Error", e);
        }
    }

    /**
     * Gets the codes of the email types in the registry
     * 
     * @return The codes dmail types in the registry
     * @throws EbxmlRegistryException
     *             If errors occur during interaction with the database
     */
    public List<String> getEmailTypes() throws EbxmlRegistryException {
        try {
            return this
                    .executeHQLQuery("select obj.code from ClassificationNodeType obj where obj.lid like '"
                            + EMAIL_TYPE_REGEX + "'");
        } catch (DataAccessLayerException e) {
            throw new EbxmlRegistryException("Data Access Error", e);
        }
    }

    @Override
    protected Class<ClassificationNodeType> getEntityClass() {
        return ClassificationNodeType.class;
    }
}
