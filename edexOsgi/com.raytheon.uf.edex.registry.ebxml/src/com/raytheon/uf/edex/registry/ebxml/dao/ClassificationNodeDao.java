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

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

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
 * 4/9/2013     1802       bphillip    Removed exception catching
 * 2/13/2014    2769       bphillip    Added read only flags to query methods
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class ClassificationNodeDao extends
        RegistryObjectTypeDao<ClassificationNodeType> {

    private static final String GET_BY_PATH_QUERY = "select obj from ClassificationNodeType obj where obj.path=:path";

    private static final String GET_NODE_FROM_CODE_QUERY = "select obj.id from ClassificationNodeType obj where obj.code=:code";

    private static final String GET_CODE_FROM_NODE_QUERY = "select obj.code from ClassificationNodeType obj where obj.id=:id";

    private static final String GET_TELEPHONE_TYPES_QUERY = "select obj.code from ClassificationNodeType obj where obj.lid like 'urn:oasis:names:tc:ebxml-regrep:PhoneType:%'";

    private static final String GET_ADDRESS_TYPES_QUERY = "select obj.code from ClassificationNodeType obj where obj.lid like 'urn:oasis:names:tc:ebxml-regrep:PostalAddressType%'";

    private static final String GET_EMAIL_TYPES_QUERY = "select obj.code from ClassificationNodeType obj where obj.lid like 'urn:oasis:names:tc:ebxml-regrep:EmailType:%'";

    public ClassificationNodeDao() {

    }

    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public boolean isValidNode(String id) {
        return getById(id) != null;
    }

    /**
     * Retrieves ClassificationNode objects based on the path
     * 
     * @param path
     *            The path to get the classification node type for
     * @return The ClassificationNode object with the specified path
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public ClassificationNodeType getByPath(String path) {
        List<ClassificationNodeType> result = this.executeHQLQuery(
                GET_BY_PATH_QUERY, "path", path);
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
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public String getNodeFromCode(String code) {
        List<String> results = this.executeHQLQuery(GET_NODE_FROM_CODE_QUERY,
                "code", code);

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
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public String getCodeFromNode(String id) {
        List<String> results = this.executeHQLQuery(GET_CODE_FROM_NODE_QUERY,
                "id", id);

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
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<String> getTelephoneTypes() {

        return this.executeHQLQuery(GET_TELEPHONE_TYPES_QUERY);

    }

    /**
     * Gets the codes of the address types in the registry
     * 
     * @return The codes of the address types in the registry
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<String> getAddressTypes() {

        return this.executeHQLQuery(GET_ADDRESS_TYPES_QUERY);

    }

    /**
     * Gets the codes of the email types in the registry
     * 
     * @return The codes dmail types in the registry
     */
    @Transactional(propagation = Propagation.REQUIRED, readOnly = true)
    public List<String> getEmailTypes() {
        return this.executeHQLQuery(GET_EMAIL_TYPES_QUERY);
    }

    @Override
    protected Class<ClassificationNodeType> getEntityClass() {
        return ClassificationNodeType.class;
    }
}
