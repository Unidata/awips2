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
package com.raytheon.uf.edex.registry.acp.xacml.engine.function.impl;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RoleType;

import org.opensaml.xacml.ctx.AttributeType;
import org.opensaml.xacml.ctx.AttributeValueType;
import org.opensaml.xacml.ctx.SubjectType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.acp.xacml.conformance.Identifiers;
import com.raytheon.uf.edex.registry.acp.xacml.engine.function.XACMLFunction;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;
import com.raytheon.uf.edex.registry.ebxml.dao.RoleDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

/**
 * 
 * Implementation of the matches-role function
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/20/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class MatchesRole extends XACMLFunction {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MatchesRole.class);

    @Override
    protected String getFunctionId() {
        return "urn:oasis:names:tc:ebxml-regrep:4.0:rim:acp:function: matches-role";
    }

    /**
     * <table border="1">
     * <tr>
     * <th>Parameter/Return</th>
     * <th>Name</th>
     * <th>Description</th>
     * <th>Data Type</th>
     * </tr>
     * <tr>
     * <td>Parameter 1</td>
     * <td>roles</td>
     * <td>Specifies a bag containing ids of RoleType instances representing the
     * contextual roles that a subject is expected to have</td>
     * <td>Bag of attributes of type http://www.w3.org/2001/XMLSchema#string</td>
     * </tr>
     * <tr>
     * <td>Parameter 2</td>
     * <td>roleType</td>
     * <td>Specifies the id of a ClassificationNode within the canonical
     * SubjectRole ClassificationScheme</td>
     * <td>http://www.w3.org/2001/XMLSchema#string</td>
     * </tr>
     * <tr>
     * <td>Parameter 3+N</td>
     * <td>contextKey</td>
     * <td>Specifies a context identifier</td>
     * <td>http://www.w3.org/2001/XMLSchema#string</td>
     * </tr>
     * <tr>
     * <td>Parameter 4+N</td>
     * <td>contextValue</td>
     * <td>Specifies a context value associated with the context identifier
     * specified by previous parameter</td>
     * <td>http://www.w3.org/2001/XMLSchema#string</td>
     * </tr>
     * </table>
     * 
     * @param roles
     *            See method description
     * @param roleType
     *            See method description
     * @param contextKey
     *            See method description
     * @return MUST return "True" if and only if at least one RoleType instance
     *         assigned to the subject meets the following conditions:<br>
     *         - If roleType is specified, then the type attribute of RoleType
     *         instance MUST match the role type ClassificationNode (or a
     *         descendant of if) specified by the roleType parameter<br>
     *         - If any context key/value pairs are specified then the RoleType
     *         instance MUST have a Slot whose name matches the context key and
     *         whose value matches the context value<br>
     *         - MUST return "false" otherwise
     * 
     */
    public Boolean executeFunction(List<String> roles, String roleType,
            String... contextKey) throws XACMLProcessingException {
        statusHandler.info("Evaluating matches-role function.");

        RoleDao roleDao = new RoleDao();
        try {
            // Get the subject ID from the request object
            String subjectId = getSubjectId();
            if (subjectId == null) {
                throw new XACMLProcessingException(
                        "Subject ID not found in request!");
            }
            // Gets the current role assigned to the user
            RoleType userRole = roleDao.getUserRole(subjectId);
            if (userRole == null) {
                throw new XACMLProcessingException("User [" + subjectId
                        + "] currently doesn not have a role assigned");
            }
            String roleName = userRole.getName().getLocalizedString().get(0)
                    .getValue();
            statusHandler.info("Role for user [" + subjectId + "] is ["
                    + userRole + "]");
            for (String role : roles) {
                if (roleName != null) {
                    if (roleName.equals(role)) {
                        statusHandler.info("Role for user matches " + roleName);
                        return true;
                    }
                }
            }
        } catch (EbxmlRegistryException e) {
            throw new XACMLProcessingException("Error getting role for user", e);
        }
        return false;
    }

    /**
     * Gets the subject ID from the current request. This method looks for the
     * urn:oasis:names:tc:xacml:1.0:subject:subject-id identifier. If found, the
     * value of the attribute is retrieved as the subject ID. If it is not
     * found, null is returned
     * 
     * @return The subject ID or null if not found in the request
     */
    private String getSubjectId() {
        statusHandler.info("Getting the subject ID from the request");
        String subjectId = null;
        List<SubjectType> subjectsList = request.getSubjects();
        /*
         * Iterate through the subjects listed in the request and look for the
         * subject ID identifier string
         */
        for (SubjectType subject : subjectsList) {
            List<AttributeType> attrs = subject.getAttributes();
            for (AttributeType attr : attrs) {
                String attributeId = attr.getAttributeID();
                if (attributeId.equals(Identifiers.SUBJECT_ID)) {
                    List<AttributeValueType> attrValues = attr
                            .getAttributeValues();
                    for (AttributeValueType attrValue : attrValues) {
                        subjectId = attrValue.getValue();
                        statusHandler.info("Subject ID determined to be "
                                + subjectId);
                    }
                }
            }
        }
        return subjectId;

    }
}
