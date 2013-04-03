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
package com.raytheon.uf.edex.registry.ebxml.services.validator.types;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationSchemeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidationExceptionType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.constants.ErrorSeverity;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.ValidatorTypeManager;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * Validator implementation for validation of ClassificationType objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/13/12      184        bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class ClassificationTypeValidator implements IRegistryValidator {

    /** The logger */
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ClassificationTypeValidator.class);;

    @Override
    public void validate(Object object, ValidateObjectsResponse response) {
        ValidatorTypeManager.getInstance().getValidator(
                RegistryObjectType.class);
        ClassificationType classType = (ClassificationType) object;

        if (classType.getClassifiedObject() == null) {
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    ValidationExceptionType.class,
                                    "",
                                    "Validation Failed: classifiedObject attribute missing in classification definition",
                                    "For both internal and external classifications, the classifiedObject "
                                            + "attribute is required and it references the RegistryObjectType instance "
                                            + "that is classified by this Classification",
                                    ErrorSeverity.ERROR, statusHandler));
        }

        String nodeName = classType.getClassificationNode();
        String schemeName = classType.getClassificationScheme();
        if (nodeName == null && schemeName == null) {
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    ValidationExceptionType.class,
                                    "",
                                    "Validation Failed: classificationNode and classificationScheme attributes missing in classification definition",
                                    "If the ClassificationType instance represents an external classification,"
                                            + "then the classificationScheme attribute is required.If the ClassificationType instance represents an internal classification, then "
                                            + "the classificationNode attribute is required.",
                                    ErrorSeverity.ERROR, statusHandler));
        }
        try {
            if (nodeName != null && schemeName == null) {
                ClassificationNodeType node = getNode(nodeName);
                if (node == null) {
                    // response.getException()
                    // .add(EbxmlExceptionUtil
                    // .createRegistryException(
                    // ValidationExceptionType.class,
                    // "",
                    // "Validation Failed: Classification Node ["
                    // + nodeName
                    // + "] does not exist",
                    // "The classificationNode value MUST reference a ClassificationNodeType instance",
                    // ErrorSeverity.ERROR));
                }
            } else if (nodeName == null && schemeName != null) {
                ClassificationSchemeType scheme = getScheme(schemeName);
                if (scheme == null) {
                    response.getException()
                            .add(EbxmlExceptionUtil
                                    .createRegistryException(
                                            ValidationExceptionType.class,
                                            "",
                                            "Validation Failed: Classification Scheme ["
                                                    + schemeName
                                                    + "] does not exist",
                                            "The classificationScheme value MUST reference a ClassificationSchemeType instance",
                                            ErrorSeverity.ERROR, statusHandler));
                }
            } else {
                ClassificationNodeType node = getNode(nodeName);
                ClassificationSchemeType scheme = getScheme(schemeName);
                if (node == null && scheme == null) {
                    response.getException()
                            .add(EbxmlExceptionUtil
                                    .createRegistryException(
                                            ValidationExceptionType.class,
                                            "",
                                            "Validation Failed: Classification Node ["
                                                    + nodeName
                                                    + "] does not exist.Classification Scheme [\"\n"
                                                    + "                                                    + schemeName\n"
                                                    + "                                                    + \"] does not exist\"",
                                            "The classificationNode value MUST reference a ClassificationNodeType instance. The classificationScheme value MUST reference a ClassificationSchemeType instance",
                                            ErrorSeverity.ERROR, statusHandler));
                }

            }
        } catch (EbxmlRegistryException e) {
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    ValidationExceptionType.class,
                                    "",
                                    "Validation Failed: Error verifying classification node and scheme",
                                    "There was an unexpected error encountered while verifying if a classification node or scheme exists",
                                    ErrorSeverity.ERROR, e, statusHandler));
        }
    }

    private ClassificationNodeType getNode(String classificationNode)
            throws EbxmlRegistryException {
        if (classificationNode == null) {
            return null;
        } else {
            return new RegistryObjectTypeDao(ClassificationNodeType.class)
                    .getById(classificationNode);
        }
    }

    private ClassificationSchemeType getScheme(String classificationScheme)
            throws EbxmlRegistryException {
        if (classificationScheme == null) {
            return null;
        } else {
            return new RegistryObjectTypeDao(ClassificationSchemeType.class)
                    .getById(classificationScheme);
        }
    }
}
