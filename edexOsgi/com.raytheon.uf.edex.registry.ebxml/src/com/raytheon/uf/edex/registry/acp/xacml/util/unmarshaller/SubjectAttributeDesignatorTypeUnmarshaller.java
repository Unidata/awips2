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
package com.raytheon.uf.edex.registry.acp.xacml.util.unmarshaller;

import org.opensaml.xacml.policy.SubjectAttributeDesignatorType;
import org.opensaml.xacml.policy.impl.AttributeDesignatorTypeUnmarshaller;
import org.opensaml.xml.XMLObject;
import org.opensaml.xml.io.UnmarshallingException;
import org.opensaml.xml.util.DatatypeHelper;
import org.w3c.dom.Attr;

/**
 * 
 * Unmarshaller implementation implemented as a workaround for a bug in the
 * opensaml 2.5.1 jar
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class SubjectAttributeDesignatorTypeUnmarshaller extends
        AttributeDesignatorTypeUnmarshaller {

    /** Constructor. */
    public SubjectAttributeDesignatorTypeUnmarshaller() {
        super();
    }

    /** {@inheritDoc} */
    protected void processAttribute(XMLObject xmlObject, Attr attribute)
            throws UnmarshallingException {

        if (attribute.getLocalName().equals(
                SubjectAttributeDesignatorType.SUBJECT_CATEGORY_ATTRIB_NAME)) {
            SubjectAttributeDesignatorType subjectAttributeDesignatorType = (SubjectAttributeDesignatorType) xmlObject;
            subjectAttributeDesignatorType.setSubjectCategory(DatatypeHelper
                    .safeTrimOrNullString(attribute.getValue()));
        } else {
            super.processAttribute(xmlObject, attribute);
        }
    }
}
