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
package com.raytheon.uf.edex.registry.acp.xacml.engine.expression.impl;

import org.opensaml.xacml.ctx.RequestType;
import org.opensaml.xacml.policy.ExpressionType;
import org.opensaml.xacml.policy.SubjectAttributeDesignatorType;
import org.opensaml.xacml.policy.impl.SubjectAttributeDesignatorTypeImpl;

import com.raytheon.uf.edex.registry.acp.xacml.conformance.Identifiers;
import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLException;
import com.raytheon.uf.edex.registry.acp.xacml.util.XACMLObjectUtil;

/**
 * The <SubjectAttributeDesignator> element retrieves a bag of values for a
 * named categorized subject attribute from the request context. A subject
 * attribute is an attribute contained within a <Subject> element of the request
 * context. A categorized subject is a subject that is identified by a
 * particular subject-category attribute. A named categorized subject attribute
 * is a named subject attribute for a particular categorized subject.
 * <p>
 * The <SubjectAttributeDesignator> element SHALL return a bag containing all
 * the subject attribute values that are matched by the named categorized
 * subject attribute. In the event that no matching attribute is present in the
 * context, the MustBePresent attribute governs whether this element returns an
 * empty bag or “Indeterminate”. See Section 7.2.5.
 * <p>
 * The SubjectAttributeDesignatorType extends the match semantics of the
 * AttributeDesignatorType (See Section 5.37) such that it narrows the attribute
 * search space to the specific categorized subject such that the value of this
 * element’s SubjectCategory attribute matches, by URI equality, the value of
 * the request context’s <Subject> element’s SubjectCategory attribute.
 * <p>
 * If the request context contains multiple subjects with the same
 * SubjectCategory XML attribute, then they SHALL be treated as if they were one
 * categorized subject.
 * <p>
 * The <SubjectAttributeDesignator> MAY appear in the <SubjectMatch> element and
 * MAY be passed to the <Apply> element as an argument.
 * <p>
 * The <SubjectAttributeDesignator> element is of type
 * SubjectAttributeDesignatorType. The SubjectAttributeDesignatorType complex
 * type extends the AttributeDesignatorType complex type with a SubjectCategory
 * attribute.
 * <p>
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
public class SubjectAttributeDesignatorExpression extends
        AttributeDesignatorExpression {

    @Override
    public String getExpressionId() throws Exception {
        return SubjectAttributeDesignatorTypeImpl.class.getName();
    }

    @Override
    public Object evaluate(ExpressionType expression, RequestType request)
            throws XACMLException {

        SubjectAttributeDesignatorType designator = (SubjectAttributeDesignatorType) XACMLObjectUtil
                .validateElement(
                        expression,
                        SubjectAttributeDesignatorType.DEFAULT_ELEMENT_LOCAL_NAME);

        String designatorAttributeId = XACMLObjectUtil.validateElement(
                designator.getAttributeId(), "AttributeId");
        String designatorDataType = XACMLObjectUtil.validateElement(
                designator.getDataType(), "DataType");
        String subjectIssuer = designator.getIssuer();
        String subjectCategory = designator.getSubjectCategory();
        boolean designatorMustBePresentXS = designator
                .getMustBePresentXSBoolean().getValue();
        Boolean designatorMustBePresent = designator.getMustBePresent();

        if (subjectCategory == null) {
            subjectCategory = Identifiers.ACCESS_SUBJECT;
        }

        return getAttributeFromSubject(request, designatorAttributeId,
                designatorDataType, subjectCategory, subjectIssuer,
                designatorMustBePresent);

    }
}
