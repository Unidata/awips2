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
package com.raytheon.uf.edex.registry.acp.xacml.conformance;

/**
 * 
 * XACML schema elements as defined in table 10.2.1 of the XACML 2.0 core spec
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
public class SchemaElements {
    public static final String CONTEXT_ACTION = "xacml-context:Action";

    public static final String CONTEXT_ATTRIBUTE = "xacml-context:Attribute";

    public static final String CONTEXT_ATTRIBUTEVALUE = "xacml-context:AttributeValue";

    public static final String CONTEXT_DECISION = "xacml-context:Decision";

    public static final String CONTEXT_ENVIRONMENT = "xacml-context:Environment";

    public static final String CONTEXT_MISSINGATTRIBUTEDETAIL = "xacml-context:MissingAttributeDetail";

    public static final String CONTEXT_OBLIGATIONS = "xacml-context:Obligations";

    public static final String CONTEXT_REQUEST = "xacml-context:Request";

    public static final String CONTEXT_RESOURCE = "xacml-context:Resource";

    public static final String CONTEXT_RESOURCECONTENT = "xacml-context:ResourceContent";

    public static final String CONTEXT_RESPONSE = "xacml-context:Response";

    public static final String CONTEXT_RESULT = "xacml-context:Result";

    public static final String CONTEXT_STATUS = "xacml-context:Status";

    public static final String CONTEXT_STATUSCODE = "xacml-context:StatusCode";

    public static final String CONTEXT_STATUSDETAIL = "xacml-context:StatusDetail";

    public static final String CONTEXT_STATUSMESSAGE = "xacml-context:StatusMessage";

    public static final String CONTEXT_SUBJECT = "xacml-context:Subject";

    public static final String ACTION = "xacml:Action";

    public static final String ACTION_ATTRIBUTE_DESIGNATOR = "xacml:ActionAttributeDesignator";

    public static final String ACTION_MATCH = "xacml:ActionMatch";

    public static final String ACTIONS = "xacml:Actions";

    public static final String APPLY = "xacml:Apply";

    public static final String ATTRIBUTE_ASSIGNMENT = "xacml:AttributeAssignment";

    public static final String ATTRIBUTE_SELECTOR = "xacml:AttributeSelector";

    public static final String ATTRIBUTE_VALUE = "xacml:AttributeValue";

    public static final String COMBINER_PARAMETERS = "xacml:CombinerParameters";

    public static final String COMBINER_PARAMETER = "xacml:CombinerParameter";

    public static final String CONDITION = "xacml:Condition";

    public static final String DESCRIPTION = "xacml:Description";

    public static final String ENVIRONMENT = "xacml:Environment";

    public static final String ENVIRONMENT_MATCH = "xacml:Environment";

    public static final String ENVIRONMENT_ATTRIBUTE_DESIGNATOR = "xacml:EnvironmentAttributeDesignator";

    public static final String ENVIRONMENTS = "xacml:Environments";

    public static final String EXPRESSION = "xacml:Expression";

    public static final String FUNCTION = "xacml:Function";

    public static final String OBLIGATION = "xacml:Obligation";

    public static final String OBLIGATIONS = "xacml:Obligations";

    public static final String POLICY = "xacml:Policy";

    public static final String POLICY_COMBINER_PARAMETERS = "xacml:PolicyCombinerParameters";

    public static final String POLICY_DEFAULTS = "xacml:PolicyDefaults";

    public static final String POLICY_ID_REFERENCE = "xacml:PolicyIdReference";

    public static final String POLICY_SET = "xacml:PolicySet";

    public static final String POLICY_SET_DEFAULTS = "xacml:PolicySetDefaults";

    public static final String POLICY_SET_ID_REFERENCE = "xacml:PolicySetIdReference";

    public static final String RESOURCE = "xacml:Resource";

    public static final String RESOURCE_ATTRIBUTE_DESIGNATOR = "xacml:ResourceAttributeDesignator";

    public static final String RESOURCE_MATCH = "xacml:ResourceMatch";

    public static final String RESOURCES = "xacml:Resources";

    public static final String RULE = "xacml:Rule";

    public static final String RULE_COMBINER_PARAMETERS = "xacml:RuleCombinerParameters";

    public static final String SUBJECT = "xacml:Subject";

    public static final String SUBJECT_MATCH = "xacml:SubjectMatch";

    public static final String SUBJECTS = "xacml:Subjects";

    public static final String TARGET = "xacml:Target";

    public static final String VARIABLE_DEFINITION = "xacml:VariableDefinition";

    public static final String VARIABLE_REFERENCE = "xacml:VariableReference";

    public static final String XPATH_VERSION = "xacml:XPathVersion";
}
