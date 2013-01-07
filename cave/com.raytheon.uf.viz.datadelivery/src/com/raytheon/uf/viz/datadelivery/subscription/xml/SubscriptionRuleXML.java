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
package com.raytheon.uf.viz.datadelivery.subscription.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Subscription Rules XML Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2012    730     jpiatt      Initial creation
 * Oct 30, 2012 1286       djohnson    Latency is an integer, remove unnecessary jaxb stuff.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */

@XmlRootElement(name = "SubscriptionRule")
@XmlAccessorType(XmlAccessType.PROPERTY)
public class SubscriptionRuleXML implements ISerializableObject {

    private String ruleName;

    private String ruleField;

    private String ruleOperator;

    private String ruleValue;

    private String ruleUnit;

    private Integer rulePriority;
    
    private Integer ruleLatency;

    /**
     * Get the name of the rule.
     * 
     * @return the ruleName
     */
    public String getRuleName() {
        return ruleName;
    }

    /**
     * Set the field name of the rule.
     * 
     * @param ruleName
     *            The name of the rule
     */
    public void setRuleName(String ruleName) {
        this.ruleName = ruleName;
    }
    
    /**
     * Get the field name of the rule.
     * 
     * @return the ruleField
     */
    public String getRuleField() {
        return ruleField;
    }
    
    /**
     * Set the rule field name.
     * 
     * @param ruleField
     *            The field value of the rule
     */
    public void setRuleField(String ruleField) {
        this.ruleField = ruleField;
    }

    /**
     * Get the operator of the rule.
     * 
     * @return the ruleOperator
     */
    public String getRuleOperator() {
        return ruleOperator;
    }

    /**
     * Set the rule rule operator.
     * 
     * @param ruleOperator
     *            The operator value of the rule
     */
    public void setRuleOperator(String ruleOperator) {
        this.ruleOperator = ruleOperator;
    }

    /**
     * Get the value of the rule.
     * 
     * @return the ruleValue
     */
    public String getRuleValue() {
        return ruleValue;
    }

    /**
     * Set the rule value.
     * 
     * @param ruleValue
     *            The value of the rule
     */
    public void setRuleValue(String ruleValue) {
        this.ruleValue = ruleValue;
    }

    /**
     * Get the unit of the value of the rule.
     * 
     * @return the ruleUnit
     */
    public String getRuleUnit() {
        return ruleUnit;
    }

    /**
     * Set the rule unit.
     * 
     * @param ruleUnit
     *            The unit value of the rule
     */
    public void setRuleUnit(String ruleUnit) {
        this.ruleUnit = ruleUnit;
    }

    /**
     * Get the priority of the rule.
     * 
     * @return the rulePriority
     */
    public Integer getRulePriority() {
        return rulePriority;
    }

    /**
     * Set the rule priority.
     * 
     * @param rulePriority
     *            The priority value of the rule
     */
    public void setRulePriority(Integer rulePriority) {
        this.rulePriority = rulePriority;
    }

    /**
     * Get the latency of the rule.
     * 
     * @return the ruleLatency
     */
    public Integer getRuleLatency() {
        return ruleLatency;
    }

    /**
     * Set the rule latency.
     * 
     * @param ruleLatency
     *            The latency value in min or hours of the rule
     */
    public void setRuleLatency(Integer ruleLatency) {
        this.ruleLatency = ruleLatency;
    }

}

