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

import java.util.Iterator;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.datadelivery.system.Operator;
import com.raytheon.uf.viz.datadelivery.system.OpsNetFieldNames;
import com.raytheon.uf.viz.datadelivery.utils.DataSizeUnit;

/**
 * Parent Rules xml class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 19, 2012   1420     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@SuppressWarnings({ "unchecked", "rawtypes" })
@XmlAccessorType(XmlAccessType.NONE)
public abstract class RuleXML {

    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RuleXML.class);

    /** Rule name */
    @XmlElement
    protected String ruleName;

    /** Rule field */
    @XmlElement
    protected String ruleField;

    /** Rule operator */
    @XmlElement
    protected String ruleOperator;

    /** Rule value */
    @XmlElement
    protected String ruleValue;

    /** Rule unit */
    @XmlElement
    protected String ruleUnit;

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
     * Check to see if a subscription to a data set with the given cycles
     * matches this rule.
     * 
     * @param sub
     *            The subscription
     * @param cycles
     *            The available cycles
     * @return true if the subscription matches this rule
     */

    public boolean matches(Subscription sub, Set<Integer> cycles) {
        String unit = null;

        if (OpsNetFieldNames.SIZE.toString().equals(ruleField)
                || OpsNetFieldNames.FREQUENCY.toString().equals(ruleField)) {
            unit = getRuleUnit();
        }

        OperatorAdapter oa = new OperatorAdapter();
        Operator oper = null;
        try {
            oper = oa.unmarshal(ruleOperator);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        }

        // If Data Name
        if (OpsNetFieldNames.NAME.toString().equals(ruleField)) {

            String dsName = sub.getDataSetName();

            return oper.evaluate(dsName, ruleValue);
        }

        // If Data Type
        if (OpsNetFieldNames.TYPE.toString().equals(ruleField)) {
            String dsType = sub.getDataSetType().toString();

            return oper.evaluate(ruleValue, dsType);
        }

        // If Data Size
        if (OpsNetFieldNames.SIZE.toString().equals(ruleField)) {
            long dsSizeKb = sub.getDataSetSize(); // Size in KB
            long ruleValueInt = Integer.parseInt(ruleValue);
            DataSizeUnit dsUnit = DataSizeUnit.valueOf(unit);
            ruleValueInt = dsUnit.toKB(ruleValueInt);

            return oper.evaluate(Long.valueOf(dsSizeKb),
                    Long.valueOf(ruleValueInt));
        }

        // If Data Frequency
        if (OpsNetFieldNames.FREQUENCY.toString().equals(ruleField)) {
            // Calculate frequency
            int ruleValueInt = Integer.parseInt(this.ruleValue);
            if (unit.equalsIgnoreCase("Mins")) {
                ruleValueInt /= 60;
            }
            int freq = 0;
            Iterator<Integer> iter = cycles.iterator();
            int tmp = 0;
            if (iter.hasNext()) {
                tmp = iter.next();
            }

            if (iter.hasNext()) {
                int val = iter.next();
                freq = val - tmp;
            }

            if (oper.evaluate(Long.valueOf(freq), Long.valueOf(ruleValueInt))) {
                return true;
            }
        }

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((ruleName == null) ? 0 : ruleName.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        RuleXML other = (RuleXML) obj;
        if (ruleName == null) {
            if (other.ruleName != null) {
                return false;
            }
        } else if (!ruleName.equals(other.ruleName)) {
            return false;
        }
        return true;
    }
}
