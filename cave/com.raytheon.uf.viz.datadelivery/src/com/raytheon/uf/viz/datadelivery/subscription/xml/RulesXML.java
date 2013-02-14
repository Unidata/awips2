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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.google.common.base.Function;
import com.google.common.collect.Lists;

/**
 * RulesXML base class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 7, 2013    1420     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class RulesXML<T extends RuleXML> {
    private static final Function<RuleXML, String> GET_RULE_NAMES_FUNCTION = new Function<RuleXML, String>() {
        @Override
        public String apply(RuleXML rule) {
            return rule.getRuleName();
        }
    };

    @XmlElements({ @XmlElement(name = "rule") })
    protected List<T> rules = new ArrayList<T>();

    /**
     * Add a rule.
     * 
     * @param ruleXml
     *            The rule to add
     * @return true if added, false if rule name already exists
     */
    public boolean addRule(T ruleXml) {
        if (rules.contains(ruleXml)) {
            return false;
        }

        this.rules.add(ruleXml);
        return true;
    }

    /**
     * @return the rules
     */
    public List<T> getRules() {
        return rules;
    }

    /**
     * Get the list of rule names.
     * 
     * @return List of rule names
     */
    public List<String> getRuleNames() {
        return new ArrayList<String>(Lists.transform(getRules(),
                GET_RULE_NAMES_FUNCTION));
    }

    /**
     * @param rules
     *            the rules to set
     */
    public void setRules(List<T> rules) {
        this.rules = rules;
    }

    /**
     * Remove a rule.
     * 
     * @param ruleName
     *            The rule to remove
     */
    public void removeRuleByName(String ruleName) {
        for (T rule : rules) {
            if (rule.getRuleName().equals(ruleName)) {
                this.rules.remove(rule);
                return;
            }
        }
    }

    public boolean updateRule(T updatedRule) {
        for (int i = 0; i < rules.size(); i++) {
            T rule = rules.get(i);
            if (rule.equals(updatedRule)) {
                rules.remove(i);
                rules.add(i, updatedRule);
                return true;
            }
        }

        return false;
    }
}
