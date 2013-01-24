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

package com.raytheon.uf.edex.database.purge;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A container class used for unmarshalling purge rules. The purge rules are
 * stored in xml files in the common_static/base/purge directory. The rules are
 * then organized into a tree for easy look up of closest matching rule.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/15/11      #2469       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "purgeRuleSet")
@XmlAccessorType(XmlAccessType.NONE)
public class PurgeRuleSet implements ISerializableObject {

    @XmlElements({ @XmlElement(name = "key", type = String.class) })
    private List<String> keys;

    @XmlElements({ @XmlElement(name = "defaultRule", type = PurgeRule.class) })
    private List<PurgeRule> defaultRules;

    /**
     * List of purge rules for/from the XML.
     */
    @XmlElements({ @XmlElement(name = "rule", type = PurgeRule.class) })
    private List<PurgeRule> rules;

    private PurgeRuleTree purgeTree = null;

    public PurgeRuleSet() {
    }

    /**
     * Returns the default rule.
     * 
     * @return
     */
    public List<PurgeRule> getDefaultRules() {
        return defaultRules;
    }

    public void setDefaultRules(final List<PurgeRule> defaultRules) {
        this.defaultRules = defaultRules;
    }

    /**
     * Sets the default rule list to the passed rule.
     * 
     * @param defaultRule
     */
    public void setDefaultRule(final PurgeRule defaultRule) {
        this.defaultRules = new ArrayList<PurgeRule>(1);
        this.defaultRules.add(defaultRule);
    }

    /**
     * Returns the list of purge rules.
     * 
     * @return
     */
    public List<PurgeRule> getRules() {
        return (rules == null) ? Collections.<PurgeRule> emptyList() : rules;
    }

    public void setRules(final ArrayList<PurgeRule> rules) {
        this.rules = rules;
    }

    /**
     * Returns the list of purge keys.
     * 
     * @return
     */
    public List<String> getKeys() {
        return keys;
    }

    public void setKeys(final List<String> keys) {
        this.keys = keys;
    }

    /**
     * Returns the purge rules associated with the passed key values.
     * 
     * @param keyValues
     *            The values associated with the plugin purge keys to check for
     *            purge rules for.
     * @return
     */
    public List<PurgeRule> getRuleForKeys(final String[] keyValues) {
        if (purgeTree == null) {
            purgeTree = new PurgeRuleTree(this);
        }

        return purgeTree.getRulesForKeys(keyValues);
    }
}
