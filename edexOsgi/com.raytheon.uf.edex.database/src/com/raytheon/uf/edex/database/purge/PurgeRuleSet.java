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
import java.util.Collection;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A container class used for unmarshalling purge rules. The purge rules are
 * stored in xml files in the edex_static/base/purge directory. The rules are
 * unmarshalled into this object before being persisted to the database.
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
    /**
     * List of purge rules for/from the XML.
     */
    @XmlElements({ @XmlElement(name = "rule", type = PurgeRule.class) })
    private ArrayList<PurgeRule> rules;

    public PurgeRuleSet() {
        rules = new ArrayList<PurgeRule>();
    }

    /**
     * Returns the list of purge rules.
     * 
     * @return
     */
    public ArrayList<PurgeRule> getRules() {
        return rules;
    }

    /**
     * Set the list of purge rules.
     * 
     * @param models
     */
    public void setModels(ArrayList<PurgeRule> rules) {
        this.rules = rules;
    }

    /**
     * Add a purge rule to this set
     * 
     * @param rule
     *            The rule to add
     */
    public void addRule(PurgeRule rule) {
        this.rules.add(rule);
    }

    /**
     * Adds purge rules to this set
     * 
     * @param rules
     *            The rules to add
     */
    public void addRules(Collection<PurgeRule> rules) {
        this.rules.addAll(rules);
    }
}
