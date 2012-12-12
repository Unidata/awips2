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
 */
package com.raytheon.uf.edex.database.purge;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Tree representation of the purge rules. Each Node can contain a List of
 * PurgeRule as well as a collection of other Nodes. Each Node should be a
 * specific purge key value based on the PurgeRuleSet keys. A given set of
 * key/value pairs will return the most significant purge key that matches.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2012            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class PurgeRuleTree {
    private final PurgeNode root;

    public PurgeRuleTree(PurgeRuleSet ruleSet) {
        root = new PurgeNode();
        root.setRules(ruleSet.getDefaultRules());
        List<PurgeRule> rules = ruleSet.getRules();
        if (rules != null) {
            for (PurgeRule rule : rules) {
                PurgeNode curNode = root;
                List<String> values = rule.getKeyValues();
                if ((values != null) && !values.isEmpty()) {
                    // descend purge tree
                    for (String val : values) {
                        Map<String, PurgeNode> childNodes = curNode
                                .getChildNodes();
                        curNode = childNodes.get(val);
                        if (curNode == null) {
                            curNode = new PurgeNode();
                            childNodes.put(val, curNode);
                        }
                    }

                    // set the rule on the leaf node defined by key values
                    curNode.addRule(rule);
                }
            }
        }
    }

    /**
     * Returns the purge rules associated with the given key value list.
     * 
     * @param keyValues
     * @return
     */
    public List<PurgeRule> getRulesForKeys(String[] keyValues) {
        // default rule is initial closest rule
        List<PurgeRule> closestRules = root.getRules();
        PurgeNode currentNode = root;

        if ((keyValues != null) && (keyValues.length > 0)) {
            // iterate over key values, descending tree as far as possible,
            // keeping track of closest matching rule.
            for (String value : keyValues) {
                currentNode = currentNode.getChildNode(value);

                // descend node
                if (currentNode != null) {
                    // check node for rules
                    List<PurgeRule> rules = currentNode.getRules();

                    if ((rules != null) && !rules.isEmpty()) {
                        // current closest rules
                        closestRules = rules;
                    }
                } else {
                    break;
                }
            }
        }

        return closestRules;
    }

    private class PurgeNode {
        // most nodes only have 1 rule
        private List<PurgeRule> rules = null;

        private final Map<String, PurgeNode> childNodes = new HashMap<String, PurgeNode>();

        public void addRule(PurgeRule rule) {
            if (rules == null) {
                rules = new ArrayList<PurgeRule>(1);
            }

            rules.add(rule);
        }

        public void setRules(List<PurgeRule> rules) {
            this.rules = rules;
        }

        public List<PurgeRule> getRules() {
            return rules;
        }

        public Map<String, PurgeNode> getChildNodes() {
            return childNodes;
        }

        public PurgeNode getChildNode(String keyValue) {
            return childNodes.get(keyValue);
        }
    }
}
