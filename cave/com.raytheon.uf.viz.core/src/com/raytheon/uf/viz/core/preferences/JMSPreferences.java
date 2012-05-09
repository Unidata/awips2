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
package com.raytheon.uf.viz.core.preferences;

import java.util.UUID;

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.uf.viz.core.Activator;

/**
 * Serialized class for the JMS policy type
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class JMSPreferences {

    // these are the policy types as supported by QPID
    //
    // reject - reject the message if the topic is full
    //
    // ring - start overwriting messages in a ring based on sizing. If head
    // meets tail, advance head.
    //
    // flow to disk - flow the messages to disk to preserve memory
    //
    // ring strict - start overwriting messages in a ring based on sizing. If
    // head meets tail, AND the consumer has the tail message acquired it will
    // reject
    //
    public enum PolicyType {
        REJECT("reject"), RING("ring"), FLOW_TO_DISK("flow_to_disk"), RING_STRICT(
                "ring_strict");

        private String type;

        /**
         * @return the type
         */
        public String getType() {
            return type;
        }

        private PolicyType(String name) {
            type = name;
        }

    }

    // declare topic as ADDR, not BURL
    private static final String addressArgs = "ADDR:'amq.topic'/";

    private static final String declareArgs = "; { node: { type: 'topic', x-declare: {arguments: {";

    private static final String linkArgs = "}}}, link: { name: '";

    private static final String linkDeclareArgs = "', x-declare: {arguments: {";

    private static final String QPID_MAX_COUNT = "qpid.max_count";

    private static final String QPID_POLICY_TYPE = "qpid.policy_type";

    private static String policyType = "";

    private static int maxVal = 0;

    // putting together the arguments for the qpid queue
    public static String getPolicyString(String val) {
        // for qpid.policy_type, there is a corresponding constant in
        // QpidQueueOptions, but did not want to add a dependency

        return addressArgs + val + declareArgs + "'" + QPID_MAX_COUNT + "':"
                + getMaxCount() + ",'" + QPID_POLICY_TYPE + "':"
                + getPolicyType() + linkArgs + val + UUID.randomUUID()
                + linkDeclareArgs + "'" + QPID_MAX_COUNT + "':" + getMaxCount()
                + ",'" + QPID_POLICY_TYPE + "':" + getPolicyType() + "}}}}";
    }

    /**
     * @return the policyTypeOption
     */
    public static String getPolicyType() {
        if (policyType != null && !policyType.isEmpty()) {
            return policyType;
        }
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        String value = store.getString(QPID_POLICY_TYPE);
        // validation
        for (PolicyType type : PolicyType.values()) {
            if (!type.getType().equals(value)) {
                policyType = value;
            }
        }
        // fail safe for bad configurations
        if (policyType == null || policyType.isEmpty()) {
            policyType = PolicyType.RING.getType();
        }
        return "'" + policyType + "'";
    }

    /**
     * Get the max count as given in the config.xml in the preference store
     * 
     * @return
     */
    public static int getMaxCount() {
        if (maxVal != 0) {
            return maxVal;
        }
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        maxVal = store.getInt(QPID_MAX_COUNT);

        // fail safe
        if (maxVal == 0) {
            maxVal = 1000;
        }
        return maxVal;
    }
}
