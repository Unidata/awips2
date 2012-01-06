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
package com.raytheon.edex.subscription.util;

import java.util.HashMap;
import java.util.Map;

/**
 * An enumeration of Subscription Actions.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public enum SubscribeAction {
    ACTION_ADD,ACTION_READ,ACTION_DELETE,ACTION_UPDATE,ACTION_QUERY;
    /**
     * A mapping of action names to SubscribeAction objects.
     */
    private static final Map<String, SubscribeAction> actions = new HashMap<String, SubscribeAction>() {
        private static final long serialVersionUID = 1L;
        {
            put("add",ACTION_ADD);
            put("read",ACTION_READ);
            put("delete",ACTION_DELETE);
            put("update",ACTION_UPDATE);
            put("query",ACTION_QUERY);
        }
    };
    /**
     * A mapping of SubscribeAction objects to implementation class names.
     */
    private static final Map<SubscribeAction, String> runners = new HashMap<SubscribeAction, String>() {
        private static final long serialVersionUID = 1L;
        {
            put(ACTION_ADD,"com.raytheon.edex.subscription.runners.SubscribeAddRunner");
            put(ACTION_READ,"com.raytheon.edex.subscription.runners.SubscribeReadRunner");
            put(ACTION_DELETE,"com.raytheon.edex.subscription.runners.SubscribeDeleteRunner");
            put(ACTION_UPDATE,"com.raytheon.edex.subscription.runners.SubscribeUpdateRunner");
            put(ACTION_QUERY,"com.raytheon.edex.subscription.runners.SubscribeQueryRunner");
        }
    };
    /**
     * Constructor.
     */
    private SubscribeAction(){}
    /**
     * Factory method that gets the SubscribeAction object for an action name.
     * 
     * @param action name of the requested action
     * 
     * @return the corresponding action
     */
    public static final SubscribeAction translate(String action) {
        return actions.get(action.toLowerCase());
    }
    /**
     * Returns the class name of the runner associated with the action.
     */
    public final String getRunner() {
        return runners.get(this);
    }
}
