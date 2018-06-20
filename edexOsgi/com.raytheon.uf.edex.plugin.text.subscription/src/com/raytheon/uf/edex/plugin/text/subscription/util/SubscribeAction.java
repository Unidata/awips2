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
package com.raytheon.uf.edex.plugin.text.subscription.util;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.edex.plugin.text.subscription.runners.ASubscribeRunner;
import com.raytheon.uf.edex.plugin.text.subscription.runners.ISubscribeRunner;
import com.raytheon.uf.edex.plugin.text.subscription.runners.SubscribeAddRunner;
import com.raytheon.uf.edex.plugin.text.subscription.runners.SubscribeDeleteRunner;
import com.raytheon.uf.edex.plugin.text.subscription.runners.SubscribeQueryRunner;
import com.raytheon.uf.edex.plugin.text.subscription.runners.SubscribeReadRunner;
import com.raytheon.uf.edex.plugin.text.subscription.runners.SubscribeUpdateRunner;

/**
 * An enumeration of Subscription Actions. This enumeration is used by the
 * factory methods ({@link ASubscribeRunner#getInstance(String, Message)} and
 * {@link ASubscribeRunner#getInstance(String)}) to determine the correct class
 * to instantiate to create a worker.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation.
 * May 22, 2014 2536       bclement    moved from autobldsrv to edex.plugin.text
 *                                      removed hard coded class names
 * Sep 05, 2014 2926       bclement    switched map from class names to class objects
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.subscription
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public enum SubscribeAction {
    ACTION_ADD, ACTION_READ, ACTION_DELETE, ACTION_UPDATE, ACTION_QUERY;
    /**
     * A mapping of action names to SubscribeAction objects.
     */
    private static final Map<String, SubscribeAction> actions = new HashMap<String, SubscribeAction>() {
        private static final long serialVersionUID = 1L;
        {
            put("add", ACTION_ADD);
            put("read", ACTION_READ);
            put("delete", ACTION_DELETE);
            put("update", ACTION_UPDATE);
            put("query", ACTION_QUERY);
        }
    };

    /**
     * A mapping of SubscribeAction objects to implementation class
     */
    private static final Map<SubscribeAction, Class<? extends ISubscribeRunner>> runners = new HashMap<SubscribeAction, Class<? extends ISubscribeRunner>>() {
        private static final long serialVersionUID = 1L;
        {
            put(ACTION_ADD, SubscribeAddRunner.class);
            put(ACTION_READ, SubscribeReadRunner.class);
            put(ACTION_DELETE, SubscribeDeleteRunner.class);
            put(ACTION_UPDATE, SubscribeUpdateRunner.class);
            put(ACTION_QUERY, SubscribeQueryRunner.class);
        }
    };

    /**
     * Constructor.
     */
    private SubscribeAction() {
    }

    /**
     * Factory method that gets the SubscribeAction object for an action name.
     * 
     * @param action
     *            name of the requested action
     * 
     * @return the corresponding action
     */
    public static final SubscribeAction translate(String action) {
        return actions.get(action.toLowerCase());
    }

    /**
     * Returns the class name of the runner associated with the action.
     */
    public final Class<? extends ISubscribeRunner> getRunner() {
        return runners.get(this);
    }
}
