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
package com.raytheon.viz.gfe.core.msgs;

/**
 * Enable/Disable Topo Message
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 20, 2013      #2331 randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class EnableDisableTopoMsg extends Message {
    /**
     * Topo action
     */
    public static enum Action {
        /** Enable Topo */
        ENABLE,
        /** Disable Topo */
        DISABLE
    };

    private Action action;

    private boolean forceVisibility;

    /**
     * Default constructor
     */
    public EnableDisableTopoMsg() {
        action = Action.DISABLE;
        forceVisibility = false;
    }

    /**
     * Constructor
     * 
     * @param action
     *            ENABLE or DISABLE
     * @param forceVisibility
     *            true to force visibility
     */
    public EnableDisableTopoMsg(Action action, boolean forceVisibility) {
        this.action = action;
        this.forceVisibility = forceVisibility;
    }

    /**
     * @return the action
     */
    public Action getAction() {
        return action;
    }

    /**
     * @return the forceVisibility
     */
    public boolean isForceVisibility() {
        return forceVisibility;
    }
}
