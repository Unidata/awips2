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
package com.raytheon.uf.viz.collaboration.comm.identity.roster;

import org.apache.commons.lang.StringUtils;

/**
 * Response to subscription (contacts) request. Includes user input from prompt
 * dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2014 2755       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SubscriptionResponse {

    private String group;

    private boolean accepted;

    public SubscriptionResponse() {
    }

    /**
     * @param accepted
     *            true if contact request was accepted
     * @param group
     *            optional group to add user to
     */
    public SubscriptionResponse(boolean accepted, String group) {
        this.accepted = accepted;
        this.group = group;
    }

    /**
     * @param accepted
     *            true if contact request was accepted
     */
    public SubscriptionResponse(boolean accepted) {
        this(accepted, null);
    }

    /**
     * @return true if user should be added to group
     */
    public boolean addToGroup() {
        return !StringUtils.isBlank(group);
    }

    /**
     * @return the group
     */
    public String getGroup() {
        return group;
    }

    /**
     * @return the accepted
     */
    public boolean isAccepted() {
        return accepted;
    }

    /**
     * @param group
     *            the group to set
     */
    public void setGroup(String group) {
        this.group = group;
    }

    /**
     * @param accepted
     *            the accepted to set
     */
    public void setAccepted(boolean accepted) {
        this.accepted = accepted;
    }

}
