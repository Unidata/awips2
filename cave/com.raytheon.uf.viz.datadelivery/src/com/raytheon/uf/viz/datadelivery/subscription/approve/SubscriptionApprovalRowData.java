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
package com.raytheon.uf.viz.datadelivery.subscription.approve;

import com.raytheon.uf.common.auth.user.IUser;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.viz.datadelivery.common.ui.ISortTable;
import com.raytheon.uf.viz.datadelivery.common.ui.ITableData;
import com.raytheon.uf.viz.datadelivery.subscription.approve.SubApprovalTableComp.Action;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * Subscription Approval row data object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun  7, 2012            mpduff      Initial creation.
 * Sep 17, 2012   1157     mpduff      Add null check.
 * Nov 28, 2012 1286       djohnson    Hide details of checking whether a user is a row's subscription's owner.
 * Dec 20, 2012 1413       bgonzale    Implemented compareTo.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionApprovalRowData implements ITableData<SubscriptionApprovalRowData> {
    /** The subscription object */
    private InitialPendingSubscription subscription;

    /** The subscription name */
    private String subName;

    /** The subscription owner */
    private String owner;

    /** The user id requesting the change */
    private String changeOwner;

    /** The subscription description */
    private String description;

    /** The office id of the subscription */
    private String officeId;

    /** Reason for the change to the subscription */
    private String changeReason;

    /** The action taking place */
    private String action;

    /**
     * Sort callback.
     */
    private ISortTable sortCallback = null;

    /**
     * @return the subscription
     */
    public InitialPendingSubscription getSubscription() {
        return subscription;
    }

    /**
     * @param subscription
     *            the subscription to set
     */
    public void setSubscription(InitialPendingSubscription subscription) {
        if (subscription != null) {
            this.subscription = subscription;
            populate();
        }
    }

    /**
     * @return the subName
     */
    public String getSubName() {
        return subName;
    }

    /**
     * @param subName
     *            the subName to set
     */
    public void setSubName(String subName) {
        this.subName = subName;
    }

    /**
     * @return the owner
     */
    public String getOwner() {
        return owner;
    }

    /**
     * @param owner
     *            the owner to set
     */
    public void setOwner(String owner) {
        this.owner = owner;
    }

    /**
     * @return the changeOwner
     */
    public String getChangeOwner() {
        return changeOwner;
    }

    /**
     * @param changeOwner
     *            the changeOwner to set
     */
    public void setChangeOwner(String changeOwner) {
        this.changeOwner = changeOwner;
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the officeId
     */
    public String getOfficeId() {
        return officeId;
    }

    /**
     * @param officeId
     *            the officeId to set
     */
    public void setOfficeId(String officeId) {
        this.officeId = officeId;
    }


    /**
     * @return the changeReason
     */
    public String getChangeReason() {
        return changeReason;
    }

    /**
     * @param changeReason the changeReason to set
     */
    public void setChangeReason(String changeReason) {
        this.changeReason = changeReason;
    }

    private void populate() {
        this.description = subscription.getDescription();
        this.officeId = subscription.getOfficeID();
        this.owner = subscription.getOwner();
        this.subName = subscription.getName();
        this.changeOwner = subscription.getChangeReqId();
        this.changeReason = subscription.getChangeReason();
        if (subscription.isDeleted()) {
            this.action = Action.DELETE.toString();
        } else if (subscription.getChangeReason() == null) {
            this.action = Action.CREATE.toString();
        } else {
            this.action = Action.EDIT.toString();
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(SubscriptionApprovalRowData o) {
        DataDeliveryUtils.PendingSubColumnNames column = DataDeliveryUtils.PendingSubColumnNames
                .valueOfColumnName(sortCallback.getSortColumnText());
        String otherValue = "";
        String selfValue = "";

        switch (column) {
        case ACTION:
            selfValue = getAction();
            otherValue = o.getAction();
            break;
        case CHANGE_ID:
            selfValue = getChangeOwner();
            otherValue = o.getChangeOwner();
            break;
        case DESCRIPTION:
            selfValue = getDescription();
            otherValue = o.getDescription();
            break;
        case NAME:
            selfValue = getSubName();
            otherValue = o.getSubName();
            break;
        case OFFICE:
            selfValue = getOfficeId();
            otherValue = o.getOfficeId();
            break;
        case OWNER:
            selfValue = getOwner();
            otherValue = o.getOwner();
            break;
        default:
            break;
        }
        int result = 0;
        switch (sortCallback.getSortDirection()) {
        case ASCENDING:
            result = otherValue.compareTo(selfValue);
            break;
        case DESCENDING:
            result = selfValue.compareTo(otherValue);
            break;
        default:
            result = 0;
            break;
        }
        return result;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.ITableData#setSortCallback
     * (com.raytheon.uf.viz.datadelivery.common.ui.ISortTable)
     */
    @Override
    public void setSortCallback(ISortTable sortCallback) {
        this.sortCallback = sortCallback;
    }

    /**
     * @param action the action to set
     */
    public void setAction(String action) {
        this.action = action;
    }

    /**
     * @return the action
     */
    public String getAction() {
        return action;
    }

    /**
     * Return whether the specified {@link IUser} is the owner of the row's
     * subscription.
     * 
     * @param user
     *            the user
     * @return true if the specified user is the owner
     */
    public boolean isOwner(IUser user) {
        return getSubscription().getOwner().equals(user.uniqueId().toString());
    }
}
