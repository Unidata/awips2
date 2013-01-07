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
package com.raytheon.uf.viz.datadelivery.subscription;

import java.util.Date;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.viz.datadelivery.common.ui.ISortTable;
import com.raytheon.uf.viz.datadelivery.common.ui.ITableData;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionManagerDlg.FullDataset;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionManagerDlg.SubscriptionNotification;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

/**
 * Subscription Manager Table's Row Data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2012            mpduff     Initial creation.
 * Mar 13, 2012   420      jpiatt     Modified for additional row data.
 * Jun 07, 2012   687      lvenable   Table data refactor.
 * Jun 12, 2012   702      jpiatt     Added group name & code clean up.
 * Aug 10, 2012  1002      mpduff     Change dataset size from int to long.
 * Aug 21, 2012   712      mpduff     Make priorities display as 1, 2, 3.
 * Oct  2, 2012  1103      jpiatt     Remove unused methods, update enum, code clean up.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionManagerRowData implements ITableData<SubscriptionManagerRowData> {
    
    /** Dataset id. */
    private String dataSetID = null;

    /** Subscription name. */
    private String name = null;

    /** Subscription owner. */
    private String owner = null;

    /** Subscription active status. */
    private boolean active = false;

    /** Subscription priority of fulfillment. */
    private int priority = 2;

    /** Subscription description. */
    private String description = null;

    /** Subscription start date & time. */
    private Date subscriptionStart = null;

    /** Subscription end date & time. */
    private Date subscriptionEnd = null;

    /** Subscription active period start date. */
    private Date activeStart = null;

    /** Subscription active period end date. */
    private Date activeEnd = null;

    /** Subscription Notification enum ref. */
    private SubscriptionNotification deliveryNotify = null;

    /** Subscription column titles. */
    private final String[] columns = DataDeliveryUtils.getColumnTitles(TABLE_TYPE.SUBSCRIPTION);

    /** Subscription details. */
    private String details = null;

    /** Office id. */
    private String officeId = null;

    /** Size of dataset. */
    private long dataSetSize = 0;

    /** Subscription object. */
    private Subscription subscription;

    /** Subscription status. */
    private String status;

    /** FullDataset enum ref. */
    private FullDataset fullDataSet = null;
    
    /** Subscription group name. */
    private String groupName;

	/** Sort callback. */
    private ISortTable sortCallback = null;

    /**
     * Constructor
     */
    public SubscriptionManagerRowData() {
        
    }

    @Override
	public void setSortCallback(ISortTable sortCallback) {
        this.sortCallback = sortCallback;
    }

    /**
     * Get Subscription Name.
     * 
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * Set Subscription Name.
     * 
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Get the subscription owner.
     * 
     * @return the owner
     */
    public String getOwner() {
        return owner;
    }

    /**
     * Set the subscription owner.
     * 
     * @param owner
     *            the owner to set
     */
    public void setOwner(String owner) {
        this.owner = owner;
    }

    /**
     * Get the subscription active status.
     * 
     * @return the active
     */
    public boolean isActive() {
        return active;
    }

    /**
     * Set  the subscription active status.
     * 
     * @param active
     *            the active to set
     */
    public void setActive(boolean active) {
        this.active = active;
    }

    /**
     * Get the subscription priority.
     * 
     * @return the priority
     */
    public int getPriority() {
        return priority;
    }

    /**
     * Set the subscription priority.
     * 
     * @param priority
     *            the priority to set
     */
    public void setPriority(int priority) {
        this.priority = priority;
    }

    /**
     * Get the subscription description.
     * 
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Set the subscription description.
     * 
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Get the subscription start time.
     * 
     * @return the subscriptionStart
     */
    public Date getSubscriptionStart() {
        return subscriptionStart;
    }

    /**
     * Set the subscription start time.
     * 
     * @param subscriptionStart
     *            the subscriptionStart to set
     */
    public void setSubscriptionStart(Date subscriptionStart) {
        this.subscriptionStart = subscriptionStart;
    }

    /**
     * Get the subscription end time.
     * 
     * @return the subscriptionEnd
     */
    public Date getSubscriptionEnd() {
        return subscriptionEnd;
    }

    /**
     * Set the subscription end time.
     * 
     * @param subscriptionEnd
     *            the subscriptionEnd to set
     */
    public void setSubscriptionEnd(Date subscriptionEnd) {
        this.subscriptionEnd = subscriptionEnd;
    }

    /**
     * Get the subscription active start date.
     * 
     * @return the activeStart
     */
    public Date getActiveStart() {
        return activeStart;
    }

    /**
     * Set the subscription active start date.
     * 
     * @param activeStart
     *            the activeStart to set
     */
    public void setActiveStart(Date activeStart) {
        this.activeStart = activeStart;
    }

    /**
     * Get the subscription active end date.
     * 
     * @return the activeEnd
     */
    public Date getActiveEnd() {
        return activeEnd;
    }

    /**
     * Set the subscription active end date.
     * 
     * @param activeEnd
     *            the activeEnd to set
     */
    public void setActiveEnd(Date activeEnd) {
        this.activeEnd = activeEnd;
    }

    /**
     * Get the delivery action.
     * 
     * @return the deliveryNotify
     */
    public SubscriptionNotification getDeliveryNotify() {
        return deliveryNotify;
    }

    /**
     * Set the delivery action.
     * 
     * @param deliveryNotify
     *            the deliveryNotify to set
     */
    public void setDeliveryNotify(SubscriptionNotification deliveryNotify) {
        this.deliveryNotify = deliveryNotify;
    }

    /**
     * Get the subscription details.
     * 
     * @return the details
     */
    public String getDetails() {
        return details;
    }

    /**
     * Set the subscription details.
     * 
     * @param details
     *            the details to set
     */
    public void setDetails(String details) {
        this.details = details;
    }

    /**
     * Get the dataset identification.
     * 
     * @return the dataSetID
     */
    public String getDataSetID() {
        return dataSetID;
    }

    /**
     * Set the dataset identification.
     * 
     * @param dataSetID
     *            the dataSetID to set
     */
    public void setDataSetID(String dataSetID) {
        this.dataSetID = dataSetID;
    }

    /**
     * Get the office identification.
     * 
     * @return the dataSetID
     */
    public String getOfficeId() {
        return officeId;
    }

    /**
     * Set the office identification.
     * 
     * @param dataSetID
     *            the dataSetID to set
     */
    public void setOfficeId(String officeId) {
        this.officeId = officeId;
    }

    /**
     * Get the size of the dataset defined.
     * 
     * @return the dataSet size
     */
    public long getDataSetSize() {
        return dataSetSize;
    }

    /**
     * Set the dataset size.
     * 
     * @param dataSetSize
     *            the dataSet size to set
     */
    public void setDataSetSize(long dataSetSize) {
        this.dataSetSize = dataSetSize;
    }

    public FullDataset getFullDataSet() {
        return fullDataSet;
    }

    /**
     * Set full dataset.
     * 
     * @param fullDataSet
     *            the full dataSet object
     */
    public void setFullDataSet(FullDataset fullDataSet) {
        this.fullDataSet = fullDataSet;
    }

    /**
     * Get the subscription object.
     * 
     * @return the subscription obj
     */
    public Subscription getSubscription() {
        return subscription;
    }

    /**
     * Set the subscription object.
     * 
     * @param subscription obj
     *            the subscription to set
     */
    public void setSubscription(Subscription subscription) {
        this.subscription = subscription;
        populate();
    }

    /**
     * Set the subscription status. Statuses include active,
     * inactive, expired, & invalid.
     * 
     * @param status
     *            the status to set
     */
    public void setStatus(String status) {
        this.status = status;
    }

    /**
     * Get the subscription status. Statuses include active,
     * inactive, expired, & invalid.
     * 
     * @return status
     */
    public String getStatus() {
        return this.status;
    }
    
    /**
     * Get subscription group name.
     * 
     * @return group name
     */
    public String getGroupName() {
		return groupName;
	}

	/**
	 * Set subscription group name.
	 * 
	 * @param groupName
	 */
	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	/**
     * Populate the subscription.
     */
    private void populate() {
        this.setActiveStart(subscription.getActivePeriodStart());
        this.setActiveEnd(subscription.getActivePeriodEnd());
        this.setDataSetID(subscription.getDataSetName());
        this.setDescription(subscription.getDescription());
        this.setOfficeId(subscription.getOfficeID());
        this.setDataSetSize(subscription.getDataSetSize());
        this.setGroupName(subscription.getGroupName());

        if (subscription.isNotify()) {
            this.setDeliveryNotify(SubscriptionNotification.NOTIFY);
        }
        else {
            this.setDeliveryNotify(SubscriptionNotification.DELIVERY);
        }

        if (subscription.isFullDataSet()) {
            this.setFullDataSet(FullDataset.FULL);
        }
        else {
            this.setFullDataSet(FullDataset.SUBSET);
        }

        this.setName(subscription.getName());
        this.setOwner(subscription.getOwner());
        this.setPriority(subscription.getPriority() + 1);
        this.setSubscriptionStart(subscription.getSubscriptionStart());
        this.setSubscriptionEnd(subscription.getSubscriptionEnd());
        this.setActive(subscription.isActive());
        this.setStatus(subscription.getStatus());
    }

    /**
     * Get the sort value.
     * 
     * @param columnName
     *           The name of the table column.
     *           
     * @return sort value
     */
    public String getSortValue(String columnName) {
        // NAME("Name"), OWNER("Owner"), STATUS("Status"), PRIORITY("Priority"),
        // DESCRIPTION(
        // "Description"), SUBSCRIPTION_START("Subscription Start"),
        // SUBSCRIPTION_EXPIRATION(
        // "Subscription Expiration"), ACTIVE_START("Active Period Start"),
        // ACTIVE_END(
        // "Active Period End"), DELIVERY("Delivery/Notify");

        int cIndex = -1;
        if (columnName.equals(columns[++cIndex])) {
            return this.name;
        } else if (columnName.equals(columns[++cIndex])) {
            return this.owner;
        } else if (columnName.equals(columns[++cIndex])) {
            return this.getStatus().toString();
        } else if (columnName.equals(columns[++cIndex])) {
            return String.valueOf(this.priority);
        } else if (columnName.equals(columns[++cIndex])) {
            return this.description;
        } else if (columnName.equals(columns[++cIndex])) {
            return columnName;
        } else if (columnName.equals(columns[++cIndex])) {
            return columnName;
        } else if (columnName.equals(columns[++cIndex])) {
            return columnName;
        } else if (columnName.equals(columns[++cIndex])) {
            return columnName;
        } else if (columnName.equals(columns[++cIndex])) {
            return deliveryNotify.toString();
        } else if (columnName.equals(columns[++cIndex])) {
            return officeId;
        } else if (columnName.equals(columns[++cIndex])) {
            return fullDataSet.toString();
        } else if (columnName.equals(columns[++cIndex])) {
            return String.valueOf(this.dataSetSize);
        } else if (columnName.equals(columns[++cIndex])) {
            return groupName;
        } else {
            return null;
        }
    }

    /**
     * Check date values.
     */
    private int checkDate(Date d1, Date d2) {
        // handle empty date cells
        if ((d1 == null) && (d2 == null)) {
            return 0;
        }
        else if (d1 == null) {
            return 1;
        }
        else if (d2 == null) {
            return -1;
        }
        if (d1.before(d2)) {
            return 1;
        }
        else if (d1.after(d2)) {
            return -1;
        }
        else {
            return 0;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(SubscriptionManagerRowData o) {
        String columnName = sortCallback.getSortColumnText();
        SortDirection direction = sortCallback.getSortDirection();

        if (columnName == null) {
            return 1;
        }

        String sortValue = this.getSortValue(columnName);
        if (sortValue == null) {
            return 1;
        }

        int returnValue = 0;

        if (columnName.equals("Subscription Start")) {
            returnValue = checkDate(this.getSubscriptionStart(), o.getSubscriptionStart());
        }
        else if (columnName.equals("Subscription Expiration")) {
            returnValue = checkDate(this.getSubscriptionEnd(), o.getSubscriptionEnd());
        }
        else if (columnName.equals("Active Period Start")) {
            returnValue = checkDate(this.getActiveStart(), o.getActiveStart());
        }
        else if (columnName.equals("Active Period End")) {
            returnValue = checkDate(this.getActiveEnd(), o.getActiveEnd());
        } else if (columnName.equals("Data Size")) {
            returnValue = (int) (this.getDataSetSize() - o.getDataSetSize());
        }
        else {
            if (o.getSortValue(columnName) != null) {
                returnValue = sortValue.toUpperCase().compareTo(o.getSortValue(columnName).toUpperCase());
            }
        }

        if (direction == SortDirection.DESCENDING) {
            returnValue *= -1;
        }

        return returnValue;
    }
}
