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
package com.raytheon.uf.viz.datadelivery.notification;

import java.util.Date;

import com.raytheon.uf.viz.datadelivery.common.ui.ISortTable;
import com.raytheon.uf.viz.datadelivery.common.ui.ITableData;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

/**
 * Data object for a row in the Notification Dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 3, 2012            mpduff     Initial creation
 * Jun 07, 2012   687     lvenable   Table data refactor.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class NotificationRowData implements ITableData<NotificationRowData> {

    /** Column string array */
    private final String[] columns = DataDeliveryUtils.getColumnTitles(TABLE_TYPE.NOTIFICATION);

    /** Notification Identification number */
    private int id;

    /** Date */
    private Date date;

    /** Priority of notification */
    private int priority;

    /** Notification Category */
    private String category;

    /** User tied to the Notification */
    private String user;

    /** Notification Message */
    private String message;

    /**
     * Sort callback.
     */
    private ISortTable sortCallback = null;

    /**
     * Constructor.
     * 
     */
    public NotificationRowData() {
    }

    /**
     * Set the sort value call back.
     * 
     * @param sortCallback
     *            The sort call back.
     */
    public void setSortCallback(ISortTable sortCallback) {
        this.sortCallback = sortCallback;
    }

    /**
     * Get the notification identity.
     * 
     * @return the id
     */
    public int getId() {
        return id;
    }

    /**
     * Set the notification identity.
     * 
     * @param id
     *            the notification identity
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * Get the notification date.
     * 
     * @return the date
     */
    public Date getDate() {
        return date;
    }

    /**
     * Set the notification date.
     * 
     * @param date
     *            the notification date
     */
    public void setDate(Date date) {
        this.date = date;
    }

    /**
     * Get the notification priority.
     * 
     * @return the priority
     */
    public int getPriority() {
        return priority;
    }

    /**
     * Set the notification priority.
     * 
     * @param priority
     *            the notification priority
     */
    public void setPriority(int priority) {
        this.priority = priority;
    }

    /**
     * Get the notification category.
     * 
     * @return the category
     */
    public String getCategory() {
        return category;
    }

    /**
     * Set the notification category.
     * 
     * @param category
     *            the notification category
     */
    public void setCategory(String category) {
        this.category = category;
    }

    /**
     * Get the notification user name.
     * 
     * @return the user name
     */
    public String getUser() {
        return user;
    }

    /**
     * Set the notification user.
     * 
     * @param user
     *            the notification user
     */
    public void setUser(String user) {
        this.user = user;
    }

    /**
     * Get the notification message.
     * 
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * Set the notification message.
     * 
     * @param message
     *            the notification message
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * Compare the row data.
     * 
     * @param o
     *            the notification row data
     * 
     * @return int
     */
    @Override
    public int compareTo(NotificationRowData o) {

        String columnName = sortCallback.getSortColumnText();
        SortDirection direction = sortCallback.getSortDirection();

        int returnValue = 0;

        String sortValue = this.getSortValue(columnName);

        if (columnName.equals("Time")) {
            returnValue = checkDate(this.getDate(), o.getDate());
        }
        else if (columnName.equals("Priority")) {
            returnValue = 0;
            if (priority > o.getPriority()) {
                returnValue = 1;
            }
            else if (priority < o.getPriority()) {
                returnValue = -1;
            }
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

    private int checkDate(Date d1, Date d2) {
        // handle empty date cells
        if ((d1 == null) && (d2 == null)) {
            return 0;
        }
        else if (d1 == null) {
            return -1;
        }
        else if (d2 == null) {
            return 1;
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

    /**
     * Get the sort value.
     * 
     * @param columnName
     *            The table column name.
     * 
     * @return the string value
     */
    public String getSortValue(String columnName) {
        // TIME("Time"),
        // PRIORITY("Priority"),
        // CATEGORY("Category"),
        // USER("User"),
        // MESSAGE("Message")

        if (columnName.equals(columns[0])) {
            return String.valueOf(this.getDate());
        }
        else if (columnName.equals(columns[1])) {
            return String.valueOf(this.priority);
        }
        else if (columnName.equals(columns[2])) {
            return this.getCategory().toString();
        }
        else if (columnName.equals(columns[3])) {
            return this.getUser().toString();
        }
        else if (columnName.equals(columns[4])) {
            return this.getMessage().toString();
        }
        else {
            return null;
        }
    }

}
